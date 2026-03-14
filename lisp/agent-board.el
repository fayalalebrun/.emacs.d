;;; agent-board.el --- Workspace + Agent Dashboard for Emacs -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'magit-git)
(require 'magit-process)
(require 'magit-worktree)
(require 'magit-status)

(declare-function magit-worktree-branch "magit-worktree")
(declare-function magit-worktree-checkout "magit-worktree")
(declare-function magit-worktree-delete "magit-worktree")

(require 'agent-bridge)

(cl-defstruct agent-board-workspace
  "A worktree with optional agent session buffer."
  project toplevel branch worktree buffer primary-p task)

(defvar agent-board--refresh-timer nil
  "Buffer-local timer for auto-refresh.")

(defvar agent-board--redraw-timer nil
  "Timer used to debounce board redraws.")

(defvar agent-board-refresh-interval 3
  "Seconds between periodic board refreshes.")

(defvar agent-board-redraw-debounce-delay 0.2
  "Seconds to wait before redrawing all visible board buffers.")

(defvar agent-board-git-cache-refresh-interval 10
  "Seconds before async git-backed cache entries are considered stale.")

(defvar agent-board-process-snapshot-refresh-interval 5
  "Seconds before process memory snapshot is refreshed.")

(defvar agent-board--pinned-repos nil
  "Buffer-local list of repo paths to always show.")

(defvar agent-board--workspaces (make-hash-table :test 'equal)
  "Buffer-local map from worktree path to workspace struct.")

(defvar agent-board--repo-root-cache (make-hash-table :test 'equal)
  "Map directory path to plist cache entry for canonical repo root.")

(defvar agent-board--repo-root-pending (make-hash-table :test 'equal)
  "Map directory path to in-flight repo-root process.")

(defvar agent-board--worktrees-cache (make-hash-table :test 'equal)
  "Map repo path to plist cache entry for worktree data.")

(defvar agent-board--worktrees-pending (make-hash-table :test 'equal)
  "Map repo path to in-flight worktree listing process.")

(defvar agent-board--task-cache (make-hash-table :test 'equal)
  "Map (REPO . BRANCH) to plist cache entry for task description.")

(defvar agent-board--task-pending (make-hash-table :test 'equal)
  "Map (REPO . BRANCH) to in-flight task lookup process.")

(defvar agent-board--process-snapshot-cache nil
  "Cached process snapshot for memory display.")

(defvar agent-board--process-snapshot-cache-at 0
  "Timestamp for `agent-board--process-snapshot-cache'.")

(defvar agent-board--process-snapshot-pending nil
  "In-flight process object for async process snapshot refresh.")

(defun agent-board--now ()
  "Return current time as a floating-point number of seconds."
  (float-time (current-time)))

(defun agent-board--cache-fresh-p (entry interval)
  "Return non-nil when ENTRY is fresh for INTERVAL seconds."
  (and entry
       (numberp interval)
       (> interval 0)
       (let ((at (plist-get entry :at)))
         (and (numberp at)
              (<= (- (agent-board--now) at) interval)))))

(defun agent-board--cache-value (table key)
  "Return cached value from TABLE for KEY."
  (plist-get (gethash key table) :value))

(defun agent-board--cache-put (table key value)
  "Store VALUE in TABLE under KEY with the current timestamp."
  (puthash key (list :value value :at (agent-board--now)) table)
  value)

(defun agent-board--visible-p (buffer)
  "Return non-nil when BUFFER is visible in a window."
  (and (buffer-live-p buffer)
       (get-buffer-window-list buffer nil t)))

(defun agent-board--schedule-redraw ()
  "Debounce redraw for all live visible board buffers."
  (when (timerp agent-board--redraw-timer)
    (cancel-timer agent-board--redraw-timer))
  (setq agent-board--redraw-timer
        (run-with-timer
         agent-board-redraw-debounce-delay nil
         (lambda ()
           (setq agent-board--redraw-timer nil)
           (dolist (buf (buffer-list))
             (when (and (buffer-live-p buf)
                        (agent-board--visible-p buf))
               (with-current-buffer buf
                 (when (derived-mode-p 'agent-board-mode)
                   (ignore-errors
                     (agent-board-refresh))))))))))

(defun agent-board--process-live-p (proc)
  "Return non-nil when PROC is a live process."
  (and proc (process-live-p proc)))

(defun agent-board--start-process (name buffer directory command sentinel)
  "Start async COMMAND in DIRECTORY using BUFFER and SENTINEL."
  (let ((default-directory directory))
    (make-process :name name
                  :buffer buffer
                  :command command
                  :coding 'utf-8-unix
                  :connection-type 'pipe
                  :noquery t
                  :sentinel sentinel
                  :file-handler t
                  :stderr buffer)))

(defun agent-board--git-output-sentinel (proc on-success on-finish)
  "Handle PROC completion, then call ON-SUCCESS and ON-FINISH.

ON-SUCCESS receives the trimmed process output when PROC exits with code 0.
ON-FINISH is always called.
"
  (when (memq (process-status proc) '(exit signal))
    (unwind-protect
        (when (and (eq (process-status proc) 'exit)
                   (zerop (process-exit-status proc)))
          (when-let* ((buf (process-buffer proc))
                      (output (with-current-buffer buf
                                (string-trim-right (buffer-string)))))
            (funcall on-success output)))
      (funcall on-finish)
      (when-let ((buf (process-buffer proc)))
        (kill-buffer buf)))))

(defun agent-board--clear-task-cache-for-repo (repo)
  "Clear all cached task descriptions for REPO."
  (let ((keys (hash-table-keys agent-board--task-cache)))
    (dolist (key keys)
      (when (equal (car-safe key) repo)
        (remhash key agent-board--task-cache)
        (when-let ((proc (gethash key agent-board--task-pending)))
          (when (process-live-p proc)
            (delete-process proc))
          (remhash key agent-board--task-pending))))))

(defun agent-board--invalidate-worktrees-cache (repo)
  "Invalidate cached worktree data for REPO."
  (remhash repo agent-board--worktrees-cache)
  (when-let ((proc (gethash repo agent-board--worktrees-pending)))
    (when (process-live-p proc)
      (delete-process proc))
    (remhash repo agent-board--worktrees-pending)))

(defun agent-board--repo-root-entry (dir)
  "Return the repo-root cache entry for DIR.

Starts an async refresh when needed.
"
  (let* ((path (and dir (file-directory-p dir) (file-truename dir)))
         (entry (and path (gethash path agent-board--repo-root-cache))))
    (when (and path
               (not (agent-board--cache-fresh-p entry agent-board-git-cache-refresh-interval))
               (not (gethash path agent-board--repo-root-pending)))
      (let ((buf (generate-new-buffer " *agent-board-repo-root*")))
        (puthash path
                 (agent-board--start-process
                  "agent-board-repo-root" buf path
                  (list "git" "-C" path "rev-parse" "--path-format=absolute" "--git-common-dir")
                  (lambda (proc _event)
                    (agent-board--git-output-sentinel
                     proc
                     (lambda (output)
                       (let ((root (and (not (string-empty-p output))
                                        (ignore-errors
                                          (file-truename
                                           (expand-file-name ".." output))))))
                         (agent-board--cache-put agent-board--repo-root-cache path root)
                         (agent-board--schedule-redraw)))
                     (lambda ()
                       (remhash path agent-board--repo-root-pending)))))
                 agent-board--repo-root-pending)))
    entry))

(defun agent-board--repo-key (dir)
  "Return the best available repo grouping key for DIR."
  (let* ((path (and dir (file-directory-p dir) (file-truename dir)))
         (entry (and path (agent-board--repo-root-entry path))))
    (or (and entry (plist-get entry :value)) path)))

(defun agent-board--parse-worktrees-porcelain (output)
  "Parse git worktree porcelain OUTPUT."
  (let ((lines (split-string output "\n"))
        (current nil)
        (worktrees nil))
    (dolist (line lines)
      (cond
       ((string-prefix-p "worktree " line)
        (when current
          (push current worktrees))
        (setq current (list (substring line 9) nil nil)))
       ((and current (string-prefix-p "HEAD " line))
        (setf (nth 1 current) (substring line 5)))
       ((and current (string-prefix-p "branch " line))
        (let ((branch (substring line 7)))
          (setf (nth 2 current)
                (string-remove-prefix "refs/heads/" branch))))
       ((and current (string-empty-p line))
        (push current worktrees)
        (setq current nil))))
    (when current
      (push current worktrees))
    (nreverse worktrees)))

(defun agent-board--ensure-worktrees (repo)
  "Refresh cached worktrees for REPO asynchronously when needed."
  (let ((entry (gethash repo agent-board--worktrees-cache)))
    (when (and repo
               (not (agent-board--cache-fresh-p entry agent-board-git-cache-refresh-interval))
               (not (gethash repo agent-board--worktrees-pending)))
      (let ((buf (generate-new-buffer " *agent-board-worktrees*")))
        (puthash repo
                 (agent-board--start-process
                  "agent-board-worktrees" buf repo
                  (list "git" "-C" repo "worktree" "list" "--porcelain")
                  (lambda (proc _event)
                    (agent-board--git-output-sentinel
                     proc
                     (lambda (output)
                       (agent-board--cache-put
                        agent-board--worktrees-cache repo
                        (agent-board--parse-worktrees-porcelain output))
                       (agent-board--schedule-redraw))
                     (lambda ()
                       (remhash repo agent-board--worktrees-pending)))))
                 agent-board--worktrees-pending))))
  (agent-board--cache-value agent-board--worktrees-cache repo))

(defun agent-board--task (repo branch)
  "Return cached task description for BRANCH in REPO, refreshing async."
  (when (and repo branch (not (string= branch "(detached)")))
    (let* ((key (cons repo branch))
           (entry (gethash key agent-board--task-cache)))
      (when (and (not (agent-board--cache-fresh-p entry agent-board-git-cache-refresh-interval))
                 (not (gethash key agent-board--task-pending)))
        (let ((buf (generate-new-buffer " *agent-board-task*")))
          (puthash key
                   (agent-board--start-process
                    "agent-board-task" buf repo
                    (list "git" "-C" repo "config" "--get"
                          (format "branch.%s.description" branch))
                    (lambda (proc _event)
                      (when (memq (process-status proc) '(exit signal))
                        (unwind-protect
                            (let ((value ""))
                              (when-let ((proc-buf (process-buffer proc)))
                                (with-current-buffer proc-buf
                                  (setq value (string-trim-right (buffer-string)))))
                              (agent-board--cache-put
                               agent-board--task-cache key value))
                          (remhash key agent-board--task-pending)
                          (when-let ((proc-buf (process-buffer proc)))
                            (kill-buffer proc-buf))
                          (agent-board--schedule-redraw)))))
                   agent-board--task-pending)))
      (let ((value (agent-board--cache-value agent-board--task-cache key)))
        (unless (string-empty-p (or value ""))
          value)))))

(defun agent-board--status (ws)
  "Return status string for workspace WS."
  (let ((buf (agent-board-workspace-buffer ws)))
    (if (and buf (buffer-live-p buf))
        (or (ignore-errors (agent-bridge-status buf)) "no-agent")
      "no-agent")))

(defun agent-board--status-face (status)
  "Return face for STATUS string."
  (pcase status
    ("idle"     'success)
    ("busy"     'warning)
    ("waiting"  'warning)
    ("exited"   'error)
    ("starting" 'font-lock-comment-face)
    (_          'shadow)))

(defun agent-board--agent-buffers ()
  "Return all live agent-shell session buffers."
  (agent-shell-buffers))

(defun agent-board--live-process (buf)
  "Return the live agent process for agent-shell BUF, or nil."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (let* ((client-proc (and (boundp 'agent-shell--state)
                               (map-elt (map-elt agent-shell--state :client)
                                        :process)))
             (proc (or client-proc (get-buffer-process buf))))
        (and proc (process-live-p proc) proc)))))

(defun agent-board--parse-process-snapshot (output)
  "Parse `ps' OUTPUT into a snapshot plist."
  (let ((attrs (make-hash-table :test 'eql))
        (children (make-hash-table :test 'eql)))
    (dolist (line (split-string output "\n" t))
      (let* ((parts (split-string line "[[:space:]]+" t))
             (pid (and (nth 0 parts) (string-to-number (nth 0 parts))))
             (ppid (and (nth 1 parts) (string-to-number (nth 1 parts))))
             (rss (and (nth 2 parts) (string-to-number (nth 2 parts)))))
        (when (> pid 0)
          (puthash pid (list (cons 'rss rss) (cons 'ppid ppid)) attrs)
          (when (> ppid 0)
            (puthash ppid (cons pid (gethash ppid children)) children)))))
    (list :attrs attrs :children children)))

(defun agent-board--ensure-process-snapshot ()
  "Refresh process snapshot asynchronously when it is stale."
  (when (and (or (null agent-board--process-snapshot-cache)
                 (> (- (agent-board--now) agent-board--process-snapshot-cache-at)
                    agent-board-process-snapshot-refresh-interval))
             (not (agent-board--process-live-p agent-board--process-snapshot-pending)))
    (let ((buf (generate-new-buffer " *agent-board-ps*")))
      (setq agent-board--process-snapshot-pending
            (agent-board--start-process
             "agent-board-ps" buf default-directory
             (list "ps" "-eo" "pid=,ppid=,rss=")
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (when (and (eq (process-status proc) 'exit)
                                (zerop (process-exit-status proc))
                                (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (setq agent-board--process-snapshot-cache
                               (agent-board--parse-process-snapshot (buffer-string)))
                         (setq agent-board--process-snapshot-cache-at (agent-board--now))
                         (agent-board--schedule-redraw)))
                   (setq agent-board--process-snapshot-pending nil)
                   (when-let ((proc-buf (process-buffer proc)))
                     (kill-buffer proc-buf)))))))))
  agent-board--process-snapshot-cache)

(defun agent-board--process-subtree-rss-kib (pid snapshot &optional seen)
  "Return cumulative RSS in KiB for PID and its descendants.
SNAPSHOT should come from `agent-board--ensure-process-snapshot'."
  (let ((seen (or seen (make-hash-table :test 'eql))))
    (if (or (null pid) (gethash pid seen))
        0
      (progn
        (puthash pid t seen)
        (+ (or (alist-get 'rss (gethash pid (plist-get snapshot :attrs))) 0)
           (cl-loop for child in (gethash pid (plist-get snapshot :children))
                    sum (agent-board--process-subtree-rss-kib child snapshot seen)))))))

(defun agent-board--process-rss-kib (proc snapshot)
  "Return PROC cumulative resident memory usage in KiB, or nil."
  (let ((pid (and proc (process-id proc))))
    (when (and pid snapshot)
      (agent-board--process-subtree-rss-kib pid snapshot))))

(defun agent-board--format-rss-kib (rss-kib)
  "Format RSS-KIB as a human-readable memory string."
  (cond
   ((null rss-kib) "-")
   ((>= rss-kib (* 1024 1024))
    (format "%.1f GiB" (/ rss-kib 1048576.0)))
   ((>= rss-kib 1024)
    (format "%.1f MiB" (/ rss-kib 1024.0)))
   (t
    (format "%d KiB" rss-kib))))

(defun agent-board--format-age (time)
  "Format elapsed TIME relative to now, or return `-'."
  (if (null time)
      "-"
    (let ((seconds (max 0 (truncate (float-time (time-subtract (current-time)
                                                               time))))))
      (cond
       ((< seconds 60)
        (format "%ds" seconds))
       ((< seconds 3600)
        (format "%dm" (/ seconds 60)))
       ((< seconds 86400)
        (format "%dh" (/ seconds 3600)))
       (t
        (format "%dd" (/ seconds 86400)))))))

(defun agent-board--format-tokens-used (buf)
  "Return token usage for BUF as a printable string."
  (if (not (and buf (buffer-live-p buf)))
      "-"
    (let ((usage (ignore-errors (agent-bridge-usage buf))))
      (if (not usage)
          "-"
        (let ((tokens (or (map-elt usage :context-used) 0)))
          (if (numberp tokens)
              (format "%.1fk" (/ tokens 1000.0))
            (format "%s" tokens)))))))

(defun agent-board--discover-repos ()
  "Return alist of (REPO-KEY . AI-BUFS) for repos with agent sessions."
  (let ((repos (make-hash-table :test 'equal)))
    (dolist (buf (agent-board--agent-buffers))
      (let* ((dir (buffer-local-value 'default-directory buf))
             (key (agent-board--repo-key dir)))
        (when key
          (puthash key (cons buf (gethash key repos)) repos))))
    (dolist (pinned agent-board--pinned-repos)
      (when-let ((key (agent-board--repo-key pinned)))
        (unless (gethash key repos)
          (puthash key nil repos))))
    (let (result)
      (maphash (lambda (key bufs) (push (cons key bufs) result)) repos)
      result)))

(defun agent-board--fallback-workspaces (repo ai-bufs)
  "Build temporary workspaces for REPO from AI-BUFS while git data loads."
  (let ((seen (make-hash-table :test 'equal))
        result)
    (dolist (buf ai-bufs)
      (let ((dir (and (buffer-live-p buf)
                      (buffer-local-value 'default-directory buf))))
        (when (and dir (file-directory-p dir))
          (let ((path (file-truename dir)))
            (unless (gethash path seen)
              (puthash path t seen)
              (push (make-agent-board-workspace
                     :project (file-name-nondirectory (directory-file-name repo))
                     :toplevel repo
                     :branch "(loading)"
                     :worktree path
                     :buffer buf
                     :primary-p (string= path repo)
                     :task nil)
                    result))))))
    (unless result
      (push (make-agent-board-workspace
             :project (file-name-nondirectory (directory-file-name repo))
             :toplevel repo
             :branch "(loading)"
             :worktree repo
             :buffer nil
             :primary-p t
             :task nil)
            result))
    (nreverse result)))

(defun agent-board--build-workspaces ()
  "Build list of workspace structs from cached repo data."
  (let ((repos (agent-board--discover-repos))
        result)
    (dolist (repo repos)
      (let* ((toplevel (car repo))
             (ai-bufs (cdr repo))
             (project (file-name-nondirectory (directory-file-name toplevel)))
             (worktrees (agent-board--ensure-worktrees toplevel))
             (primary-path (and (consp worktrees)
                                (file-truename (car (car worktrees))))))
        (if (consp worktrees)
            (dolist (wt worktrees)
              (let ((raw-path (car wt)))
                (when (file-directory-p raw-path)
                  (let* ((path (file-truename raw-path))
                         (branch (or (nth 2 wt) "(detached)"))
                         (matched-buf
                          (cl-find-if
                           (lambda (buf)
                             (let ((dir (and (buffer-live-p buf)
                                             (buffer-local-value 'default-directory buf))))
                               (and dir
                                    (file-directory-p dir)
                                    (file-equal-p dir path))))
                           ai-bufs)))
                    (push (make-agent-board-workspace
                           :project project
                           :toplevel toplevel
                           :branch branch
                           :worktree path
                           :buffer matched-buf
                           :primary-p (and primary-path (string= path primary-path))
                           :task (agent-board--task toplevel branch))
                          result)))))
          (setq result (nconc (nreverse (agent-board--fallback-workspaces toplevel ai-bufs))
                              result)))))
    (sort (nreverse result)
          (lambda (a b)
            (let ((pa (agent-board-workspace-project a))
                  (pb (agent-board-workspace-project b)))
              (if (string= pa pb)
                  (string< (agent-board-workspace-worktree a)
                           (agent-board-workspace-worktree b))
                (string< pa pb)))))))

(defun agent-board--entries ()
  "Return `tabulated-list-entries' for the board."
  (let ((workspaces (agent-board--build-workspaces))
        (snapshot (agent-board--ensure-process-snapshot)))
    (clrhash agent-board--workspaces)
    (mapcar
     (lambda (ws)
       (let* ((status (agent-board--status ws))
              (path (agent-board-workspace-worktree ws))
              (buf (agent-board-workspace-buffer ws))
              (proc (agent-board--live-process buf))
              (pid (and proc (process-id proc)))
              (last-activity (and buf (ignore-errors (agent-bridge-last-activity-time buf))))
              (memory (agent-board--format-rss-kib
                       (agent-board--process-rss-kib proc snapshot))))
         (puthash path ws agent-board--workspaces)
         (list path
               (vector
                (propertize status 'face (agent-board--status-face status))
                (agent-board-workspace-project ws)
                (or (agent-board-workspace-task ws) "-")
                (agent-board-workspace-branch ws)
                (if pid (number-to-string pid) "-")
                (agent-board--format-age last-activity)
                memory
                (agent-board--format-tokens-used buf)
                (abbreviate-file-name path)))))
     workspaces)))

(defun agent-board--workspace-at-point ()
  "Return the `agent-board-workspace' at point."
  (let ((id (tabulated-list-get-id)))
    (and id (gethash id agent-board--workspaces))))

(defun agent-board-refresh ()
  "Refresh the agent board using cached data only."
  (interactive)
  (when (derived-mode-p 'agent-board-mode)
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (let ((saved (mapcar (lambda (w) (cons w (window-point w))) windows)))
        (setq tabulated-list-entries (agent-board--entries))
        (tabulated-list-print t t)
        (dolist (wp saved)
          (when (window-live-p (car wp))
            (set-window-point (car wp) (cdr wp))))))))

(defun agent-board-jump ()
  "Jump to the agent buffer at point."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (let ((buf (agent-board-workspace-buffer ws)))
      (cond
       ((and buf (buffer-live-p buf))
        (pop-to-buffer buf))
       (t
        (user-error "No agent buffer for this workspace"))))))

(defun agent-board-restart ()
  "Start (or restart) an agent in the workspace at point."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (let ((dir (file-truename (agent-board-workspace-worktree ws)))
          (board-buf (current-buffer)))
      (agent-bridge-start dir)
      (run-with-timer 0.5 nil
                      (lambda ()
                        (when (buffer-live-p board-buf)
                          (with-current-buffer board-buf
                            (agent-board-refresh))))))))

(defun agent-board-kill ()
  "Kill the agent in the workspace at point (keep worktree)."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (let ((buf (agent-board-workspace-buffer ws)))
      (cond
       ((and buf (buffer-live-p buf))
        (kill-buffer buf)
        (agent-board-refresh))
       (t
        (user-error "No agent to kill"))))))

(defun agent-board-interrupt ()
  "Interrupt the running agent in the workspace at point."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (let ((buf (agent-board-workspace-buffer ws)))
      (if (and buf (buffer-live-p buf))
          (progn (agent-bridge-interrupt buf) (agent-board-refresh))
        (user-error "No running agent to interrupt")))))

(defun agent-board-set-task ()
  "Set the task description for the branch at point."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (let* ((branch (agent-board-workspace-branch ws))
           (toplevel (agent-board-workspace-toplevel ws))
           (current (agent-board-workspace-task ws))
           (task (read-string (format "Task for %s: " branch) current)))
      (when (string= branch "(detached)")
        (user-error "Cannot set task on a detached HEAD"))
      (let ((default-directory toplevel)
            (key (format "branch.%s.description" branch)))
        (if (string-empty-p task)
            (magit-call-git "config" "--unset" key)
          (magit-call-git "config" key task)))
      (agent-board--cache-put agent-board--task-cache (cons toplevel branch) task)
      (agent-board-refresh))))

(defun agent-board-set-default-agent ()
  "Set default `agent-shell' backend for future shells."
  (interactive)
  (let* ((choices (cons (cons "Prompt each time (default)" nil)
                        (mapcar
                         (lambda (config)
                           (let* ((identifier (map-elt config :identifier))
                                  (buffer-name (map-elt config :buffer-name))
                                  (mode-line-name (map-elt config :mode-line-name))
                                  (name (or mode-line-name buffer-name "Unknown agent"))
                                  (id (if identifier
                                          (symbol-name identifier)
                                        "custom")))
                             (cons (format "%s (%s)" name id) identifier)))
                         agent-shell-agent-configs)))
         (selection (completing-read "Default agent: "
                                     (mapcar #'car choices)
                                     nil t))
         (value (cdr (assoc selection choices))))
    (setq agent-shell-preferred-agent-config value)
    (message "Default agent set to: %s" (or (and value (symbol-name value))
                                            "prompt each time"))))

(defun agent-board-create ()
  "Create a new worktree, then start an agent there."
  (interactive)
  (let* ((ws (agent-board--workspace-at-point))
         (toplevel (if ws
                       (agent-board-workspace-toplevel ws)
                     (let ((dir (read-directory-name "Repository: ")))
                       (let ((default-directory dir))
                         (or (magit-toplevel)
                             (user-error "Not a git repository: %s" dir))))))
         (default-directory toplevel)
         (branch (magit-read-branch-or-commit "Branch"))
         (existing (magit-local-branch-p branch))
         (start-point (unless existing
                        (magit-read-branch-or-commit "Start point")))
         (worktree-dir (funcall magit-read-worktree-directory-function
                                "Worktree directory: " branch))
         (task (read-string (format "Task for %s: " branch))))
    (if existing
        (magit-worktree-checkout worktree-dir branch)
      (magit-worktree-branch worktree-dir branch start-point))
    (when (and (not (string-empty-p task))
               (not (string= branch "(detached)")))
      (magit-call-git "config" (format "branch.%s.description" branch) task))
    (agent-board--invalidate-worktrees-cache toplevel)
    (agent-board--clear-task-cache-for-repo toplevel)
    (let ((dir (expand-file-name worktree-dir))
          (board-buf (current-buffer)))
      (run-with-timer 1.5 nil
                      (lambda ()
                        (agent-bridge-start dir)
                        (when (buffer-live-p board-buf)
                          (with-current-buffer board-buf
                            (agent-board-refresh))))))))

(defun agent-board-delete ()
  "Kill agent, remove worktree, and delete branch at point."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (when (agent-board-workspace-primary-p ws)
      (user-error "Cannot delete the primary worktree"))
    (let ((path (agent-board-workspace-worktree ws))
          (toplevel (agent-board-workspace-toplevel ws))
          (branch (agent-board-workspace-branch ws))
          (buf (agent-board-workspace-buffer ws)))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf))
      (let ((default-directory toplevel))
        (magit-worktree-delete path))
      (when (and branch (magit-local-branch-p branch))
        (let ((default-directory toplevel))
          (magit-call-git "branch" "-D" branch)))
      (agent-board--invalidate-worktrees-cache toplevel)
      (agent-board--clear-task-cache-for-repo toplevel)
      (agent-board-refresh))))

(defun agent-board-magit ()
  "Open magit-status for the worktree at point."
  (interactive)
  (let ((ws (agent-board--workspace-at-point)))
    (unless ws (user-error "No workspace at point"))
    (magit-status-setup-buffer (agent-board-workspace-worktree ws))))

(defun agent-board-help ()
  "Show help for agent board keybindings."
  (interactive)
  (let ((buf (get-buffer-create "*agent-board-help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize "Agent Board" 'face 'bold)
         " - Global Workspace + Agent Dashboard\n\n"
         (propertize "Keybindings" 'face 'bold) "\n"
         "  RET      Jump to agent buffer at point\n"
         "  g        Refresh\n"
         "  a        Set default agent for future shells\n"
         "  t        Set task (git branch description)\n"
         "  k        Kill agent (keep worktree)\n"
         "  c        Create workspace (new worktree + agent)\n"
         "  r        Restart agent in workspace\n"
         "  d        Delete workspace (kill agent + remove worktree)\n"
         "  C-c C-c  Interrupt running agent\n"
         "  G        Open magit-status for worktree\n"
         "  h/?      This help\n"
         "  q        Quit\n\n"
         (propertize "Status" 'face 'bold) "\n"
         "  " (propertize "idle"     'face 'success) "      Waiting for input\n"
         "  " (propertize "busy"     'face 'warning) "      Agent is working\n"
         "  " (propertize "exited"   'face 'error)   "    Process has ended\n"
         "  " (propertize "waiting"  'face 'warning) "   Permission requested\n"
         "  " (propertize "no-agent" 'face 'shadow)  "  No agent for worktree\n"
         "\nGit and memory data refresh asynchronously from cache.\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

(defun agent-board--hook-refresh (&rest _)
  "Refresh all agent-board buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'agent-board-mode)
          (agent-board-refresh))))))

(defvar agent-board-mode-map (make-sparse-keymap)
  "Keymap for `agent-board-mode'.")

(define-key agent-board-mode-map (kbd "RET") #'agent-board-jump)
(define-key agent-board-mode-map (kbd "g") #'agent-board-refresh)
(define-key agent-board-mode-map (kbd "k") #'agent-board-kill)
(define-key agent-board-mode-map (kbd "c") #'agent-board-create)
(define-key agent-board-mode-map (kbd "r") #'agent-board-restart)
(define-key agent-board-mode-map (kbd "d") #'agent-board-delete)
(define-key agent-board-mode-map (kbd "t") #'agent-board-set-task)
(define-key agent-board-mode-map (kbd "a") #'agent-board-set-default-agent)
(define-key agent-board-mode-map (kbd "C-c C-c") #'agent-board-interrupt)
(define-key agent-board-mode-map (kbd "G") #'agent-board-magit)
(define-key agent-board-mode-map (kbd "q") #'quit-window)
(define-key agent-board-mode-map (kbd "?") #'agent-board-help)
(define-key agent-board-mode-map (kbd "h") #'agent-board-help)

(defun agent-board--refresh-timer-callback (board-buf)
  "Refresh BOARD-BUF if it is alive and visible."
  (when (and (buffer-live-p board-buf)
             (agent-board--visible-p board-buf))
    (with-current-buffer board-buf
      (condition-case err
          (agent-board-refresh)
        (error
         (message "agent-board: refresh error: %s"
                  (error-message-string err)))))))

(defun agent-board--start-refresh-timer (&optional board-buf)
  "Start or restart the refresh timer for BOARD-BUF."
  (let ((board-buf (or board-buf (current-buffer))))
    (with-current-buffer board-buf
      (when (timerp agent-board--refresh-timer)
        (cancel-timer agent-board--refresh-timer))
      (setq-local agent-board--refresh-timer
                  (run-with-timer
                   agent-board-refresh-interval
                   agent-board-refresh-interval
                   (lambda ()
                     (agent-board--refresh-timer-callback board-buf)))))))

(define-derived-mode agent-board-mode tabulated-list-mode "Agent-Board"
  "Major mode for the agent board dashboard.

\\{agent-board-mode-map}"
  (setq tabulated-list-format
        [("Status"   10 t)
         ("Project"  15 t)
         ("Task"     40 t)
         ("Branch"   20 t)
         ("PID"       8 t)
         ("Last"      7 t)
         ("Memory"   10 t)
         ("Tokens"   10 t)
         ("Worktree" 27 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (agent-board--start-refresh-timer (current-buffer))
  (add-hook 'kill-buffer-hook #'agent-board--cleanup nil t))

(defun agent-board--cleanup ()
  "Clean up timers when the board buffer is killed."
  (when (timerp agent-board--refresh-timer)
    (cancel-timer agent-board--refresh-timer)))

;;;###autoload
(defun agent-board ()
  "Open the global agent board.
The repo for the current directory is pinned so it always appears,
even if it has no agents running."
  (interactive)
  (let ((caller-dir default-directory)
        (buf (get-buffer-create "*agent-board*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-board-mode)
        (agent-board-mode))
      (when caller-dir
        (push (file-truename caller-dir) agent-board--pinned-repos)
        (setq agent-board--pinned-repos (delete-dups agent-board--pinned-repos)))
      (setq tabulated-list-entries (agent-board--entries))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'agent-board)
;;; agent-board.el ends here
