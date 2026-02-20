;;; agent-board.el --- Workspace + Agent Dashboard for Emacs -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'magit-git)
(require 'magit-process)
(require 'magit-worktree)
(require 'magit-status)

(declare-function magit-worktree-branch "magit-worktree")
(declare-function magit-worktree-checkout "magit-worktree")
(declare-function magit-worktree-delete "magit-worktree")

(declare-function ai-code-claude-code "ai-code-claude-code")
(declare-function ai-code-backends-infra--session-buffer-p "ai-code-backends-infra")

(cl-defstruct agent-board-workspace
  "A worktree with optional ai-code session buffer."
  project toplevel branch worktree buffer primary-p task)

(defvar agent-board--refresh-timer nil
  "Buffer-local timer for auto-refresh.")

(defvar agent-board--pinned-repos nil
  "Buffer-local list of canonical repo roots to always show.")

(defvar agent-board--workspaces (make-hash-table :test 'equal)
  "Buffer-local map from worktree path to workspace struct.")

(defun agent-board--repo-root (&optional dir)
  "Return the canonical repo root for DIR (or `default-directory').
Resolves worktree paths to the primary worktree via git-common-dir.
Returns nil if DIR does not exist or is not in a git repo."
  (let ((default-directory (or dir default-directory)))
    (when (file-directory-p default-directory)
      (let ((common-dir (magit-git-string "rev-parse" "--git-common-dir")))
        (when common-dir
          (file-truename
           (expand-file-name (file-name-as-directory "..") common-dir)))))))

(defun agent-board--task (toplevel branch)
  "Return the git branch description for BRANCH in repo at TOPLEVEL."
  (when (and branch (not (string= branch "(detached)")))
    (let ((default-directory toplevel))
      (magit-git-string "config" (format "branch.%s.description" branch)))))

(defun agent-board--status (ws)
  "Return status string for workspace WS."
  (let ((buf (agent-board-workspace-buffer ws)))
    (cond
     ((or (null buf) (not (buffer-live-p buf)))
      "no-agent")
     ((not (get-buffer-process buf))
      "exited")
     ((with-current-buffer buf
        (and ai-code-backends-infra--last-activity-time
             (< (float-time (time-since ai-code-backends-infra--last-activity-time))
                (or ai-code-backends-infra-idle-delay 5.0))))
      "busy")
     (t "idle"))))

(defun agent-board--status-face (status)
  "Return face for STATUS string."
  (pcase status
    ("idle"   'success)
    ("busy"   'warning)
    ("exited" 'error)
    (_        'shadow)))

(defun agent-board--ai-code-buffers ()
  "Return all live ai-code session buffers."
  (cl-remove-if-not
   (lambda (buf)
     (and (buffer-live-p buf)
          (ai-code-backends-infra--session-buffer-p buf)))
   (buffer-list)))

(defun agent-board--discover-repos ()
  "Return alist of (REPO-ROOT . AI-BUFS) for all repos with ai-code sessions.
Also includes pinned repos added via `agent-board'.
REPO-ROOT is the canonical primary worktree path."
  (let ((ai-bufs (agent-board--ai-code-buffers))
        (repos (make-hash-table :test 'equal)))
    ;; Group ai-code buffers by their canonical repo root.
    (dolist (buf ai-bufs)
      (let* ((dir (buffer-local-value 'default-directory buf))
             (root (and dir (agent-board--repo-root dir))))
        (when root
          (puthash root (cons buf (gethash root repos)) repos))))
    ;; Include pinned repos even if they have no agents.
    (dolist (pinned agent-board--pinned-repos)
      (unless (gethash pinned repos)
        (puthash pinned nil repos)))
    ;; Convert to alist.
    (let (result)
      (maphash (lambda (root bufs) (push (cons root bufs) result)) repos)
      result)))

(defun agent-board--build-workspaces ()
  "Build list of workspace structs from all repos with ai-code sessions."
  (let ((repos (agent-board--discover-repos))
        result)
    (dolist (repo repos)
      (let* ((toplevel (car repo))
             (ai-bufs (cdr repo))
             (project (file-name-nondirectory (directory-file-name toplevel)))
             (worktrees (let ((default-directory toplevel))
                          (magit-list-worktrees)))
             (primary-path (file-truename (car (car worktrees)))))
        (dolist (wt worktrees)
          (let ((raw-path (car wt)))
            (when (file-directory-p raw-path)
              (let* ((path (file-truename raw-path))
                     (branch (nth 2 wt))
                     (matched-buf
                      (cl-find-if
                       (lambda (buf)
                         (and (get-buffer-process buf)
                              (let ((dir (buffer-local-value 'default-directory buf)))
                                (and dir
                                     (file-directory-p dir)
                                     (file-equal-p dir path)))))
                       ai-bufs)))
                (push (make-agent-board-workspace
                       :project project
                       :toplevel toplevel
                       :branch (or branch "(detached)")
                       :worktree path
                       :buffer matched-buf
                       :primary-p (string= path primary-path)
                       :task (agent-board--task toplevel branch))
                      result)))))))
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
  (let ((workspaces (agent-board--build-workspaces)))
    (clrhash agent-board--workspaces)
    (mapcar
     (lambda (ws)
       (let* ((status (agent-board--status ws))
              (path (agent-board-workspace-worktree ws)))
         (puthash path ws agent-board--workspaces)
         (list path
               (vector
                (propertize status 'face (agent-board--status-face status))
                (agent-board-workspace-project ws)
                (or (agent-board-workspace-task ws) "-")
                (agent-board-workspace-branch ws)
                (abbreviate-file-name path)))))
     workspaces)))


(defun agent-board--workspace-at-point ()
  "Return the `agent-board-workspace' at point."
  (let ((id (tabulated-list-get-id)))
    (and id (gethash id agent-board--workspaces))))

(defun agent-board-refresh ()
  "Refresh the agent board.
Preserves window-point in every window displaying this buffer, so
that a timer-driven refresh does not move the user's cursor."
  (interactive)
  (when (derived-mode-p 'agent-board-mode)
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      ;; Save each window's point before printing.  We must capture
      ;; window-point (not buffer point) because when the selected
      ;; window displays this buffer, buffer-point and window-point
      ;; are the same object -- any goto-char in this buffer will
      ;; move the cursor the user sees.
      (let ((saved (mapcar (lambda (w) (cons w (window-point w))) windows)))
        (setq tabulated-list-entries (agent-board--entries))
        (tabulated-list-print t t)
        ;; Restore every window's point.  For the selected window
        ;; this also restores buffer-point (set-window-point on
        ;; the selected window is equivalent to goto-char).
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
      (let ((default-directory dir))
        (ai-code-claude-code))
      (run-with-timer 0.5 nil (lambda ()
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
      (cond
       ((and buf (buffer-live-p buf) (get-buffer-process buf))
        (interrupt-process (get-buffer-process buf))
        (agent-board-refresh))
       (t
        (user-error "No running agent to interrupt"))))))

(defun agent-board-set-task ()
  "Set the task description for the branch at point.
Stored as the git branch description."
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
      (agent-board-refresh))))

(defun agent-board-create ()
  "Create a new worktree, then start an agent there.
If the branch already exists, check it out; otherwise create it.
Uses the repo of the workspace at point, or prompts for a directory."
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
    (let ((dir (expand-file-name worktree-dir))
          (board-buf (current-buffer)))
      (run-with-timer 1.5 nil
                      (lambda ()
                        (let ((default-directory dir))
                          (ai-code-claude-code))
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
         "  " (propertize "no-agent" 'face 'shadow)  "  No agent for worktree\n"))
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
(define-key agent-board-mode-map (kbd "C-c C-c") #'agent-board-interrupt)
(define-key agent-board-mode-map (kbd "G") #'agent-board-magit)
(define-key agent-board-mode-map (kbd "q") #'quit-window)
(define-key agent-board-mode-map (kbd "?") #'agent-board-help)
(define-key agent-board-mode-map (kbd "h") #'agent-board-help)

(define-derived-mode agent-board-mode tabulated-list-mode "Agent-Board"
  "Major mode for the agent board dashboard.

\\{agent-board-mode-map}"
  (setq tabulated-list-format
        [("Status"   10 t)
         ("Project"  15 t)
         ("Task"     50 t)
         ("Branch"   25 t)
         ("Worktree" 40 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (let ((board-buf (current-buffer)))
    (setq-local agent-board--refresh-timer
                (run-with-timer 2 2 (lambda ()
                                      (when (buffer-live-p board-buf)
                                        (with-current-buffer board-buf
                                          (condition-case err
                                              (agent-board-refresh)
                                            (error
                                             (message "agent-board: refresh error: %s"
                                                      (error-message-string err))))))))))
  (add-hook 'kill-buffer-hook #'agent-board--cleanup nil t))

(defun agent-board--cleanup ()
  "Clean up timer when board buffer is killed."
  (when (timerp agent-board--refresh-timer)
    (cancel-timer agent-board--refresh-timer)))

;;;###autoload
(defun agent-board ()
  "Open the global agent board.
The repo for the current directory is pinned so it always appears,
even if it has no agents running."
  (interactive)
  (let ((caller-root (agent-board--repo-root))
        (buf (get-buffer-create "*agent-board*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-board-mode)
        (agent-board-mode))
      (when caller-root
        (unless (member caller-root agent-board--pinned-repos)
          (push caller-root agent-board--pinned-repos)))
      (setq tabulated-list-entries (agent-board--entries))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'agent-board)
;;; agent-board.el ends here
