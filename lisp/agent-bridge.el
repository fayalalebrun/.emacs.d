;;; agent-bridge.el --- Adapter between agent-shell and board/web -*- lexical-binding: t; -*-

;; Author: Francisco
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (agent-shell "0.1"))
;; Keywords: tools, processes

;;; Commentary:

;; Thin adapter layer between agent-shell and agent-board/agent-web.
;; Provides lookup-by-directory, status detection, permission handling,
;; and session history APIs that board/web need.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'json)
(require 'agent-shell)
(require 'agent-shell-anthropic)

;;;; Buffer lookup

(defun agent-bridge-find-buffer (dir)
  "Find a live agent-shell buffer whose CWD matches DIR."
  (let ((target (file-truename (expand-file-name dir))))
    (cl-find-if
     (lambda (buf)
       (and (buffer-live-p buf)
            (let ((d (buffer-local-value 'default-directory buf)))
              (and d (file-directory-p d) (file-equal-p d target)))))
     (agent-shell-buffers))))

;;;; Status detection

(defun agent-bridge-status (buf)
  "Return status string for agent-shell BUF.
One of: idle, busy, waiting, exited, starting, no-agent."
  (cond
   ((or (null buf) (not (buffer-live-p buf))) "no-agent")
   (t (with-current-buffer buf
        (if (not (boundp 'agent-shell--state)) "no-agent"
          (let* ((state agent-shell--state)
                 (proc (get-buffer-process buf))
                 (alive (and proc (process-live-p proc))))
            (cond
             ((not alive) "exited")
             ;; pending permissions -> waiting
             ((seq-some (lambda (tc)
                          (map-elt (cdr tc) :permission-request-id))
                        (map-elt state :tool-calls))
              "waiting")
             ((shell-maker-busy) "busy")
             (t "idle"))))))))

;;;; Session management

(defun agent-bridge-start (dir)
  "Start an agent-shell session for DIR.  Returns the buffer."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (buf (agent-shell-start
               :config (agent-shell-anthropic-make-claude-code-config))))
    (agent-bridge-clear-segments buf)
    buf))

(defun agent-bridge-ensure (dir)
  "Get or create an agent-shell buffer for DIR."
  (or (agent-bridge-find-buffer dir) (agent-bridge-start dir)))

;;;; Prompt / interrupt

(defun agent-bridge-send (prompt buf)
  "Send PROMPT to agent-shell BUF."
  (with-current-buffer buf (agent-shell-queue-request prompt)))

(defun agent-bridge-interrupt (buf)
  "Interrupt the agent in BUF."
  (with-current-buffer buf (agent-shell-interrupt t)))

;;;; Permission handling

(defvar-local agent-bridge--permission-options nil
  "Alist: tool-call-id -> list of ACP option alists.")

;;;; Segment accumulator

(defvar-local agent-bridge--segments nil
  "List of display segments accumulated from ACP notifications.
Each segment is an alist with `type' key.  Same format as
`agent-bridge-parse-session-history' output.")

(defvar-local agent-bridge--current-chunk nil
  "Cons (TYPE . TEXT) for the in-progress streaming chunk, or nil.")

(defun agent-bridge--finalize-chunk (buf)
  "Push current chunk as a segment in BUF and clear it."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when agent-bridge--current-chunk
        (push `((type . ,(car agent-bridge--current-chunk))
                (text . ,(cdr agent-bridge--current-chunk)))
              agent-bridge--segments)
        (setq agent-bridge--current-chunk nil)))))

(defun agent-bridge--accumulate-chunk (buf type text)
  "Accumulate TEXT into the current chunk of TYPE in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (if (and agent-bridge--current-chunk
               (eq (car agent-bridge--current-chunk) type))
          (setcdr agent-bridge--current-chunk
                  (concat (cdr agent-bridge--current-chunk) text))
        (agent-bridge--finalize-chunk buf)
        (setq agent-bridge--current-chunk (cons type text))))))

(defun agent-bridge--capture-segments (&rest args)
  "Advice :before `agent-shell--on-notification' to accumulate segments.
ARGS are the keyword arguments: :state STATE :notification NOTIFICATION."
  (let* ((state (plist-get args :state))
         (notification (plist-get args :notification))
         (method (map-elt notification 'method))
         (buf (map-elt state :buffer)))
    (when (and (equal method "session/update")
               buf (buffer-live-p buf))
      (let* ((update (map-elt (map-elt notification 'params) 'update))
             (session-update (map-elt update 'sessionUpdate)))
        (cond
         ((equal session-update "user_message_chunk")
          (agent-bridge--accumulate-chunk
           buf 'prompt (map-nested-elt update '(content text))))

         ((equal session-update "agent_message_chunk")
          (agent-bridge--accumulate-chunk
           buf 'text (map-nested-elt update '(content text))))

         ((equal session-update "agent_thought_chunk")
          (agent-bridge--accumulate-chunk
           buf 'thought (map-nested-elt update '(content text))))

         ((equal session-update "tool_call")
          (agent-bridge--finalize-chunk buf)
          (let ((tool-call-id (map-elt update 'toolCallId))
                (title (map-elt update 'title))
                (raw-input (map-elt update 'rawInput))
                (status (or (map-elt update 'status) "pending")))
            (with-current-buffer buf
              (push `((type . tool-use)
                      (tool-call-id . ,tool-call-id)
                      (name . ,(or title ""))
                      (status . ,status)
                      (input . ,(if raw-input
                                    (json-encode raw-input)
                                  "")))
                    agent-bridge--segments))))

         ((equal session-update "tool_call_update")
          (let ((tc-id (map-elt update 'toolCallId))
                (status (map-elt update 'status))
                (raw-input (map-elt update 'rawInput))
                (content (map-elt update 'content)))
            (with-current-buffer buf
              ;; Update matching tool-use segment: title, status, input
              (when-let ((seg (cl-find-if
                               (lambda (s)
                                 (and (eq (alist-get 'type s) 'tool-use)
                                      (equal (alist-get 'tool-call-id s) tc-id)))
                               agent-bridge--segments)))
                (when status
                  (setf (alist-get 'status seg) status))
                (when-let ((new-title (map-elt update 'title)))
                  (unless (string-empty-p new-title)
                    (setf (alist-get 'name seg) new-title)))
                (when raw-input
                  (setf (alist-get 'input seg) (json-encode raw-input))))
              ;; Emit tool-result when completed or failed
              (when (member status '("completed" "failed"))
                (agent-bridge--finalize-chunk buf)
                (let* ((content-text
                        (mapconcat
                         (lambda (item)
                           (or (map-nested-elt item '(content text)) ""))
                         content "\n\n"))
                       ;; Extract diff info (mirrors agent-shell--make-diff-info)
                       (diff-item
                        (cond
                         ;; ACP content with type "diff"
                         ((and content (equal (map-elt content 'type) "diff"))
                          content)
                         ((and content (listp content))
                          (seq-find (lambda (item)
                                      (equal (map-elt item 'type) "diff"))
                                    content))
                         ;; Copilot: rawInput has new_str/old_str
                         ((and raw-input (map-elt raw-input 'new_str))
                          `((oldText . ,(or (map-elt raw-input 'old_str) ""))
                            (newText . ,(map-elt raw-input 'new_str))
                            (path . ,(or (map-elt raw-input 'path)
                                         (map-elt raw-input 'file_path)))))))
                       (old-text (when diff-item
                                   (or (map-elt diff-item 'oldText) "")))
                       (new-text (when diff-item
                                   (map-elt diff-item 'newText)))
                       (file-path (when diff-item
                                    (map-elt diff-item 'path))))
                  (push `((type . tool-result)
                          (tool-call-id . ,tc-id)
                          (text . ,content-text)
                          ,@(when new-text
                              `((diff-old . ,old-text)
                                (diff-new . ,new-text)))
                          ,@(when file-path
                              `((diff-file . ,file-path)))
                          (error . ,(equal status "failed")))
                        agent-bridge--segments))))))

         ((equal session-update "plan")
          (agent-bridge--finalize-chunk buf)
          (let ((entries (map-elt update 'entries)))
            (with-current-buffer buf
              (push `((type . plan)
                      (entries . ,(append entries nil)))
                    agent-bridge--segments)))))))))

(advice-add 'agent-shell--on-notification :before #'agent-bridge--capture-segments)

(defun agent-bridge-segments (buf)
  "Return display segments for agent-shell BUF.
Finalizes any in-progress chunk first."
  (when (and buf (buffer-live-p buf))
    (agent-bridge--finalize-chunk buf)
    (with-current-buffer buf
      (nreverse (copy-sequence agent-bridge--segments)))))

(defun agent-bridge-clear-segments (buf)
  "Clear accumulated segments in BUF."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (setq agent-bridge--segments nil
            agent-bridge--current-chunk nil))))

(defun agent-bridge-usage (buf)
  "Return usage alist for BUF, or nil.
Reads live data from `agent-shell--state' :usage."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (when (boundp 'agent-shell--state)
        (let ((usage (map-elt agent-shell--state :usage)))
          (when (and usage
                     (> (or (map-elt usage :context-size) 0) 0))
            usage))))))

(defun agent-bridge--capture-permission-options (&rest args)
  "Advice :before `agent-shell--on-request' to capture permission options.
ARGS are the keyword arguments: :state STATE :request REQUEST."
  (let* ((request (plist-get args :request))
         (method (map-elt request 'method)))
    (when (equal method "session/request_permission")
      (let* ((params (map-elt request 'params))
             (tool-call (map-elt params 'toolCall))
             (tool-call-id (map-elt tool-call 'toolCallId))
             (options (map-elt params 'options))
             (state (plist-get args :state))
             (buf (map-elt state :buffer)))
        (when (and buf (buffer-live-p buf) tool-call-id options)
          (with-current-buffer buf
            (unless agent-bridge--permission-options
              (setq agent-bridge--permission-options nil))
            (setf (alist-get tool-call-id agent-bridge--permission-options
                             nil nil #'equal)
                  options)))))))

(advice-add 'agent-shell--on-request :before #'agent-bridge--capture-permission-options)

(defun agent-bridge-pending-permissions (buf)
  "Return list of pending permission alists for BUF.
Each alist has keys: tool-call-id, request-id, title, kind."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (when (boundp 'agent-shell--state)
        (let ((tool-calls (map-elt agent-shell--state :tool-calls))
              result)
          (map-do
           (lambda (tc-id tc-data)
             (when-let ((req-id (map-elt tc-data :permission-request-id)))
               (push `((tool-call-id . ,tc-id)
                       (request-id . ,req-id)
                       (title . ,(or (map-elt tc-data :title) ""))
                       (kind . ,(or (map-elt tc-data :kind) "")))
                     result)))
           tool-calls)
          (nreverse result))))))

(defun agent-bridge-approve (buf tool-call-id)
  "Approve permission for TOOL-CALL-ID in BUF."
  (with-current-buffer buf
    (let* ((state agent-shell--state)
           (client (map-elt state :client))
           (tc-data (map-elt (map-elt state :tool-calls) tool-call-id))
           (request-id (map-elt tc-data :permission-request-id))
           (options (alist-get tool-call-id agent-bridge--permission-options
                               nil nil #'equal))
           (option-id
            (or (cl-loop for opt in (append options nil)
                         when (equal (map-elt opt 'kind) "allow_once")
                         return (map-elt opt 'optionId))
                ;; Fallback: first option
                (map-elt (car (append options nil)) 'optionId))))
      (when (and client request-id option-id)
        (agent-shell--send-permission-response
         :client client
         :request-id request-id
         :option-id option-id
         :state state
         :tool-call-id tool-call-id
         :message-text "Allowed")))))

(defun agent-bridge-deny (buf tool-call-id)
  "Deny permission for TOOL-CALL-ID in BUF."
  (with-current-buffer buf
    (let* ((state agent-shell--state)
           (client (map-elt state :client))
           (tc-data (map-elt (map-elt state :tool-calls) tool-call-id))
           (request-id (map-elt tc-data :permission-request-id)))
      (when (and client request-id)
        (agent-shell--send-permission-response
         :client client
         :request-id request-id
         :cancelled t
         :state state
         :tool-call-id tool-call-id
         :message-text "Denied")))))

;;;; Resume support

(defvar-local agent-bridge--resume-session-id nil
  "Target ACP session ID to resume; consumed once by session-select advice.")

(defun agent-bridge--advice-prompt-select-session (orig-fn acp-sessions)
  "Intercept session selection when `agent-bridge--resume-session-id' is set.
ORIG-FN is the original `agent-shell--prompt-select-session'.
ACP-SESSIONS is the list of ACP session alists."
  (if (and (buffer-live-p (current-buffer))
           (boundp 'agent-bridge--resume-session-id)
           agent-bridge--resume-session-id)
      (let ((target agent-bridge--resume-session-id))
        (setq agent-bridge--resume-session-id nil)
        (or (cl-find-if (lambda (s)
                          (equal (map-elt s 'sessionId) target))
                        acp-sessions)
            (car acp-sessions)))
    (funcall orig-fn acp-sessions)))

(advice-add 'agent-shell--prompt-select-session :around
            #'agent-bridge--advice-prompt-select-session)

(defun agent-bridge-resume (dir session-id)
  "Start or return an agent-shell buffer for DIR, resuming SESSION-ID.
If a live buffer already exists for DIR, return it unchanged."
  (or (agent-bridge-find-buffer dir)
      (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
             (buf (agent-shell--start
                   :config (agent-shell-anthropic-make-claude-code-config)
                   :no-focus t
                   :new-session t
                   :session-strategy 'prompt)))
        (with-current-buffer buf
          (setq-local agent-bridge--resume-session-id session-id))
        (agent-bridge-clear-segments buf)
        buf)))

;;;; Buffer text extraction

(defun agent-bridge-buffer-text (buf)
  "Return plain text content of agent-shell BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Session history (Claude Code JSONL files)

(defvar agent-bridge--session-metadata-cache (make-hash-table :test 'equal)
  "Cache of (file . mtime) -> parsed session metadata alist.")

(defun agent-bridge--claude-sessions-dir (directory)
  "Return the Claude Code sessions directory for DIRECTORY."
  (let* ((abs (directory-file-name (expand-file-name directory)))
         (encoded (replace-regexp-in-string "[/._]" "-" abs)))
    (expand-file-name encoded "~/.claude/projects/")))

(defun agent-bridge--parse-session-head (file)
  "Parse the first few lines and tail of session FILE for metadata.
Returns an alist with keys: id, first-prompt, summary, git-branch, timestamp."
  (let ((file-size (file-attribute-size (file-attributes file)))
        session-id first-prompt git-branch timestamp summary)
    ;; Read the head for session ID, first prompt, branch, timestamp
    (with-temp-buffer
      (insert-file-contents file nil 0 (min 8192 file-size))
      (goto-char (point-min))
      (dotimes (_ 5)
        (unless (eobp)
          (let* ((line (buffer-substring-no-properties
                        (point) (line-end-position)))
                 (json (ignore-errors
                         (json-parse-string line :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil))))
            (when json
              (unless session-id
                (setq session-id (alist-get 'sessionId json)))
              (unless timestamp
                (setq timestamp (alist-get 'timestamp json)))
              (unless git-branch
                (setq git-branch (alist-get 'gitBranch json)))
              (when (and (not first-prompt)
                         (equal (alist-get 'type json) "user"))
                (let ((content (alist-get 'content
                                          (alist-get 'message json))))
                  (when (stringp content)
                    (setq first-prompt content))))))
          (forward-line 1))))
    ;; Read the tail for summary
    (when (> file-size 512)
      (with-temp-buffer
        (insert-file-contents file nil (- file-size 512) file-size)
        (goto-char (point-max))
        (forward-line -3)
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (point) (line-end-position)))
                 (json (ignore-errors
                         (json-parse-string line :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil))))
            (when (and json (equal (alist-get 'type json) "summary"))
              (setq summary (alist-get 'summary json))))
          (forward-line 1))))
    (unless session-id
      (setq session-id (file-name-sans-extension
                        (file-name-nondirectory file))))
    `((id . ,session-id)
      (first-prompt . ,first-prompt)
      (summary . ,summary)
      (git-branch . ,git-branch)
      (timestamp . ,timestamp))))

(defun agent-bridge--scan-sessions (directory)
  "Scan Claude Code session files for DIRECTORY.
Returns a list of alists sorted by mtime (newest first),
each with keys: id, file, mtime, size, first-prompt, summary,
git-branch, timestamp."
  (let* ((sessions-dir (agent-bridge--claude-sessions-dir directory))
         (files (and (file-directory-p sessions-dir)
                     (directory-files sessions-dir t
                                     "^[0-9a-f].*\\.jsonl\\'")))
         results)
    (dolist (file files)
      (let* ((attrs (file-attributes file))
             (mtime (file-attribute-modification-time attrs))
             (size (file-attribute-size attrs))
             (cache-key (cons file mtime))
             (cached (gethash cache-key agent-bridge--session-metadata-cache))
             (metadata (or cached
                          (let ((parsed (agent-bridge--parse-session-head file)))
                            (puthash cache-key parsed
                                     agent-bridge--session-metadata-cache)
                            parsed))))
        (push (append metadata
                      `((file . ,file)
                        (mtime . ,mtime)
                        (size . ,size)))
              results)))
    (sort results (lambda (a b)
                    (time-less-p (alist-get 'mtime b)
                                 (alist-get 'mtime a))))))

(defun agent-bridge--format-session-date (time)
  "Format TIME as a human-friendly relative date string."
  (let* ((now (current-time))
         (days (/ (float-time (time-subtract now time)) 86400.0)))
    (cond
     ((< days 1)   (format-time-string "Today, %H:%M" time))
     ((< days 2)   (format-time-string "Yesterday, %H:%M" time))
     ((< days 7)   (format-time-string "%A, %H:%M" time))
     (t             (format-time-string "%b %d, %Y" time)))))

(defun agent-bridge--extract-result-text (result)
  "Extract plain text from RESULT.
RESULT may be a string or a list of content blocks
\(each an alist with `type' and `text' keys)."
  (cond
   ((stringp result) result)
   ((listp result)
    (mapconcat
     (lambda (block)
       (when (equal (alist-get 'type block) "text")
         (or (alist-get 'text block) "")))
     result ""))
   (t nil)))

(defun agent-bridge-parse-session-history (file)
  "Parse session JSONL FILE into a list of display segments."
  (let ((segments nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (point) (line-end-position)))
               (json (condition-case nil
                         (json-parse-string line :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil)
                       (error nil))))
          (when json
            (let ((type (alist-get 'type json)))
              (cond
               ((equal type "user")
                (let ((content (alist-get 'content
                                          (alist-get 'message json))))
                  (cond
                   ((stringp content)
                    (push `((type . prompt) (text . ,content)) segments))
                   ((listp content)
                    (dolist (block content)
                      (when (equal (alist-get 'type block) "tool_result")
                        (let ((text (alist-get 'content block))
                              (is-error (alist-get 'is_error block)))
                          (when (stringp text)
                            (push `((type . tool-result)
                                    (text . ,text)
                                    (error . ,is-error))
                                  segments)))))))))

               ((equal type "assistant")
                (let ((content (alist-get 'content
                                          (alist-get 'message json))))
                  (when (listp content)
                    (dolist (block content)
                      (let ((btype (alist-get 'type block)))
                        (cond
                         ((equal btype "text")
                          (let ((text (alist-get 'text block)))
                            (when (and text (not (string-empty-p text)))
                              (push `((type . text) (text . ,text))
                                    segments))))
                         ((equal btype "tool_use")
                          (let ((name (alist-get 'name block))
                                (input (alist-get 'input block)))
                            (push `((type . tool-use)
                                    (name . ,name)
                                    (input . ,(if input
                                                   (json-encode input)
                                                 "")))
                                  segments)))))))))

               ((equal type "result")
                (let ((text (agent-bridge--extract-result-text
                             (alist-get 'result json))))
                  (when (and text (not (equal text "")))
                    (push `((type . result) (text . ,text))
                          segments))))))))
        (forward-line 1)))
    (nreverse segments)))

(defun agent-bridge-commands (buf)
  "Return list of available slash commands for BUF, or nil.
Each entry is an alist with keys: name, description."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (when (boundp 'agent-shell--state)
        (map-elt agent-shell--state :available-commands)))))

(defun agent-bridge-tool-label (name input)
  "Return a summary label for tool NAME with INPUT json string."
  (let ((parsed (condition-case nil
                    (json-parse-string input :object-type 'alist)
                  (error nil))))
    (if (not parsed) name
      (let ((file (or (alist-get 'file_path parsed)
                      (alist-get 'path parsed)))
            (cmd (alist-get 'command parsed))
            (pattern (alist-get 'pattern parsed)))
        (cond
         (file (format "%s %s" name (file-name-nondirectory file)))
         (cmd (format "%s: %s" name
                      (truncate-string-to-width cmd 60)))
         (pattern (format "%s %s" name pattern))
         (t name))))))

(provide 'agent-bridge)
;;; agent-bridge.el ends here
