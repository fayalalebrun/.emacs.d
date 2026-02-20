;;; agent-pipe.el --- Headless coding agent interface -*- lexical-binding: t; -*-

;; Author: Francisco
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;;; Commentary:

;; A structured, non-terminal interface for headless coding agents.
;; Uses Claude Code's JSON streaming protocol (`claude -p --output-format
;; stream-json`) to provide programmatic interaction with coding agents.
;;
;; Unlike terminal-based wrappers (eat/vterm), this package parses the
;; agent's output as structured events, enabling session tracking,
;; programmatic control, and integration with other packages.

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; Customization

(defgroup agent-pipe nil
  "Headless coding agent interface."
  :group 'tools
  :prefix "agent-pipe-")

(defcustom agent-pipe-default-backend 'claude-code
  "Default backend to use for new sessions."
  :type 'symbol
  :group 'agent-pipe)

(defcustom agent-pipe-claude-program "claude"
  "Program name or path for the Claude CLI."
  :type 'string
  :group 'agent-pipe)

(defcustom agent-pipe-claude-default-model nil
  "Default model for Claude Code sessions.
nil means use the CLI default."
  :type '(choice (const nil) string)
  :group 'agent-pipe)

(defcustom agent-pipe-claude-allowed-tools
  '("Bash" "Read" "Edit" "Grep" "Glob" "Write")
  "List of tools to allow in Claude Code sessions."
  :type '(repeat string)
  :group 'agent-pipe)

(defcustom agent-pipe-claude-max-turns nil
  "Maximum number of agent turns.
nil means no limit."
  :type '(choice (const nil) integer)
  :group 'agent-pipe)

(defcustom agent-pipe-debug nil
  "When non-nil, log raw process I/O to a debug buffer."
  :type 'boolean
  :group 'agent-pipe)

;;;; Data Structures

(cl-defstruct agent-pipe-backend
  "Definition of a CLI backend for a coding agent."
  name              ; symbol -- e.g. 'claude-code
  build-cmd-fn      ; (fn prompt session) -> (program . args)
  build-resume-fn   ; (fn prompt session-id session) -> (program . args)
  parse-event-fn    ; (fn json-alist) -> agent-pipe-event or nil
  default-program   ; string -- e.g. "claude"
  default-args)     ; list of strings

(cl-defstruct agent-pipe-event
  "A normalized event emitted by a coding agent backend."
  type        ; symbol: text, tool-start, tool-input, tool-stop,
              ;         message-start, message-stop, result, error
  text        ; string or nil
  tool-name   ; string or nil
  session-id  ; string or nil
  raw)        ; alist -- original JSON

(cl-defstruct agent-pipe-session
  "A coding agent session."
  id             ; string or nil -- CLI session ID once known
  backend        ; agent-pipe-backend
  directory      ; working directory
  buffer         ; output buffer
  process        ; subprocess
  status         ; symbol: running, completed, error
  model          ; string or nil
  allowed-tools  ; list of strings or nil
  slash-commands ; list of strings -- from system init event
  skills         ; list of strings -- from system init event
  history)       ; list of prompt strings

;;;; Hooks

(defvar agent-pipe-event-hook nil
  "Hook run on every event.  Called with (SESSION EVENT).")

(defvar agent-pipe-text-hook nil
  "Hook run on text events.  Called with (SESSION TEXT-STRING).")

(defvar agent-pipe-tool-hook nil
  "Hook run on tool-start events.  Called with (SESSION TOOL-NAME).")

(defvar agent-pipe-finished-hook nil
  "Hook run when a session finishes.  Called with (SESSION).")

;;;; Internal State

(defvar agent-pipe--sessions (make-hash-table :test 'equal)
  "Hash table mapping (directory . backend-name) -> session.")

(defvar agent-pipe--session-id-history (make-hash-table :test 'equal)
  "Hash table mapping directory -> list of session-id strings.")

(defvar agent-pipe--backends (make-hash-table :test 'eq)
  "Hash table mapping backend-name -> agent-pipe-backend.")

;;;; Claude Code Backend

(defun agent-pipe--claude-build-cmd (_prompt session)
  "Build a command list to start a new Claude Code session.
SESSION is the `agent-pipe-session'.
The prompt is sent via stdin by the caller."
  (let ((program agent-pipe-claude-program)
        (args (list "-p" "--output-format" "stream-json" "--verbose"))
        (model (agent-pipe-session-model session))
        (tools (agent-pipe-session-allowed-tools session)))
    (when model
      (setq args (append args (list "--model" model))))
    (when tools
      (setq args (append args (list "--allowedTools"
                                    (mapconcat #'identity tools ",")))))
    (when agent-pipe-claude-max-turns
      (setq args (append args (list "--max-turns"
                                    (number-to-string
                                     agent-pipe-claude-max-turns)))))
    (cons program args)))

(defun agent-pipe--claude-build-resume-cmd (_prompt session-id session)
  "Build a command list to resume a Claude Code session.
SESSION-ID is the session to resume.
SESSION is the `agent-pipe-session'.
The prompt is sent via stdin by the caller."
  (let ((program agent-pipe-claude-program)
        (args (list "-p" "--output-format" "stream-json" "--verbose"
                    "--resume" session-id))
        (model (agent-pipe-session-model session))
        (tools (agent-pipe-session-allowed-tools session)))
    (when model
      (setq args (append args (list "--model" model))))
    (when tools
      (setq args (append args (list "--allowedTools"
                                    (mapconcat #'identity tools ",")))))
    (cons program args)))

(defun agent-pipe--claude-parse-event (json)
  "Parse a JSON alist from Claude Code into an `agent-pipe-event'.
Returns nil or a list of events for multi-event messages."
  (let ((type (alist-get 'type json)))
    (cond
     ;; Unwrap stream_event envelope and re-parse the inner event
     ((equal type "stream_event")
      (let ((inner (alist-get 'event json)))
        (when inner
          (agent-pipe--claude-parse-event inner))))

     ((equal type "system")
      (let ((sid (alist-get 'session_id json)))
        (when sid
          (make-agent-pipe-event :type 'system :session-id sid :raw json))))

     ((equal type "result")
      (make-agent-pipe-event
       :type 'result
       :session-id (alist-get 'session_id json)
       :text (alist-get 'result json)
       :raw json))

     ((equal type "error")
      (make-agent-pipe-event
       :type 'error
       :text (or (alist-get 'error json)
                 (json-encode json))
       :raw json))

     ;; Turn-level assistant message -- extract tool_use blocks.
     ;; Text is already streamed incrementally, so skip it here.
     ((equal type "assistant")
      (let* ((message (alist-get 'message json))
             (content (and message (alist-get 'content message))))
        (when content
          (let ((events nil))
            (dolist (block content)
              (when (equal (alist-get 'type block) "tool_use")
                (push (make-agent-pipe-event
                       :type 'tool-start
                       :tool-name (alist-get 'name block)
                       :text (let ((input (alist-get 'input block)))
                               (when input (json-encode input)))
                       :raw json)
                      events)))
            (nreverse events)))))

     ;; Turn-level user message -- carries tool results
     ((equal type "user")
      (let* ((message (alist-get 'message json))
             (content (and message (alist-get 'content message))))
        (when content
          (let ((events nil))
            (dolist (block content)
              (when (equal (alist-get 'type block) "tool_result")
                (let ((result-content (alist-get 'content block))
                      (is-error (alist-get 'is_error block)))
                  (push (make-agent-pipe-event
                         :type (if is-error 'tool-error 'tool-result)
                         :text (if (stringp result-content)
                                   result-content
                                 (json-encode result-content))
                         :raw json)
                        events))))
            (nreverse events)))))

     ((equal type "content_block_start")
      (let ((content-block (alist-get 'content_block json)))
        (when content-block
          (let ((cb-type (alist-get 'type content-block)))
            (cond
             ((equal cb-type "tool_use")
              (make-agent-pipe-event
               :type 'tool-start
               :tool-name (alist-get 'name content-block)
               :raw json))
             ((equal cb-type "text")
              (let ((initial (alist-get 'text content-block)))
                (when (and initial (not (equal initial "")))
                  (make-agent-pipe-event
                   :type 'text :text initial :raw json))))
             (t nil))))))

     ((equal type "content_block_delta")
      (let ((delta (alist-get 'delta json)))
        (when delta
          (let ((delta-type (alist-get 'type delta)))
            (cond
             ((equal delta-type "text_delta")
              (make-agent-pipe-event
               :type 'text
               :text (alist-get 'text delta)
               :raw json))
             ((equal delta-type "input_json_delta")
              (make-agent-pipe-event
               :type 'tool-input
               :text (alist-get 'partial_json delta)
               :raw json))
             (t nil))))))

     ((equal type "content_block_stop")
      (make-agent-pipe-event :type 'tool-stop :raw json))

     ((equal type "message_start")
      (make-agent-pipe-event :type 'message-start :raw json))

     ((equal type "message_stop")
      (make-agent-pipe-event :type 'message-stop :raw json))

     ((equal type "message_delta")
      nil)

     (t
      (agent-pipe--debug-log "parse" "unhandled event type: %s" type)
      nil))))

(defvar agent-pipe-claude-code-backend
  (make-agent-pipe-backend
   :name 'claude-code
   :build-cmd-fn #'agent-pipe--claude-build-cmd
   :build-resume-fn #'agent-pipe--claude-build-resume-cmd
   :parse-event-fn #'agent-pipe--claude-parse-event
   :default-program "claude"
   :default-args '("-p" "--output-format" "stream-json" "--verbose"))
  "The Claude Code backend instance.")

;; Register the backend
(puthash 'claude-code agent-pipe-claude-code-backend agent-pipe--backends)

;;;; Debug Logging

(defun agent-pipe--debug-buffer ()
  "Return the debug log buffer, creating it if needed."
  (get-buffer-create "*agent-pipe-debug*"))

(defun agent-pipe--debug-log (source format-string &rest args)
  "Log a message to the debug buffer when `agent-pipe-debug' is non-nil.
SOURCE is a label like \"stdout\", \"stderr\", or \"cmd\".
FORMAT-STRING and ARGS are as in `format'."
  (when agent-pipe-debug
    (let ((buf (agent-pipe--debug-buffer))
          (msg (apply #'format format-string args))
          (ts (format-time-string "%H:%M:%S.%3N")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "[%s] [%s] %s\n" ts source msg))))))

;;;; NDJSON Process Filter

(defun agent-pipe--process-filter (proc output)
  "Process filter for NDJSON output from coding agent PROC.
OUTPUT is the string chunk received."
  (agent-pipe--debug-log "stdout" "%s" output)
  (let ((partial (process-get proc 'agent-pipe--partial)))
    (setq partial (concat (or partial "") output))
    (let ((start 0)
          (session (process-get proc 'agent-pipe--session)))
      (while (string-match "\n" partial start)
        (let* ((end (match-end 0))
               (line (substring partial start (1- end))))
          (setq start end)
          (when (and (> (length line) 0)
                     session)
            (condition-case err
                (let* ((json (json-parse-string line :object-type 'alist
                                                :array-type 'list
                                                :null-object nil
                                                :false-object nil))
                       (backend (agent-pipe-session-backend session))
                       (parse-fn (agent-pipe-backend-parse-event-fn backend))
                       (event (funcall parse-fn json)))
                  (when event
                    ;; parse-event may return a single event or a list
                    (let ((events (if (agent-pipe-event-p event)
                                      (list event)
                                    event)))
                      (dolist (ev events)
                        (agent-pipe--debug-log "event" "%s: %s"
                                               (agent-pipe-event-type ev)
                                               (truncate-string-to-width
                                                (format "%s" (agent-pipe-event-raw ev))
                                                200))
                        (agent-pipe--dispatch-event session ev)))))
              (error
               (message "agent-pipe: parse error: %s (line: %.80s)"
                        (error-message-string err) line))))))
      (let ((remaining (substring partial start)))
        (when (and agent-pipe-debug (> (length remaining) 0))
          (agent-pipe--debug-log "partial" "buffered %d bytes" (length remaining)))
        (process-put proc 'agent-pipe--partial remaining)))))

(defun agent-pipe--process-sentinel (proc event)
  "Sentinel for coding agent PROC.  EVENT is the process event string."
  (agent-pipe--debug-log "sentinel" "exit: %s (code %s)"
                         (string-trim event) (process-exit-status proc))
  (let ((session (process-get proc 'agent-pipe--session)))
    (when session
      ;; Log any remaining partial data that never got a newline
      (let ((remaining (process-get proc 'agent-pipe--partial)))
        (when (and remaining (> (length remaining) 0))
          (agent-pipe--debug-log "partial" "unprocessed at exit: %s" remaining)))
      (cond
       ((string-match-p "finished" event)
        (setf (agent-pipe-session-status session) 'completed))
       (t
        (setf (agent-pipe-session-status session) 'error)))
      (setf (agent-pipe-session-process session) nil)
      (agent-pipe--render-status session)
      (run-hook-with-args 'agent-pipe-finished-hook session))))

;;;; Event Dispatch

(defun agent-pipe--dispatch-event (session event)
  "Dispatch EVENT for SESSION to hooks and rendering."
  ;; Extract session ID and slash commands from system events
  (let ((sid (agent-pipe-event-session-id event)))
    (when (and sid (not (agent-pipe-session-id session)))
      (setf (agent-pipe-session-id session) sid)
      (agent-pipe--record-session-id session sid)))
  (when (eq (agent-pipe-event-type event) 'system)
    (let ((raw (agent-pipe-event-raw event)))
      (let ((cmds (alist-get 'slash_commands raw)))
        (when cmds
          (setf (agent-pipe-session-slash-commands session) cmds)))
      (let ((sk (alist-get 'skills raw)))
        (when sk
          (setf (agent-pipe-session-skills session) sk)))))

  ;; Run hooks
  (run-hook-with-args 'agent-pipe-event-hook session event)

  (let ((type (agent-pipe-event-type event)))
    (cond
     ((eq type 'text)
      (run-hook-with-args 'agent-pipe-text-hook
                          session (agent-pipe-event-text event))
      (agent-pipe--render-text session (agent-pipe-event-text event)))

     ((eq type 'tool-start)
      (run-hook-with-args 'agent-pipe-tool-hook
                          session (agent-pipe-event-tool-name event))
      (agent-pipe--render-tool-start session
                                     (agent-pipe-event-tool-name event)))

     ((eq type 'tool-input)
      (agent-pipe--render-tool-input session
                                     (agent-pipe-event-text event)))

     ((eq type 'tool-stop)
      (agent-pipe--render-tool-stop session))

     ((eq type 'tool-result)
      (agent-pipe--render-tool-result session
                                      (agent-pipe-event-text event)))

     ((eq type 'tool-error)
      (agent-pipe--render-tool-error session
                                     (agent-pipe-event-text event)))

     ((eq type 'result)
      (agent-pipe--render-result session event))

     ((eq type 'error)
      (agent-pipe--render-error session (agent-pipe-event-text event)))

     ((eq type 'message-start)
      nil)

     ((eq type 'message-stop)
      nil))))

;;;; Session History (Filesystem)

(defvar agent-pipe--session-metadata-cache (make-hash-table :test 'equal)
  "Cache of (file . mtime) -> parsed session metadata alist.")

(defun agent-pipe--claude-sessions-dir (directory)
  "Return the Claude Code sessions directory for DIRECTORY."
  (let* ((abs (directory-file-name (expand-file-name directory)))
         (encoded (replace-regexp-in-string "[/.]" "-" abs)))
    (expand-file-name encoded "~/.claude/projects/")))

(defun agent-pipe--parse-session-head (file)
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

(defun agent-pipe--scan-sessions (directory)
  "Scan Claude Code session files for DIRECTORY.
Returns a list of alists sorted by mtime (newest first),
each with keys: id, file, mtime, size, first-prompt, summary,
git-branch, timestamp."
  (let* ((sessions-dir (agent-pipe--claude-sessions-dir directory))
         (files (and (file-directory-p sessions-dir)
                     (directory-files sessions-dir t
                                     "^[0-9a-f].*\\.jsonl\\'")))
         results)
    (dolist (file files)
      (let* ((attrs (file-attributes file))
             (mtime (file-attribute-modification-time attrs))
             (size (file-attribute-size attrs))
             (cache-key (cons file mtime))
             (cached (gethash cache-key agent-pipe--session-metadata-cache))
             (metadata (or cached
                          (let ((parsed (agent-pipe--parse-session-head file)))
                            (puthash cache-key parsed
                                     agent-pipe--session-metadata-cache)
                            parsed))))
        (push (append metadata
                      `((file . ,file)
                        (mtime . ,mtime)
                        (size . ,size)))
              results)))
    (sort results (lambda (a b)
                    (time-less-p (alist-get 'mtime b)
                                 (alist-get 'mtime a))))))

(defun agent-pipe--format-session-date (time)
  "Format TIME as a human-friendly relative date string."
  (let* ((now (current-time))
         (days (/ (float-time (time-subtract now time)) 86400.0)))
    (cond
     ((< days 1)   (format-time-string "Today, %H:%M" time))
     ((< days 2)   (format-time-string "Yesterday, %H:%M" time))
     ((< days 7)   (format-time-string "%A, %H:%M" time))
     (t             (format-time-string "%b %d, %Y" time)))))

(defun agent-pipe--session-display-label (session-alist)
  "Build a display label for SESSION-ALIST for use in `completing-read'."
  (let* ((summary (alist-get 'summary session-alist))
         (prompt (alist-get 'first-prompt session-alist))
         (title (or summary
                    (and prompt
                         (truncate-string-to-width
                          (replace-regexp-in-string "[\n\r]+" " " prompt)
                          50 nil nil t))
                    "(no title)"))
         (branch (or (alist-get 'git-branch session-alist) ""))
         (mtime (alist-get 'mtime session-alist))
         (date-str (if mtime (agent-pipe--format-session-date mtime) ""))
         (id (or (alist-get 'id session-alist) "")))
    (format "%-52s %-16s  %-20s  %s"
            title branch date-str
            (propertize (substring id 0 (min 8 (length id)))
                        'face 'shadow))))

(defun agent-pipe--record-session-id (session sid)
  "Record SID in the session-id history for SESSION's directory.
Kept for backward compatibility; resume now uses filesystem scanning."
  (let* ((dir (agent-pipe-session-directory session))
         (existing (gethash dir agent-pipe--session-id-history)))
    (unless (member sid existing)
      (puthash dir (cons sid existing)
               agent-pipe--session-id-history))))

;;;; Output Buffer and Mode

(defvar-local agent-pipe--session-ref nil
  "The `agent-pipe-session' associated with this buffer.")

(defvar-local agent-pipe--input-marker nil
  "Marker at the start of the input area.")

(defvar-local agent-pipe--output-marker nil
  "Marker at the end of the output area (just before the separator).")

(defvar-local agent-pipe--in-tool nil
  "Non-nil when currently rendering inside a tool block.")

(defvar agent-pipe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-pipe-send-input)
    (define-key map (kbd "C-c C-k") #'agent-pipe-interrupt)
    (define-key map (kbd "C-c C-r") #'agent-pipe-resume)
    (define-key map (kbd "C-c C-l") #'agent-pipe-continue)
    (define-key map (kbd "C-c C-n") #'agent-pipe-new-session)
    map)
  "Keymap for `agent-pipe-mode'.")

(define-derived-mode agent-pipe-mode nil "AgentPipe"
  "Major mode for coding agent output buffers.
The buffer is read-only except for the input area at the bottom.
Text properties enforce read-only on the output region.

\\{agent-pipe-mode-map}"
  (setq-local agent-pipe--in-tool nil)
  (setq truncate-lines nil)
  (setq word-wrap t)
  (add-hook 'pre-command-hook #'agent-pipe--move-to-prompt nil t)
  (add-hook 'completion-at-point-functions
            #'agent-pipe-completion-at-point nil t)
  (when (fboundp 'company-mode)
    (company-mode 1)))

(defun agent-pipe--move-to-prompt ()
  "Move point to the input area when typing outside it."
  (when (and agent-pipe--input-marker
             (< (point) agent-pipe--input-marker)
             (eq this-command 'self-insert-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun agent-pipe--make-header-line (session)
  "Build a header line string for SESSION."
  (let ((backend-name (agent-pipe-backend-name
                       (agent-pipe-session-backend session)))
        (model (or (agent-pipe-session-model session) "default"))
        (dir (abbreviate-file-name (agent-pipe-session-directory session)))
        (status (agent-pipe-session-status session)))
    (format " %s | model: %s | %s | %s"
            backend-name model dir status)))

(defun agent-pipe--setup-buffer (session)
  "Set up the output buffer for SESSION."
  (let ((buf (agent-pipe-session-buffer session)))
    (with-current-buffer buf
      (agent-pipe-mode)
      (setq agent-pipe--session-ref session)
      (setq header-line-format
            '(:eval (agent-pipe--make-header-line
                     agent-pipe--session-ref)))

      ;; Initial buffer content
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        ;; Output marker: text inserted here goes before the separator
        (setq agent-pipe--output-marker (point-marker))
        (set-marker-insertion-type agent-pipe--output-marker nil)
        ;; Separator
        (insert (propertize (concat "\n" (make-string 40 ?─) "\n")
                            'face 'shadow
                            'read-only t
                            'field 'output
                            'front-sticky t
                            'rear-nonsticky t))
        ;; Prompt — rear-nonsticky prevents read-only from leaking
        ;; into the input area
        (insert (propertize "> "
                            'face 'minibuffer-prompt
                            'read-only t
                            'field 'agent-pipe-prompt
                            'front-sticky t
                            'rear-nonsticky t))
        ;; Input marker: everything after here is the writable input area.
        ;; Insertion type nil so it stays at the start of user input.
        (setq agent-pipe--input-marker (point-marker))))))


(defun agent-pipe--render-status (session)
  "Update the header line to reflect SESSION's current status."
  (let ((buf (agent-pipe-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (force-mode-line-update)))))

(defun agent-pipe--insert-output (session text &optional face)
  "Insert TEXT into SESSION's output area with optional FACE."
  (let ((buf (agent-pipe-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (save-excursion
            (goto-char agent-pipe--output-marker)
            (let ((str (if face (propertize text 'face face) text)))
              (insert (propertize str
                                  'read-only t
                                  'field 'output
                                  'front-sticky t
                                  'rear-nonsticky t)))
            (set-marker agent-pipe--output-marker (point))))
        ;; Auto-scroll if point is near the end
        (when (>= (point) (- agent-pipe--output-marker 1))
          (goto-char (point-max)))))))

(defun agent-pipe--render-text (session text)
  "Render a text event for SESSION."
  (agent-pipe--insert-output session text))

(defun agent-pipe--render-tool-start (session tool-name)
  "Render a tool-start event for SESSION."
  (setf (buffer-local-value 'agent-pipe--in-tool
                            (agent-pipe-session-buffer session))
        t)
  (let ((header (format "\n── %s " tool-name))
        (fill (make-string (max 1 (- 40 (length tool-name) 4)) ?─)))
    (agent-pipe--insert-output session (concat header fill "\n")
                               'shadow)))

(defun agent-pipe--render-tool-input (session text)
  "Render tool input TEXT for SESSION (truncated)."
  (when text
    (let ((truncated (if (> (length text) 200)
                         (concat (substring text 0 200) "...")
                       text)))
      (agent-pipe--insert-output session truncated 'font-lock-comment-face))))

(defun agent-pipe--render-tool-stop (session)
  "Render a tool-stop event for SESSION."
  (with-current-buffer (agent-pipe-session-buffer session)
    (setq agent-pipe--in-tool nil))
  (agent-pipe--insert-output session "\n" 'shadow))

(defun agent-pipe--render-result (session event)
  "Render a result EVENT for SESSION."
  (let ((text (agent-pipe-event-text event)))
    (when (and text (not (equal text "")))
      (agent-pipe--insert-output
       session
       (format "\n%s\n" (make-string 40 ?─))
       'shadow)
      (agent-pipe--insert-output session text 'font-lock-doc-face)
      (agent-pipe--insert-output session "\n"))))

(defun agent-pipe--render-tool-result (session text)
  "Render a tool result TEXT for SESSION."
  (when (and text (not (equal text "")))
    (let ((truncated (if (> (length text) 500)
                         (concat (substring text 0 500) "\n...")
                       text)))
      (agent-pipe--insert-output session truncated
                                 'font-lock-comment-face)
      (agent-pipe--insert-output session "\n"))))

(defun agent-pipe--render-tool-error (session text)
  "Render a tool error TEXT for SESSION."
  (when (and text (not (equal text "")))
    (agent-pipe--insert-output
     session (format "[tool error] %s\n" text) 'error)))

(defun agent-pipe--render-error (session text)
  "Render an error TEXT for SESSION."
  (agent-pipe--insert-output
   session (format "\n[ERROR] %s\n" text) 'error))

;;;; Process Management

(defun agent-pipe--stderr-filter (_proc output)
  "Filter for stderr from agent subprocess.
Logs OUTPUT to the debug buffer and `*Messages*'."
  (agent-pipe--debug-log "stderr" "%s" output)
  (message "agent-pipe stderr: %s" (string-trim-right output)))

(defun agent-pipe--start-process (session prompt &optional resume-id)
  "Start a subprocess for SESSION with PROMPT.
If RESUME-ID is non-nil, resume that session."
  (let* ((backend (agent-pipe-session-backend session))
         (cmd (if resume-id
                  (funcall (agent-pipe-backend-build-resume-fn backend)
                           prompt resume-id session)
                (funcall (agent-pipe-backend-build-cmd-fn backend)
                         prompt session)))
         (program (car cmd))
         (args (cdr cmd))
         (default-directory (agent-pipe-session-directory session))
         (buf (agent-pipe-session-buffer session))
         (proc-name (format "agent-pipe-%s"
                            (agent-pipe-backend-name backend)))
         (stderr-buf (when agent-pipe-debug
                       (generate-new-buffer
                        (format " *agent-pipe-stderr:%s*"
                                (agent-pipe-backend-name backend)))))
         (proc (make-process
                :name proc-name
                :buffer buf
                :command (cons program args)
                :connection-type 'pipe
                :filter #'agent-pipe--process-filter
                :sentinel #'agent-pipe--process-sentinel
                :noquery t
                :stderr stderr-buf)))
    ;; Attach stderr filter when debugging
    (when stderr-buf
      (set-process-filter (get-buffer-process stderr-buf)
                          #'agent-pipe--stderr-filter))
    (agent-pipe--debug-log "cmd" "%s %s" program
                           (mapconcat #'shell-quote-argument args " "))
    (agent-pipe--debug-log "cmd" "cwd: %s" default-directory)
    (process-put proc 'agent-pipe--session session)
    (process-put proc 'agent-pipe--partial "")
    (setf (agent-pipe-session-process session) proc)
    (setf (agent-pipe-session-status session) 'running)
    (push prompt (agent-pipe-session-history session))
    (agent-pipe--render-status session)
    ;; Send the prompt via stdin then close -- claude -p reads from
    ;; stdin when it's a pipe, ignoring positional arguments.
    (process-send-string proc prompt)
    (process-send-eof proc)
    proc))

;;;; Session Management

(defun agent-pipe--get-backend (name)
  "Get the backend struct for NAME."
  (or (gethash name agent-pipe--backends)
      (error "Unknown agent-pipe backend: %s" name)))

(defun agent-pipe--buffer-name (backend-name directory)
  "Generate a buffer name for BACKEND-NAME in DIRECTORY."
  (format "*agent-pipe:%s [%s]*"
          backend-name
          (file-name-nondirectory
           (directory-file-name directory))))

(defun agent-pipe--get-or-create-session (backend-name directory
                                                       &optional model)
  "Get or create a session for BACKEND-NAME in DIRECTORY.
MODEL overrides the default model if non-nil."
  (let* ((key (cons directory backend-name))
         (existing (gethash key agent-pipe--sessions)))
    (if (and existing
             (buffer-live-p (agent-pipe-session-buffer existing)))
        existing
      (let* ((backend (agent-pipe--get-backend backend-name))
             (buf-name (agent-pipe--buffer-name backend-name directory))
             (buf (get-buffer-create buf-name))
             (session (make-agent-pipe-session
                       :backend backend
                       :directory directory
                       :buffer buf
                       :status 'idle
                       :model (or model
                                  agent-pipe-claude-default-model)
                       :allowed-tools agent-pipe-claude-allowed-tools)))
        (puthash key session agent-pipe--sessions)
        (agent-pipe--setup-buffer session)
        session))))

;;;; Public API

;;;###autoload
(defun agent-pipe-start (&optional backend model directory)
  "Start a new coding agent session.
BACKEND is a backend name symbol (default: `agent-pipe-default-backend').
MODEL is a model name string (default: backend-specific).
DIRECTORY is the working directory (default: `default-directory')."
  (interactive)
  (let* ((backend-name (or backend agent-pipe-default-backend))
         (dir (or directory default-directory))
         (session (agent-pipe--get-or-create-session
                   backend-name dir model)))
    (pop-to-buffer (agent-pipe-session-buffer session))
    session))

(defun agent-pipe-send (prompt &optional session)
  "Send PROMPT to the coding agent SESSION.
If SESSION is nil, use the current buffer's session.
If the session has a previous session ID, uses --resume."
  (interactive
   (list (read-string "Prompt: ")))
  (let* ((session (or session (agent-pipe-current-session)))
         (existing-proc (agent-pipe-session-process session)))
    (when (and existing-proc (process-live-p existing-proc))
      (error "Session is already running"))
    ;; Insert the prompt into the output area
    (agent-pipe--insert-output
     session (format "\n\n> %s\n\n" prompt) 'bold)
    ;; Start process, resuming if we have a session ID
    (let ((sid (agent-pipe-session-id session)))
      (agent-pipe--start-process session prompt sid))))

(defun agent-pipe-send-input ()
  "Send the text in the input area to the coding agent."
  (interactive)
  (let ((session agent-pipe--session-ref))
    (unless session
      (error "No agent-pipe session in this buffer"))
    (let* ((input-start agent-pipe--input-marker)
           (input-end (point-max))
           (prompt (string-trim
                    (buffer-substring-no-properties input-start input-end))))
      (when (equal prompt "")
        (error "Empty prompt"))
      ;; Clear the input area
      (delete-region input-start input-end)
      ;; Send
      (agent-pipe-send prompt session))))

(defun agent-pipe-interrupt (&optional session)
  "Interrupt the running process for SESSION."
  (interactive)
  (let* ((session (or session (agent-pipe-current-session)))
         (proc (agent-pipe-session-process session)))
    (if (and proc (process-live-p proc))
        (progn
          (kill-process proc)
          (setf (agent-pipe-session-status session) 'interrupted)
          (agent-pipe--insert-output session "\n[interrupted]\n" 'warning)
          (agent-pipe--render-status session))
      (message "No running process to interrupt"))))

(defun agent-pipe-resume (&optional session)
  "Resume a past session by picking from history.
Scans session files on disk for metadata display.
SESSION defaults to the current buffer's session."
  (interactive)
  (let* ((session (or session (agent-pipe-current-session)))
         (dir (agent-pipe-session-directory session))
         (sessions (agent-pipe--scan-sessions dir)))
    (unless sessions
      (error "No sessions found for %s" dir))
    (let* ((labeled (mapcar (lambda (s)
                              (cons (agent-pipe--session-display-label s) s))
                            sessions))
           (chosen-label (completing-read "Resume session: "
                                          labeled nil t))
           (chosen (cdr (assoc chosen-label labeled)))
           (sid (alist-get 'id chosen))
           (prompt (read-string "Follow-up prompt (RET to continue): ")))
      (when (equal prompt "")
        (setq prompt "Continue where we left off."))
      (setf (agent-pipe-session-id session) sid)
      (agent-pipe--insert-output
       session (format "\n\n> [resume %s] %s\n\n"
                       (or (alist-get 'summary chosen)
                           (truncate-string-to-width sid 8))
                       prompt)
       'bold)
      (agent-pipe--start-process session prompt sid))))

(defun agent-pipe-continue (&optional session)
  "Continue the most recent session in the current directory.
SESSION defaults to the current buffer's session."
  (interactive)
  (let* ((session (or session (agent-pipe-current-session)))
         (dir (agent-pipe-session-directory session))
         (sessions (agent-pipe--scan-sessions dir))
         (latest (car sessions)))
    (unless latest
      (error "No sessions found for %s" dir))
    (let ((sid (alist-get 'id latest))
          (prompt (read-string "Follow-up prompt (RET to continue): ")))
      (when (equal prompt "")
        (setq prompt "Continue where we left off."))
      (setf (agent-pipe-session-id session) sid)
      (agent-pipe--insert-output
       session (format "\n\n> [continue %s] %s\n\n"
                       (or (alist-get 'summary latest)
                           (truncate-string-to-width sid 8))
                       prompt)
       'bold)
      (agent-pipe--start-process session prompt sid))))

(defun agent-pipe-new-session ()
  "Start a fresh session in the current buffer's directory."
  (interactive)
  (let* ((session (agent-pipe-current-session))
         (dir (agent-pipe-session-directory session))
         (backend-name (agent-pipe-backend-name
                        (agent-pipe-session-backend session)))
         (key (cons dir backend-name)))
    ;; Kill existing process if any
    (let ((proc (agent-pipe-session-process session)))
      (when (and proc (process-live-p proc))
        (kill-process proc)))
    ;; Remove from session table
    (remhash key agent-pipe--sessions)
    ;; Create fresh session reusing the same buffer
    (let* ((backend (agent-pipe-session-backend session))
           (buf (agent-pipe-session-buffer session))
           (new-session (make-agent-pipe-session
                         :backend backend
                         :directory dir
                         :buffer buf
                         :status 'idle
                         :model (agent-pipe-session-model session)
                         :allowed-tools (agent-pipe-session-allowed-tools
                                         session))))
      (puthash key new-session agent-pipe--sessions)
      (agent-pipe--setup-buffer new-session)
      (message "New session started"))))

(defun agent-pipe-current-session ()
  "Return the session for the current buffer."
  (or agent-pipe--session-ref
      (error "No agent-pipe session in this buffer")))

;;;; Slash Command Completion

(defun agent-pipe-completion-at-point ()
  "Complete slash commands and skills in the input area."
  (when (and agent-pipe--input-marker
             (>= (point) agent-pipe--input-marker))
    (save-excursion
      (goto-char agent-pipe--input-marker)
      (skip-chars-forward " \t")
      (when (eq (char-after) ?/)
        (let* ((slash-pos (point))
               (start (1+ slash-pos))
               (end (save-excursion
                      (goto-char start)
                      (skip-chars-forward "a-zA-Z0-9_:-")
                      (point)))
               (session agent-pipe--session-ref)
               (commands (when session
                           (agent-pipe-session-slash-commands session)))
               (skills (when session
                         (agent-pipe-session-skills session))))
          (list start end
                (or commands
                    '("help" "compact" "clear" "config"
                      "cost" "init" "model" "review"
                      "memory" "status" "doctor"))
                :exclusive t
                :annotation-function
                (lambda (c)
                  (when (member c skills)
                    " (skill)"))))))))

(provide 'agent-pipe)
;;; agent-pipe.el ends here
