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

(defun agent-pipe--claude-build-cmd (prompt session)
  "Build a command list to start a new Claude Code session.
PROMPT is the user prompt string.  SESSION is the `agent-pipe-session'."
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
    (setq args (append args (list prompt)))
    (cons program args)))

(defun agent-pipe--claude-build-resume-cmd (prompt session-id session)
  "Build a command list to resume a Claude Code session.
PROMPT is the follow-up prompt.  SESSION-ID is the session to resume.
SESSION is the `agent-pipe-session'."
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
    (setq args (append args (list prompt)))
    (cons program args)))

(defun agent-pipe--claude-parse-event (json)
  "Parse a JSON alist from Claude Code into an `agent-pipe-event'.
Returns nil if the event is not recognized or not relevant."
  (let ((type (alist-get 'type json)))
    (cond
     ((equal type "system")
      ;; System init event -- extract session info if present
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
              ;; text block start -- might have initial text
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
      ;; Could extract stop_reason, usage -- store in raw for now
      nil)

     (t nil))))

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

;;;; NDJSON Process Filter

(defun agent-pipe--process-filter (proc output)
  "Process filter for NDJSON output from coding agent PROC.
OUTPUT is the string chunk received."
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
                    (agent-pipe--dispatch-event session event)))
              (error
               (message "agent-pipe: parse error: %s (line: %.80s)"
                        (error-message-string err) line))))))
      (process-put proc 'agent-pipe--partial (substring partial start)))))

(defun agent-pipe--process-sentinel (proc event)
  "Sentinel for coding agent PROC.  EVENT is the process event string."
  (let ((session (process-get proc 'agent-pipe--session)))
    (when session
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
  ;; Extract session ID from system or result events
  (let ((sid (agent-pipe-event-session-id event)))
    (when (and sid (not (agent-pipe-session-id session)))
      (setf (agent-pipe-session-id session) sid)
      (agent-pipe--record-session-id session sid)))

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

     ((eq type 'result)
      (agent-pipe--render-result session event))

     ((eq type 'error)
      (agent-pipe--render-error session (agent-pipe-event-text event)))

     ((eq type 'message-start)
      nil)

     ((eq type 'message-stop)
      nil))))

;;;; Session ID History

(defun agent-pipe--record-session-id (session sid)
  "Record SID in the session-id history for SESSION's directory."
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
  (add-hook 'pre-command-hook #'agent-pipe--move-to-prompt nil t))

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

(defun agent-pipe--render-error (session text)
  "Render an error TEXT for SESSION."
  (agent-pipe--insert-output
   session (format "\n[ERROR] %s\n" text) 'error))

;;;; Process Management

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
         (proc (make-process
                :name proc-name
                :buffer buf
                :command (cons program args)
                :connection-type 'pipe
                :filter #'agent-pipe--process-filter
                :sentinel #'agent-pipe--process-sentinel
                :noquery t)))
    (process-put proc 'agent-pipe--session session)
    (process-put proc 'agent-pipe--partial "")
    (setf (agent-pipe-session-process session) proc)
    (setf (agent-pipe-session-status session) 'running)
    (push prompt (agent-pipe-session-history session))
    (agent-pipe--render-status session)
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
SESSION defaults to the current buffer's session."
  (interactive)
  (let* ((session (or session (agent-pipe-current-session)))
         (dir (agent-pipe-session-directory session))
         (ids (gethash dir agent-pipe--session-id-history)))
    (unless ids
      (error "No session history for %s" dir))
    (let ((chosen (completing-read "Resume session: " ids nil t))
          (prompt (read-string "Follow-up prompt: ")))
      (when (equal prompt "")
        (error "Empty prompt"))
      (setf (agent-pipe-session-id session) chosen)
      (agent-pipe--insert-output
       session (format "\n\n> [resume %s] %s\n\n" chosen prompt) 'bold)
      (agent-pipe--start-process session prompt chosen))))

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

(provide 'agent-pipe)
;;; agent-pipe.el ends here
