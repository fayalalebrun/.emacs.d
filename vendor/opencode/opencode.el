;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (markdown-mode "2.6") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-permission)
(require 'opencode-question)
(require 'opencode-record)
(require 'opencode-sessions)
(require 'plz-media-type)
(require 'plz-event-source)
(require 'project)

;; pending fix upstream at https://github.com/r0man/plz-event-source/pull/15
;; see also: https://codeberg.org/sczi/opencode.el/issues/12
(setq plz-event-source-parser--line-regexp
      (rx (*? not-newline) (or "\r\n" "\n" "\r")))

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defgroup opencode-faces nil
  "Faces for opencode interface."
  :group 'opencode)

(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port for the opencode server."
  :type 'integer
  :group 'opencode)

(defcustom opencode-command "opencode"
  "Base command for the opencode executable.
Used when `opencode-serve-command' is nil to construct the serve command."
  :type 'string
  :group 'opencode)

(defcustom opencode-serve-command nil
  "Full command to start the opencode server.
When nil, the command is constructed from `opencode-command',
`opencode-host', and `opencode-port'.

Set this to a custom command for special cases like nix:
  \"nix run github:numtide/nix-ai-tools#opencode -- serve --port 4096 --hostname localhost\""
  :type '(choice (const :tag "Construct from opencode-command" nil)
		 (string :tag "Custom command"))
  :group 'opencode)

(defcustom opencode-auto-start-server nil
  "Deprecated compatibility option.
Emacs never starts an OpenCode server; start `opencode serve' externally."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-use-fast-event-stream t
  "Use OpenCode-specific SSE parsing for the live event stream.
This avoids `plz-event-source' buffer insertion/deletion overhead and allows
client-side redaction before JSON decoding."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-redact-hidden-tool-output t
  "Redact hidden tool output before decoding event JSON."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-redacted-tool-output-placeholder "[hidden tool output redacted]"
  "Placeholder used when hidden tool output is redacted from events."
  :type 'string
  :group 'opencode)

(defcustom opencode-session-deleted-functions nil
  "Hook run when an opencode session is deleted.
This is an abnormal hook. Each function receives one argument, SESSION-ID."
  :type 'hook
  :group 'opencode)

(defvar opencode--process nil
  "Opencode server process when started by Emacs.")

(defvar-local opencode--sse-pending ""
  "Unprocessed SSE data for the current OpenCode event process.")

(defvar-local opencode--sse-discarding-ignored-frame nil
  "Non-nil while discarding the rest of a large ignored SSE frame.")

(defvar-local opencode--sse-discard-tail ""
  "Suffix retained while discarding to detect split SSE delimiters.")

(defvar-local opencode--sse-open-logged nil
  "Non-nil after logging the open event for this SSE process.")

(defclass opencode--event-stream (plz-media-type:application/octet-stream)
  ((coding-system :initform 'utf-8)
   (type :initform 'text)
   (subtype :initform 'event-stream))
  "Fast media type for OpenCode server-sent events.")

;; so invisible prompt "> " doesn't make whole prompt invisible
(add-to-list 'comint--prompt-rear-nonsticky 'invisible)

(defun opencode--server-running-p ()
  "Return non-nil if an opencode server is running at configured host and port."
  (condition-case err
      (plz 'get (format "http://%s:%d/global/health" opencode-host opencode-port)
        :headers (list (opencode--auth-header))
        :timeout 1)
    (plz-http-error
     (when-let (plz-error (cl-third err))
       (when (= 401 (plz-response-status (plz-error-response plz-error)))
         (user-error "OpenCode server is already running but password protected: \
set `opencode-server-password' to connect to it"))))
    (error nil)))

(defun opencode--connected-p ()
  "Return non-nil if the configured OpenCode server is connected."
  (and opencode-api-url
       (process-live-p opencode--event-subscription)
       (opencode--server-running-p)
       t))

(defun opencode--serve-command ()
  "Return the command to start the opencode server."
  (or opencode-serve-command
      (format "%s serve --port %d --hostname %s"
              opencode-command opencode-port opencode-host)))

(defun opencode--start-server (on-connect)
  "Start an opencode server and run ON-CONNECT when ready."
  (ignore on-connect)
  (user-error "Emacs OpenCode server autostart is disabled; start opencode serve externally"))

(defun opencode-autoconnect (on-connect)
  "Connect to an existing OpenCode server and run ON-CONNECT."
  (cond
   ;; Already connected
   ((opencode--connected-p)
    (funcall on-connect))
   ;; We started a server process that's still alive
   ((process-live-p opencode--process)
    (funcall on-connect))
   ;; Server already running externally
   ((opencode--server-running-p)
    (opencode-connect opencode-host opencode-port)
    (funcall on-connect))
   (t
    (user-error "No opencode server running at %s:%d; start opencode serve externally"
                opencode-host opencode-port))))


;;;###autoload
(defun opencode ()
  "Open opencode sessions control buffer for the current project directory.
Connects only to an existing server; Emacs does not start `opencode serve'."
  (interactive)
  (let ((project-dir (expand-file-name
                      (if-let (proj (project-current))
                          (project-root proj)
                        default-directory))))
    (opencode-autoconnect (lambda () (opencode-open-project project-dir)))))

(defun opencode-connect (host port)
  "Connect to opencode server, prompting for HOST and PORT."
  (interactive
   (list (read-string "Host: " opencode-host)
         (read-number "Port: " opencode-port)))
  (when opencode--event-subscription
    (user-error "Already connected"))
  (setq opencode-api-url (format "http://%s:%d" host port))
  (setq opencode--slash-commands-by-directory nil)
  (opencode--subscribe-global-events)
  (opencode--fetch-agents)
  (opencode-api-configured-providers result
    (setq opencode-providers (alist-get 'providers result)))
  (message "Connected to %s" opencode-api-url))

(defun opencode-open-project (directory)
  "Open sessions control buffer for DIRECTORY."
  (opencode--download-slash-commands directory)
  (let ((buffer-name (format "*OpenCode Sessions in %s*" directory)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (opencode-session-control-mode)
        (setq default-directory directory)
        (opencode-api-current-project project
          (let-alist project
            (setf (map-elt opencode--session-control-buffers .id)
                  (cons (current-buffer)
                        (seq-filter #'buffer-live-p
                                    (map-elt opencode--session-control-buffers
                                             .id))))))
        (opencode-sessions-redisplay)))
    (pop-to-buffer buffer-name)))

(defvar opencode-worktree-directory (expand-file-name "~/opencode_worktrees/")
  "Directory to store worktrees created for opencode.")

(defun opencode-new-worktree ()
  "Create a new git branch, and worktree prompting for a name.
Then open an opencode session in it."
  (interactive)
  (let* ((name (read-string "Worktree and branch name: "))
         (directory (file-name-concat opencode-worktree-directory name)))
    (when (magit-worktree-branch directory name "HEAD")
      (let ((default-directory directory))
        (opencode-new-session)))))

(defun opencode-select-project ()
  "Completing read to prompt which project to select."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api-projects projects
       (opencode-open-project
        (opencode--annotated-completion
         "Project: "
         (cl-loop for project in projects
                  for worktree = (alist-get 'worktree project)
                  when worktree
                  collect (list (string-remove-prefix
                                 (expand-file-name "~/")
                                 worktree)
                                worktree
                                (opencode--format-time-ago
                                 (opencode--time-ago
                                  project 'updated))))))))))

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (when opencode-event-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-event-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "[%s] %s: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        type
                        event))
        (opencode--truncate-at-max-lines opencode-event-log-max-lines)))))

(defun opencode--run-file-edited-hook (tool state)
  "Run `opencode-file-edited-functions' for completed TOOL with STATE."
  (let-alist state
    (dolist (file-ranges (pcase tool
                           ("write"
                            (when .input.filePath
                              (list (cons .input.filePath nil))))
                           ((or "edit" "apply_patch")
                            (unless .metadata.diff
                              (error "Missing diff metadata for %s tool" tool))
                            (opencode-diff->source-line-ranges .metadata.diff))))
      (run-hook-with-args 'opencode-file-edited-functions
                          (car file-ranges)
                          (cdr file-ranges)))))

(defun opencode--raw-event-type-p (data type)
  "Return non-nil if raw JSON event DATA contains event TYPE."
  (and (stringp data)
       (string-match-p
        (format "\"type\"[[:space:]]*:[[:space:]]*%s"
                (regexp-quote (json-encode-string type)))
        data)))

(defun opencode--ignored-message-data-p (data)
  "Return non-nil if raw event DATA can be ignored before JSON decoding."
  (cl-some (lambda (type)
             (opencode--raw-event-type-p data type))
           '("file.watcher.updated" "server.heartbeat" "session.diff" "sync")))

(defun opencode--message-session-id (data)
  "Return the session id associated with decoded event DATA."
  (let-alist (alist-get 'properties data)
    (pcase (alist-get 'type data)
      ((or "message.updated" "message.part.delta" "session.idle"
           "session.status" "session.error" "permission.asked"
           "permission.replied" "permission.rejected"
           "question.asked" "question.replied" "question.rejected"
           "session.next.shell.started" "session.next.shell.ended")
       .sessionID)
      ("message.part.updated" .part.sessionID))))

(defun opencode--queue-message-while-bootstrapping (data)
  "Queue DATA when its session buffer is still replaying history."
  (when-let* ((session-id (opencode--message-session-id data))
              (buffer (gethash session-id opencode-session-buffers)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'opencode-session--bootstrapping buffer))
      (with-current-buffer buffer
        (opencode-record--queued-event data)
        (push data opencode-session--queued-events))
      t)))

(defun opencode--raw-user-message-updated-p (data)
  "Return non-nil if raw message.updated DATA is for a user message."
  (when (stringp data)
    (let ((prefix (substring data 0 (min (length data) 4096))))
      ;; The role lives in the small metadata prefix before large summaries.
      (string-match-p
       (rx "\"role\"" (* space) ":" (* space) "\"user\"")
       prefix))))

(defun opencode--redactable-tool-output-data-p (data)
  "Return non-nil when DATA is a completed hidden tool update."
  (and opencode-redact-hidden-tool-output
       (stringp data)
       (not opencode-show-tool-output)
       (opencode--raw-event-type-p data "message.part.updated")
       (opencode--raw-event-type-p data "tool")
       (string-match-p "\"status\"[[:space:]]*:[[:space:]]*\"completed\"" data)
       ;; Preserve user-run shell command output, which the UI displays even
       ;; when generic tool output is hidden.
       (not (string-match-p "\"tool\"[[:space:]]*:[[:space:]]*\"bash\"" data))))

(defun opencode--redact-tool-output-data (data)
  "Return DATA with completed hidden tool output replaced by a placeholder."
  (if (opencode--redactable-tool-output-data-p data)
      (replace-regexp-in-string
       (rx "\"output\":\""
           (* (or (seq "\\" anything)
                  (not (any "\\\""))))
           "\"")
       (concat "\"output\":"
               (json-encode-string opencode-redacted-tool-output-placeholder))
       data t t)
    data))

(defun opencode--sse-line-value (line field)
  "Return the SSE value in LINE for FIELD, or nil."
  (let ((prefix (concat field ":")))
    (when (string-prefix-p prefix line)
      (let ((value (substring line (length prefix))))
        (if (string-prefix-p " " value)
            (substring value 1)
          value)))))

(defvar opencode--files-finished-editing-current nil
  "Current `opencode-files-finished-editing-functions' run context.")

(defvar opencode--files-finished-editing-queue nil
  "Queued `opencode-files-finished-editing-functions' run contexts.")

(defun opencode-report-diagnostic (message)
  "Queue diagnostic MESSAGE for the active finished-editing hook."
  (unless opencode--files-finished-editing-current
    (error "`opencode-report-diagnostic' called outside a finished-editing hook"))
  (when (and message (not (string= message "")))
    (push message
          (plist-get opencode--files-finished-editing-current :diagnostics))))

(defun opencode-hook-finished ()
  "Mark the active finished-editing hook as complete."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "`opencode-hook-finished' called outside a finished-editing hook"))
    (unless (plist-get context :active-hook)
      (error "`opencode-hook-finished' called without an active hook"))
    (plist-put context :active-hook nil)
    (opencode--run-next-files-finished-editing-hook)))

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--sse-delimiter-match (string)
  "Return match data for the first SSE frame delimiter in STRING."
  (string-match (rx (or "\r\n\r\n" "\n\n" "\r\r")) string))

(defun opencode--sse-delimiter-tail (string)
  "Return suffix of STRING needed to detect a split SSE delimiter."
  (substring string (max 0 (- (length string) 3))))

(defun opencode--sse-ignored-frame-start-p (data)
  "Return non-nil if DATA starts an ignored SSE frame."
  (when-let ((raw-data (opencode--sse-frame-data-prefix data)))
    (opencode--ignored-message-data-p raw-data)))

(defun opencode--sse-frame-data-prefix (data)
  "Return the first data line value from SSE frame prefix DATA."
  (when (and (stringp data)
             (string-match (rx string-start "data:" (? " ")
                               (group (* anything)))
                            data))
    (match-string 1 data)))

(defun opencode--sse-record-ignored-frame (frame)
  "Record that SSE FRAME was intentionally ignored by the prefilter."
  (when-let ((raw-data (opencode--sse-frame-data-prefix frame)))
    (opencode-record--raw-event raw-data t)))

(defun opencode--sse-dispatch-frame (frame)
  "Dispatch one SSE FRAME from the global event stream."
  (let (event-type data-lines)
    (dolist (line (split-string frame (rx (or "\r\n" "\n" "\r"))))
      (cond
       ((string-prefix-p ":" line))
       ((when-let ((event (opencode--sse-line-value line "event")))
          (setq event-type event)))
       ((when-let ((data (opencode--sse-line-value line "data")))
          (push data data-lines)))))
    (when data-lines
      (let ((data (mapconcat #'identity (nreverse data-lines) "\n")))
        (opencode-record--write
         "sse.frame"
         `((event . ,event-type)
           (data . ,data)))
        (when (and (not (string-empty-p data))
                   (or (null event-type) (string= event-type "message")))
          (opencode--handle-global-event-data data))))))

(defun opencode--sse-process-chunk (chunk)
  "Process SSE CHUNK without using an Emacs stream buffer."
  (when (stringp chunk)
    (while (not (string-empty-p chunk))
      (cond
       (opencode--sse-discarding-ignored-frame
        (let ((discard-data (concat opencode--sse-discard-tail chunk)))
          (if (opencode--sse-delimiter-match discard-data)
              (setq chunk (substring discard-data (match-end 0))
                    opencode--sse-discard-tail ""
                    opencode--sse-discarding-ignored-frame nil)
            (setq opencode--sse-discard-tail
                  (opencode--sse-delimiter-tail discard-data)
                  chunk ""))))
       ((and (string-empty-p opencode--sse-pending)
             (opencode--sse-ignored-frame-start-p chunk))
         (if (opencode--sse-delimiter-match chunk)
            (let ((frame (substring chunk 0 (match-beginning 0)))
                  (next (match-end 0)))
              (opencode--sse-record-ignored-frame frame)
              (setq chunk (substring chunk next)))
          (opencode--sse-record-ignored-frame chunk)
           (setq opencode--sse-discard-tail
                 (opencode--sse-delimiter-tail chunk)
                 chunk ""
                 opencode--sse-discarding-ignored-frame t)))
       (t
        (setq opencode--sse-pending (concat opencode--sse-pending chunk)
              chunk "")
         (if (opencode--sse-ignored-frame-start-p opencode--sse-pending)
             (if (opencode--sse-delimiter-match opencode--sse-pending)
                (let ((frame (substring opencode--sse-pending
                                        0 (match-beginning 0)))
                      (next (match-end 0)))
                  (opencode--sse-record-ignored-frame frame)
                  (setq chunk (substring opencode--sse-pending next)
                        opencode--sse-pending ""))
              (opencode--sse-record-ignored-frame opencode--sse-pending)
               (setq opencode--sse-discard-tail
                     (opencode--sse-delimiter-tail opencode--sse-pending)
                     opencode--sse-pending ""
                    opencode--sse-discarding-ignored-frame t))
          (while (opencode--sse-delimiter-match opencode--sse-pending)
            (let ((frame (substring opencode--sse-pending 0 (match-beginning 0)))
                  (next (match-end 0)))
              (setq opencode--sse-pending (substring opencode--sse-pending next))
              (unless (string-empty-p frame)
                (opencode--sse-dispatch-frame frame))))))))))

(cl-defmethod plz-media-type-process ((media-type opencode--event-stream) process chunk)
  "Process OpenCode SSE CHUNK using MEDIA-TYPE for PROCESS."
  (with-current-buffer (process-buffer process)
    (unless opencode--sse-open-logged
      (setq opencode--sse-open-logged t)
      (opencode-record--sse-lifecycle "open")
      (opencode--log-event "OPEN" nil))
    (when-let ((body (plz-response-body chunk)))
      (when (stringp body)
        (opencode--sse-process-chunk
         (plz-media-type-decode-coding-string media-type body))))))

(cl-defmethod plz-media-type-then ((media-type opencode--event-stream) response)
  "Finalize OpenCode event stream RESPONSE without parsing a buffered body."
  (cl-call-next-method media-type response)
  (setf (plz-response-body response) nil)
  response)

(cl-defmethod plz-media-type-else ((_media-type opencode--event-stream) error)
  "Return OpenCode event stream ERROR without parsing a buffered body."
  error)

(defun opencode--maybe-start-files-finished-editing-hook ()
  "Start the next queued finished-editing hook run if none is active."
  (unless opencode--files-finished-editing-current
    (when opencode--files-finished-editing-queue
      (let ((context (pop opencode--files-finished-editing-queue)))
        (setq opencode--files-finished-editing-current context)
        (opencode--run-next-files-finished-editing-hook)))))

(defun opencode--finish-files-finished-editing-hook-run (context)
  "Finish finished-editing hook run CONTEXT and send queued diagnostics."
  (let ((diagnostics (nreverse (plist-get context :diagnostics)))
        (buffer (plist-get context :session-buffer)))
    (setq opencode--files-finished-editing-current nil)
    (when (and diagnostics (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-session--send-synthetic-input
         (opencode--files-finished-editing-diagnostic-message diagnostics)))))
  (opencode--maybe-start-files-finished-editing-hook))

(defun opencode--run-next-files-finished-editing-hook ()
  "Run the next hook in the active finished-editing context."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "No active finished-editing hook context"))
    (when-let ((buffer (plist-get context :session-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if-let ((hook (car (plist-get context :hooks))))
              (let (result errored)
                (plist-put context :hooks (cdr (plist-get context :hooks)))
                (plist-put context :active-hook hook)
                (condition-case err
                    (setq result (funcall hook (plist-get context :files)))
                  (error
                   (setq errored t)
                   (opencode--log-event
                    "WARNING FINISHED EDITING HOOK"
                    (format "%S failed: %s" hook (error-message-string err)))
                   (when (and (eq opencode--files-finished-editing-current context)
                              (eq (plist-get context :active-hook) hook))
                     (plist-put context :active-hook nil)
                     (opencode--run-next-files-finished-editing-hook))))
                (when (and (not errored)
                           (not (eq result :opencode-async))
                           (eq opencode--files-finished-editing-current context)
                           (eq (plist-get context :active-hook) hook))
                  (plist-put context :active-hook nil)
                  (opencode--run-next-files-finished-editing-hook)))
            (opencode--finish-files-finished-editing-hook-run context)))))))

(defun opencode--maybe-run-file-edited-hook (part)
  "Run `opencode-file-edited-functions' if PART completed a file edit."
  (let-alist part
    (when (and (equal .type "tool")
               (equal .state.status "completed"))
      (opencode--run-file-edited-hook .tool .state))))

(defun opencode--message-summary-diff-files (info)
  "Return file names from INFO summary diffs."
  (when-let ((diffs (map-nested-elt info '(summary diffs))))
    (delq nil
          (mapcar (lambda (diff)
                    (opencode--resolve-file-reference
                     (alist-get 'file diff)))
                  (seq-into diffs 'list)))))

(defun opencode--record-files-edited-this-turn (info)
  "Record INFO summary diff files for the session's current turn."
  (let-alist info
    (when (and .sessionID (map-nested-elt info '(summary diffs)))
      (let ((buffer (gethash .sessionID opencode-session-buffers))
            (files (opencode--message-summary-diff-files info)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq opencode--files-edited-this-turn files)))))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (when-let ((buffer (gethash session-id opencode-session-buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when opencode--files-edited-this-turn
          (let ((files opencode--files-edited-this-turn))
            (setq opencode--files-edited-this-turn nil)
            (when-let ((hooks (append opencode-files-finished-editing-functions
                                      opencode-project-files-finished-editing-functions)))
              (setq opencode--files-finished-editing-queue
                    (append opencode--files-finished-editing-queue
                            (list (list :session-buffer (current-buffer)
                                        :files files
                                        :hooks hooks
                                        :diagnostics nil
                                        :active-hook nil))))
              (opencode--maybe-start-files-finished-editing-hook))))))))

(defun opencode--indent-files (files)
  "Indent FILES with Emacs."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (unless (opencode--indent-sensitive-buffer-p)
        (let ((inhibit-message t))
          (revert-buffer t t t)
          (indent-region (point-min) (point-max)))
        (when (buffer-modified-p)
          (message "opencode indented %s" file)
          (save-buffer))))))

(defun opencode-run-command-diagnostic (command)
  "Run COMMAND asynchronously and report diagnostics.
For use within `opencode-files-finished-editing-functions' or
`opencode-project-files-finished-editing-functions'."
  (let* ((command-name (car command))
         (buffer (generate-new-buffer (format "*%s*" command-name)))
         (command-string (string-join command " ")))
    (condition-case err
        (progn
          (make-process
           :name command-name
           :buffer buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (unwind-protect
                   (unless (zerop (process-exit-status process))
                     (let ((output (with-current-buffer (process-buffer process)
                                     (string-trim (buffer-string)))))
                       (opencode-report-diagnostic
                        (if (equal output "")
                            (format "`%s` failed" command-string)
                          (format "`%s` failed:\n\n%s"
                                  command-string
                                  output)))))
                 (kill-buffer buffer)
                 (opencode-hook-finished)))))
          :opencode-async)
      (error
       (kill-buffer buffer)
       (opencode-report-diagnostic
        (format "Failed to start `%s`: %s"
                command-string
                (error-message-string err)))
       nil))))

(defun opencode--emacs-ert-command ()
  "Return the batch Emacs command used to test with ERT."
  (append
   (list (concat invocation-directory invocation-name) "--batch")
   (cl-loop for path in (delete-dups (append (list default-directory) load-path nil))
            when (and (stringp path) (file-directory-p path))
            append (list "-L" (expand-file-name path)))
   (cl-loop for file in (directory-files-recursively default-directory
                                                     "\\(?:-test\\|-tests\\)\\.el\\'")
            append (list "-l" file))
   (list "-f" "ert-run-tests-batch-and-exit")))

(defun opencode-uv-pytest (_files)
  "Run pytest with uv and report diagnostics."
  (opencode-run-command-diagnostic
   '("uv" "run" "python3" "-m" "pytest")))

(defun opencode-emacs-ert (_files)
  "Run ERT and report diagnostics."
  (opencode-run-command-diagnostic
   (opencode--emacs-ert-command)))

(defun opencode--selection-change-hook (&optional _frame)
  "Hook to remove session from the alerted sessions list when it's visited.
Also prompts for pending questions or permissions if any."
  (when opencode-session-id
    (setf opencode-alerted-sessions
          (cl-delete-if (lambda (session)
                          (string= opencode-session-id
                                   (alist-get 'id session)))
                        opencode-alerted-sessions)
          opencode-last-session-buffer (current-buffer))
    ;; Permissions are higher-priority blockers than questions upstream.
    (cond
     (opencode-session-pending-permission
      (run-at-time 0 nil #'opencode-respond-permission))
     (opencode-session-pending-questions
      (let ((pending opencode-session-pending-questions))
        (setq opencode-session-pending-questions nil)
        (opencode--prompt-questions (car pending) (cdr pending)))))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

(defun opencode--handle-message (data)
  "Handle decoded message DATA from opencode server."
  (let* ((msg-type (intern (alist-get 'type data)))
         (properties (alist-get 'properties data)))
    (unless (or (memq msg-type '(file.watcher.updated server.heartbeat session.diff sync))
                (opencode--queue-message-while-bootstrapping data))
      (opencode-record--event data)
      (opencode--log-event "MESSAGE" data)
      (let-alist properties
        (cl-case msg-type
          (tui.toast.show (opencode--toast-show properties))
          (session.idle
           (let ((session-id .sessionID))
             (opencode-session--maybe-schedule-question-recover session-id 0)
             (opencode--maybe-run-files-finished-editing-hook session-id)
             (opencode-session--finish-idle
              session-id
              (lambda ()
                (opencode-api-session (session-id)
                    session
                  (let ((buffer (gethash session-id opencode-session-buffers)))
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (opencode--show-prompt)))
                    (unless (or
                             (not (buffer-live-p buffer))
                             (opencode--buffer-active-p buffer)
                             ;; don't show alert for subagent sessions
                             (alist-get 'parentID session))
                      (opencode--toast-show `((title . "OpenCode Finished")
                                              (message . ,(alist-get 'title session))
                                              (variant . "success")))
                      (push session opencode-alerted-sessions))))))))
          (session.status (pcase .status.type
                            ((or "busy" "idle")
                             (opencode-session--set-status .sessionID .status.type)
                             (opencode-session--maybe-schedule-question-recover
                              .sessionID 0.25)
                             (if (string= .status.type "busy")
                                 (opencode-session--schedule-status-poll .sessionID)
                               (opencode-session--finish-idle .sessionID)))
                            ("retry"
                             (opencode-api-session (.sessionID)
                                 session
                               (opencode--toast-show `((title . ,(concat "OpenCode: "
                                                                         (alist-get 'title session)))
                                                       (message . ,(format "%s\n\nRetry #%d"
                                                                           .status.message
                                                                           .status.attempt))
                                                       (variant . "warning")))))))
          ((session.created session.updated session.deleted)
           (when (eq 'session.deleted msg-type)
             (run-hook-with-args 'opencode-session-deleted-functions .info.id))
           (dolist (buffer (map-elt opencode--session-control-buffers .info.projectID))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode-sessions-redisplay))))
           (when-let (buffer (map-elt opencode-session-buffers .info.id))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (cl-case msg-type
                   (session.updated (rename-buffer (format "*OpenCode: %s*" .info.title) t))
                   (session.deleted (delete-process)))))))
          (session.error (opencode-session--display-error .sessionID .error.data.message))
          (message.part.updated
           (opencode--maybe-run-file-edited-hook .part)
           (opencode-session--update-part .part .delta .part.type))
          (message.part.delta
           (when (string= .field "text")
             (opencode-session--update-part properties .delta nil)))
          (message.updated
           (opencode--record-files-edited-this-turn .info)
           (opencode-session--message-updated .info))
          (session.next.shell.started
           (opencode-session--handle-shell-started
            .sessionID .callID .command))
          (session.next.shell.ended
           (opencode-session--handle-shell-ended
            .sessionID .callID .output))
          (permission.asked
           (opencode--permission-request
            .id .sessionID .permission
            .metadata
            (seq-into .patterns 'list)
            (seq-into .always 'list)
            .tool))
          ((permission.replied permission.rejected)
           (when-let (buffer (map-elt opencode-session-buffers .sessionID))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (fboundp 'opencode-session--clear-pending-permission)
                   (opencode-session--clear-pending-permission
                    (or .requestID .permissionID .id)))))))
          (server.instance.disposed
           (opencode-session--mark-stream-failed
            "OpenCode instance disposed" .directory))
          (question.asked
           (opencode--question-request .id .sessionID .questions .tool))
          ((question.replied question.rejected)
           (when-let (buffer (map-elt opencode-session-buffers .sessionID))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode-session--clear-pending-question
                  (or .requestID .questionID .id))))))
          (otherwise
           (opencode--log-event "WARNING" "unhandled message type")
           (opencode-record--autosave-recent
            "unhandled-message-type"
            (opencode-record--event-session-id data)))))
      (opencode-record--after-event data))))

(defun opencode--handle-global-event-data (raw-data)
  "Handle raw global event RAW-DATA from opencode server."
  (let ((ignored (or (not (stringp raw-data))
                      (string-empty-p raw-data)
                      (opencode--ignored-message-data-p raw-data)))
        (redacted (and (stringp raw-data)
                        (opencode--redact-tool-output-data raw-data))))
    (if ignored
        (opencode-record--raw-event redacted t)
      (condition-case err
          (let* ((data (json-read-from-string redacted))
                 (directory (alist-get 'directory data))
                 (payload (alist-get 'payload data)))
            (opencode-record--raw-event redacted nil data)
            (if (and directory payload)
                (let ((default-directory (opencode--normalize-directory directory))
                      (opencode-record--current-raw-event redacted)
                      (opencode-record--current-directory directory))
                  (opencode--handle-message payload))
              (unless (equal "server.heartbeat" (alist-get 'type payload))
                (opencode--log-event "WARNING GLOBAL EVENT" data)
                (opencode-record--autosave-recent "warning-global-event"))))
        (error
         (opencode-record--sse-parse-error redacted err)
         (opencode--log-event "WARNING GLOBAL EVENT PARSE" err)
         (opencode-record--autosave-recent "warning-global-event-parse"))))))

(defun opencode--handle-global-event (event)
  "Handle a global wrapper EVENT from opencode server."
  (opencode--handle-global-event-data (plz-event-source-event-data event)))

(defun opencode--subscribe-global-events ()
  "Subscribe to the global opencode event stream."
  (let ((event-stream
         (if opencode-use-fast-event-stream
             (opencode--event-stream)
           (plz-event-source:text/event-stream
            :events `((open . ,(lambda (event)
                                 (opencode--log-event "OPEN" event)))
                      (message . opencode--handle-global-event)
                      (close . opencode-disconnect))))))
    (setq opencode--event-subscription
          (plz-media-type-request
            'get (concat opencode-api-url "/global/event")
            :as `(media-types
                  ((text/event-stream . ,event-stream)))
            :headers (delq nil (list (opencode--auth-header)))
            :then 'opencode-disconnect
            :else 'opencode-disconnect)))
  (set-process-query-on-exit-flag opencode--event-subscription nil))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode-record--sse-lifecycle
   "disconnect"
   `((event . ,(and event (format "%s" event)))))
  (opencode--log-event "DISCONNECT" event)
  (when event
    (opencode-session--mark-stream-failed "OpenCode event stream disconnected"))
  (when (process-live-p opencode--event-subscription)
    (set-process-query-on-exit-flag opencode--event-subscription nil)
    (set-process-sentinel opencode--event-subscription nil)
    (kill-process opencode--event-subscription))
  (when (process-live-p opencode--process)
    (set-process-sentinel opencode--process nil)
    (kill-process opencode--process))
  (setq opencode-api-url nil
        opencode--event-subscription nil
        opencode--slash-commands-by-directory nil))

(defun opencode--fetch-agents ()
  "Fetch available agents from server and filter out hidden agents."
  (opencode-api-agents agents
    (setq opencode-agents
          (seq-filter (lambda (agent)
                        (opencode--json-falsy (alist-get 'hidden agent)))
                      agents))))

(cl-defun opencode-new-session (&key title callback)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one.
If CALLBACK is given, it will be called with the session after it is created."
  (interactive
   (when current-prefix-arg
     (list :title (read-string "Title: "))))
  (opencode-autoconnect
   (lambda ()
     (opencode--download-slash-commands default-directory)
     (opencode-api-create-session (if title
                                      `((title . ,title))
                                    (make-hash-table))
         session
       (opencode-open-session session :callback callback)))))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode-api-mcps mcps
    (let ((mcp (opencode--annotated-completion
                "MCP: "
                (cl-loop for mcp in mcps
                         for (mcp-name . mcp-info) = mcp
                         collect (list (symbol-name mcp-name)
                                       mcp-name
                                       (pcase (alist-get 'status mcp-info)
                                         ("connected" "🟢 connected")
                                         ("disabled" "🔴 disabled")))))))
      (pcase (map-nested-elt mcps `(,mcp status))
        ("connected" (opencode-api-disable-mcp (mcp)
                         _res
                       (message "Disabled %s" mcp)))
        ("disabled" (opencode-api-enable-mcp (mcp)
                        _res
                      (message "Enabled %s" mcp)))))))

(defun opencode-fork-session ()
  "Fork the current session from the message at point.
Creates a new session starting from the current user message.
If point is before the first prompt, creates a new session instead."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-fork-session (opencode-session-id)
            `((messageID . ,message-id))
            session
          (opencode-open-session-same-window session))
      ;; if before the first prompt just open a new session
      (opencode-new-session))))

(defun opencode-revert-message ()
  "Select a message to revert in the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-revert-message (opencode-session-id)
            `((messageID . ,message-id))
            result
          (message
           (if result
               "Reverted edits after message"
             "Failed to revert message")))
      (user-error "No user message at point"))))

(defun opencode-delete-message ()
  "Delete the message at point from the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-exchange (user-id assistant-id)
    (if (and user-id assistant-id)
        (opencode-api-delete-message (opencode-session-id assistant-id)
            assistant-result
          (unless assistant-result
            (error "Failed to delete assistant message"))
          (opencode-api-delete-message (opencode-session-id user-id)
              user-result
            (unless user-result
              (error "Failed to delete user message"))
            (opencode--delete-message-at-point)))
      (user-error "No message with assistant response at point"))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode-api-unrevert-all (opencode-session-id)
      result
    (message
     (if result
         "Restored all edits in session"
       "Failed to restore edits"))))

(defun opencode--download-slash-commands (directory)
  "Download slash commands for DIRECTORY."
  (setf directory (opencode--normalize-directory directory))
  (unless (assoc directory opencode--slash-commands-by-directory)
    (let ((default-directory directory))
      (opencode-api-commands commands
        (setf (alist-get directory opencode--slash-commands-by-directory
                         nil nil #'string=)
              commands)))))

(defun opencode--slash-commands-for-directory (directory)
  "Return slash commands for DIRECTORY, fetching them on cache miss."
  (setf directory (opencode--normalize-directory directory))
  (or (alist-get directory opencode--slash-commands-by-directory nil nil #'string=)
      (let* ((default-directory directory)
             (commands
              (plz 'get (concat opencode-api-url "/command")
                :as (lambda ()
                      (unless (string-empty-p (buffer-string))
                        (json-parse-buffer :array-type 'list
                                           :object-type 'alist)))
                :headers `(("Content-Type" . "application/json")
                           ,(cons "x-opencode-directory" default-directory)
                           ,(opencode--auth-header))
                :then 'sync)))
        (setf (alist-get directory opencode--slash-commands-by-directory
                         nil nil #'string=)
              commands)
        commands)))

(provide 'opencode)
;;; opencode.el ends here
