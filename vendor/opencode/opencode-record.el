;;; opencode-record.el --- Record and replay OpenCode frontend traces -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Optional JSONL recorder for debugging frontend event ordering and rendering.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'opencode-common)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)

(defvar opencode-api-url)
(defvar opencode-session-buffers)
(defvar opencode-session-id)
(defvar opencode-session-directory)
(defvar opencode-session-status)
(defvar opencode-session-pending-questions)
(defvar opencode-session-pending-permission)
(defvar opencode-assistant-messages)
(defvar opencode-rendered-message-ids)
(defvar opencode-part-type)
(defvar opencode-part-text)
(defvar opencode-part-sent)
(defvar opencode-part-replay-text)
(defvar opencode-part-message)
(defvar opencode-message-roles)
(defvar opencode-shell-echo)
(defvar opencode-session--interrupted-message-ids)
(defvar opencode-session--bootstrapping)
(defvar opencode-session--queued-events)
(defvar opencode-session--draining-queued-events)
(defvar opencode-session--stream-batch-timer)
(defvar opencode-session--stream-batch-type)
(defvar opencode-session--stream-batch-start)
(defvar opencode-session--stream-batch-strings)
(defvar opencode-session--idle-finalize-timer)
(defvar opencode-session--idle-finalize-callback)
(defvar opencode-session--suspect-reconcile-timer)
(defvar opencode-session--stream-message-states)
(defvar opencode--pending-question-tools)
(defvar opencode--completed-question-tools)
(defvar opencode--pending-permission-tools)
(defvar opencode-shell-calls)
(defvar opencode--tool-calls-displayed)

(declare-function opencode--handle-message "opencode" (data))
(declare-function opencode--normalize-directory "opencode-common" (directory))
(declare-function opencode-session-mode "opencode-sessions" ())
(declare-function opencode-session--flush-stream-batch "opencode-sessions" (&optional buffer))
(declare-function opencode-session--run-idle-finalize "opencode-sessions" (buffer))
(declare-function opencode-session--reconcile-active-provisional-messages "opencode-sessions" (messages))
(declare-function opencode-session--reconcile-pending-messages "opencode-sessions" (messages-by-id))
(declare-function opencode-session--reconcile-missed-history-messages "opencode-sessions" (messages))

(defvar-local opencode-record-replay-buffer nil
  "Non-nil when the current buffer is an OpenCode recording replay.")

(defun opencode-record--replay-input-sender (_proc _input)
  "Reject interactive input from recording replay buffers."
  (user-error "OpenCode recording replay buffers are read-only"))

(defcustom opencode-record-directory
  (expand-file-name
   "opencode/log/emacs-recordings"
   (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
  "Directory where OpenCode frontend recordings are written."
  :type 'directory
  :group 'opencode)

(defcustom opencode-record-tail-chars 4000
  "Number of trailing session-buffer characters to include in snapshots."
  :type 'integer
  :group 'opencode)

(defcustom opencode-record-max-string 200000
  "Maximum string length stored in a recording entry."
  :type 'integer
  :group 'opencode)

(defcustom opencode-record-snapshot-after-events t
  "Whether to record a compact UI snapshot after decoded events."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-record-recent-events-enabled t
  "Whether to keep recent OpenCode events in memory for later saving."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-record-recent-session-limit 5000
  "Maximum recent decoded events to retain per OpenCode session."
  :type 'integer
  :group 'opencode)

(defcustom opencode-record-recent-global-limit 20000
  "Maximum recent decoded events to retain across all OpenCode sessions."
  :type 'integer
  :group 'opencode)

(defcustom opencode-record-recent-session-byte-limit (* 10 1024 1024)
  "Approximate byte limit for recent events retained per session.
One oversized event may exceed this limit so the failure trigger is not lost."
  :type 'integer
  :group 'opencode)

(defcustom opencode-record-recent-global-byte-limit (* 50 1024 1024)
  "Approximate byte limit for recent events retained across all sessions.
One oversized event may exceed this limit so the failure trigger is not lost."
  :type 'integer
  :group 'opencode)

(defcustom opencode-record-recent-autosave-on-warning nil
  "Whether frontend warnings automatically save recent events."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-record-recent-autosave-min-interval 60
  "Minimum seconds between automatic recent-event saves."
  :type 'number
  :group 'opencode)

(defcustom opencode-record-recent-include-history t
  "Whether recent saves include a bounded persisted session-history snapshot.
OpenCode's event stream can occasionally miss the final deltas for a message
that is already complete in persisted history.  Including a small history tail
keeps recent recordings self-contained enough to replay those missed tails."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-record-recent-sse-diagnostics-enabled t
  "Whether recent saves retain SSE parser and connection diagnostics."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-record-recent-history-limit 50
  "Maximum number of persisted messages to include in recent saves."
  :type 'integer
  :group 'opencode)

(defvar opencode-record--file nil
  "Current OpenCode recording file, or nil when recording is disabled.")

(defvar opencode-record--seq 0
  "Sequence number for the current OpenCode recording.")

(defvar opencode-record--last-error nil
  "Last recorder write error, used to avoid repeated messages.")

(cl-defstruct (opencode-record--recent-queue
               (:constructor opencode-record--recent-queue-create))
  "FIFO queue for bounded recent OpenCode events."
  head
  tail
  (count 0)
  (bytes 0))

(cl-defstruct (opencode-record--recent-entry
               (:constructor opencode-record--recent-entry-create))
  "One retained recent OpenCode event."
  seq
  time
  type
  data
  raw
  directory
  session-id
  bytes)

(defvar opencode-record--recent-global-queue
  (opencode-record--recent-queue-create)
  "Recent decoded events across all OpenCode sessions.")

(defvar opencode-record--recent-session-queues
  (make-hash-table :test 'equal)
  "Mapping of session ids to recent decoded event queues.")

(defvar opencode-record--recent-seq 0
  "Sequence number for recent in-memory OpenCode events.")

(defvar opencode-record--recent-last-autosave-time nil
  "Time when recent events were last auto-saved, or nil.")

(defvar opencode-record--current-raw-event nil
  "Dynamically bound redacted raw SSE event for the current decoded event.")

(defvar opencode-record--current-directory nil
  "Dynamically bound project directory for the current decoded event.")

(defun opencode-record--active-p ()
  "Return non-nil when OpenCode recording is active."
  (and (stringp opencode-record--file)
       (not (string-empty-p opencode-record--file))))

(defun opencode-record--timestamp ()
  "Return an ISO-like timestamp suitable for files and JSONL entries."
  (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t))

(defun opencode-record--recent-queue-pop (queue)
  "Remove and return the oldest entry from recent event QUEUE."
  (when-let ((head (opencode-record--recent-queue-head queue)))
    (let ((entry (car head)))
      (setf (opencode-record--recent-queue-head queue) (cdr head)
            (opencode-record--recent-queue-count queue)
            (max 0 (1- (opencode-record--recent-queue-count queue)))
            (opencode-record--recent-queue-bytes queue)
            (max 0 (- (opencode-record--recent-queue-bytes queue)
                      (or (opencode-record--recent-entry-bytes entry) 0))))
      (unless (opencode-record--recent-queue-head queue)
        (setf (opencode-record--recent-queue-tail queue) nil))
      entry)))

(defun opencode-record--recent-queue-push (queue entry limit byte-limit)
  "Append ENTRY to QUEUE bounded by LIMIT and BYTE-LIMIT."
  (when (and (integerp limit) (> limit 0))
    (let ((node (list entry)))
      (if-let ((tail (opencode-record--recent-queue-tail queue)))
          (setcdr tail node)
        (setf (opencode-record--recent-queue-head queue) node))
      (setf (opencode-record--recent-queue-tail queue) node
            (opencode-record--recent-queue-count queue)
            (1+ (opencode-record--recent-queue-count queue))
            (opencode-record--recent-queue-bytes queue)
            (+ (opencode-record--recent-queue-bytes queue)
               (or (opencode-record--recent-entry-bytes entry) 0)))
      (while (> (opencode-record--recent-queue-count queue) limit)
        (opencode-record--recent-queue-pop queue))
      (while (and (integerp byte-limit)
                  (> byte-limit 0)
                  (> (opencode-record--recent-queue-bytes queue) byte-limit)
                  (> (opencode-record--recent-queue-count queue) 1))
        (opencode-record--recent-queue-pop queue))
      queue)))

(defun opencode-record--recent-queue-entries (queue)
  "Return recent event entries in QUEUE from oldest to newest."
  (let (entries)
    (let ((node (opencode-record--recent-queue-head queue)))
      (while node
        (push (car node) entries)
        (setq node (cdr node))))
    (nreverse entries)))

(defun opencode-record--value-byte-size (value)
  "Return an approximate in-memory byte size for VALUE."
  (cond
   ((stringp value) (string-bytes value))
   ((or (numberp value) (symbolp value)) (length (format "%s" value)))
   ((vectorp value)
    (cl-loop for item across value sum (opencode-record--value-byte-size item)))
   ((hash-table-p value)
    (let ((bytes 0))
      (maphash (lambda (key val)
                 (setq bytes (+ bytes
                                (opencode-record--value-byte-size key)
                                (opencode-record--value-byte-size val))))
               value)
      bytes))
   ((consp value)
    (+ (opencode-record--value-byte-size (car value))
       (opencode-record--value-byte-size (cdr value))))
   (t 0)))

(defun opencode-record--event-byte-size (data raw-data)
  "Return an approximate byte size for decoded DATA and RAW-DATA."
  (if (stringp raw-data)
      (string-bytes raw-data)
    (opencode-record--value-byte-size data)))

(defun opencode-record--event-session-id (data)
  "Return the OpenCode session id associated with decoded event DATA."
  (let ((properties (alist-get 'properties data)))
    (or (alist-get 'sessionID properties)
        (map-nested-elt properties '(part sessionID))
        (map-nested-elt properties '(info sessionID))
        (map-nested-elt properties '(info id))
        (map-nested-elt properties '(session sessionID)))))

(defun opencode-record--directory-for-session (session-id)
  "Return the best known directory for SESSION-ID."
  (or opencode-record--current-directory
      (when (and (stringp session-id)
                 (boundp 'opencode-session-buffers))
        (when-let ((buffer (gethash session-id opencode-session-buffers)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (and default-directory
                   (ignore-errors
                     (opencode--normalize-directory default-directory)))))))))

(defun opencode-record--recent-session-queue (session-id)
  "Return the recent event queue for SESSION-ID, creating it if needed."
  (or (gethash session-id opencode-record--recent-session-queues)
      (puthash session-id
               (opencode-record--recent-queue-create)
               opencode-record--recent-session-queues)))

(defun opencode-record--remember-recent-entry (type data &optional raw-data
                                                    session-id directory)
  "Remember recent entry TYPE with DATA, RAW-DATA, SESSION-ID, and DIRECTORY."
  (when opencode-record-recent-events-enabled
    (let* ((session-id (or session-id (opencode-record--event-session-id data)))
           (directory (or directory
                          (opencode-record--directory-for-session session-id)))
           (entry (opencode-record--recent-entry-create
                   :seq (cl-incf opencode-record--recent-seq)
                   :time (format-time-string "%FT%TZ" (current-time) t)
                   :type type
                   :data data
                   :raw raw-data
                   :directory directory
                   :session-id session-id
                   :bytes (opencode-record--event-byte-size
                           data raw-data))))
      (opencode-record--recent-queue-push
       opencode-record--recent-global-queue entry
       opencode-record-recent-global-limit
       opencode-record-recent-global-byte-limit)
      (when (and (stringp session-id) (not (string-empty-p session-id)))
        (opencode-record--recent-queue-push
         (opencode-record--recent-session-queue session-id) entry
         opencode-record-recent-session-limit
         opencode-record-recent-session-byte-limit))
      entry)))

(defun opencode-record--remember-event (type data)
  "Remember decoded event DATA of recording TYPE in recent event queues."
  (opencode-record--remember-recent-entry
   type data opencode-record--current-raw-event))

(defun opencode-record--remember-sse-diagnostic (type data &optional raw-data
                                                      decoded-wrapper)
  "Remember SSE diagnostic TYPE with DATA, RAW-DATA, and DECODED-WRAPPER."
  (when opencode-record-recent-sse-diagnostics-enabled
    (let* ((payload (alist-get 'payload decoded-wrapper))
           (session-id (and payload (opencode-record--event-session-id payload)))
           (directory (alist-get 'directory decoded-wrapper)))
      (opencode-record--remember-recent-entry
       type data raw-data session-id directory))))

(defun opencode-record--recent-events (&optional session-id)
  "Return retained recent events for SESSION-ID, or global events when nil."
  (opencode-record--recent-queue-entries
   (if (and (stringp session-id) (not (string-empty-p session-id)))
       (or (gethash session-id opencode-record--recent-session-queues)
           (opencode-record--recent-queue-create))
     opencode-record--recent-global-queue)))

;;;###autoload
(defun opencode-record-clear-recent-events ()
  "Clear in-memory recent OpenCode events."
  (interactive)
  (setq opencode-record--recent-global-queue
        (opencode-record--recent-queue-create)
        opencode-record--recent-session-queues
        (make-hash-table :test 'equal)
        opencode-record--recent-seq 0
        opencode-record--recent-last-autosave-time nil)
  (message "OpenCode recent events cleared"))

(defun opencode-record--string (value)
  "Return VALUE as a plain, bounded string."
  (let ((text (substring-no-properties (format "%s" value))))
    (if (and (integerp opencode-record-max-string)
             (> (length text) opencode-record-max-string))
        (concat (substring text 0 opencode-record-max-string)
                (format "\n[... truncated %d chars ...]"
                        (- (length text) opencode-record-max-string)))
      text)))

(defun opencode-record--sanitize (value)
  "Return VALUE in a JSON-encodable, bounded shape."
  (cond
   ((stringp value) (opencode-record--string value))
   ((or (numberp value) (eq value t) (null value) (eq value :false)) value)
   ((symbolp value) (symbol-name value))
   ((vectorp value) (mapcar #'opencode-record--sanitize value))
   ((hash-table-p value)
    (let (items)
      (maphash (lambda (key val)
                 (push (cons (opencode-record--string key)
                             (opencode-record--sanitize val))
                       items))
               value)
      (nreverse items)))
   ((consp value)
    (cons (opencode-record--sanitize (car value))
          (opencode-record--sanitize (cdr value))))
   (t (opencode-record--string value))))

(defun opencode-record--session-context (&optional buffer)
  "Return session context for BUFFER or the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      `((buffer . ,(buffer-name buffer))
        (directory . ,(and default-directory
                           (ignore-errors
                             (opencode--normalize-directory default-directory))))
        (sessionID . ,(and (boundp 'opencode-session-id)
                           opencode-session-id))
        (status . ,(and (boundp 'opencode-session-status)
                        opencode-session-status))))))

(defun opencode-record--hash-count (value)
  "Return hash-table count for VALUE, or nil."
  (when (hash-table-p value)
    (hash-table-count value)))

(defun opencode-record--snapshot-data (&optional label buffer)
  "Return a compact UI snapshot for LABEL and BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (tail-start (max (point-min)
                                (- (point-max) opencode-record-tail-chars)))
               (tail (buffer-substring-no-properties tail-start (point-max)))
               (question-id (and (boundp 'opencode-session-pending-questions)
                                 (car-safe opencode-session-pending-questions)))
               (permission-id (and (boundp 'opencode-session-pending-permission)
                                   (plist-get opencode-session-pending-permission
                                              :id))))
          `((label . ,label)
            (session . ,(opencode-record--session-context buffer))
            (pointMax . ,(point-max))
            (hash . ,(secure-hash 'sha256 text))
            (tail . ,tail)
            (activeAssistantIDs . ,(and (boundp 'opencode-assistant-messages)
                                        (mapcar #'car opencode-assistant-messages)))
            (renderedCount . ,(and (boundp 'opencode-rendered-message-ids)
                                   (opencode-record--hash-count
                                    opencode-rendered-message-ids)))
            (pendingQuestionID . ,question-id)
            (pendingPermissionID . ,permission-id)
            (partTypeCount . ,(and (boundp 'opencode-part-type)
                                   (opencode-record--hash-count opencode-part-type)))
            (partTextCount . ,(and (boundp 'opencode-part-text)
                                   (opencode-record--hash-count opencode-part-text)))
            (partSentCount . ,(and (boundp 'opencode-part-sent)
                                   (opencode-record--hash-count opencode-part-sent)))
            (queuedEventCount . ,(and (boundp 'opencode-session--queued-events)
                                      (length opencode-session--queued-events)))))))))

(defun opencode-record--write (type &optional data)
  "Append recording entry TYPE with DATA when recording is active."
  (when (opencode-record--active-p)
    (condition-case err
        (let ((json-encoding-pretty-print nil)
              (entry `((time . ,(format-time-string "%FT%TZ" (current-time) t))
                       (seq . ,(cl-incf opencode-record--seq))
                       (type . ,type)
                       (data . ,(opencode-record--sanitize data)))))
          (with-temp-buffer
            (insert (json-encode entry) "\n")
            (write-region (point-min) (point-max) opencode-record--file t
                          'silent)))
      (error
       (unless (equal err opencode-record--last-error)
         (setq opencode-record--last-error err)
         (message "OpenCode recorder error: %s" err))))))

(defun opencode-record--snapshot (&optional label buffer)
  "Record a UI snapshot for LABEL and BUFFER."
  (when (opencode-record--active-p)
    (when-let ((snapshot (opencode-record--snapshot-data label buffer)))
      (opencode-record--write "ui.snapshot" snapshot))))

(defun opencode-record--api-request (method path data)
  "Record API request METHOD PATH with DATA."
  (opencode-record--write
   "api.request"
   `((method . ,(symbol-name method))
     (path . ,path)
     (body . ,data)
     (session . ,(opencode-record--session-context)))))

(defun opencode-record--api-response (method path response)
  "Record API RESPONSE for METHOD PATH."
  (opencode-record--write
   "api.response"
   `((method . ,(symbol-name method))
     (path . ,path)
     (response . ,response)
     (session . ,(opencode-record--session-context)))))

(defun opencode-record--api-error (method path response)
  "Record API error RESPONSE for METHOD PATH."
  (opencode-record--write
   "api.error"
   `((method . ,(symbol-name method))
     (path . ,path)
     (error . ,(opencode-record--string response))
     (session . ,(opencode-record--session-context)))))

(defun opencode-record--raw-event (raw-data ignored &optional decoded-wrapper)
  "Record raw event RAW-DATA and whether it was IGNORED.
DECODED-WRAPPER is the decoded global event wrapper when available."
  (opencode-record--write
   (if ignored "sse.ignored" "sse.event")
   `((raw . ,raw-data)))
  (when ignored
    (opencode-record--remember-sse-diagnostic
     "sse.ignored"
     `((ignored . t)
       (rawBytes . ,(and (stringp raw-data) (string-bytes raw-data))))
     raw-data decoded-wrapper)))

(defun opencode-record--sse-parse-error (raw-data error)
  "Record that RAW-DATA failed to parse with ERROR."
  (opencode-record--write
   "sse.parse-error"
   `((raw . ,raw-data)
     (error . ,(opencode-record--string error))))
  (opencode-record--remember-sse-diagnostic
   "sse.parse-error"
   `((error . ,(opencode-record--string error))
     (rawBytes . ,(and (stringp raw-data) (string-bytes raw-data))))
   raw-data))

(defun opencode-record--sse-lifecycle (event &optional data)
  "Record SSE lifecycle EVENT with optional DATA."
  (opencode-record--write "sse.lifecycle" `((event . ,event) ,@data))
  (opencode-record--remember-sse-diagnostic
   "sse.lifecycle" `((event . ,event) ,@data)))

(defun opencode-record--event (data)
  "Record decoded event DATA."
  (opencode-record--remember-event "event.decoded" data)
  (opencode-record--write "event.decoded" data))

(defun opencode-record--queued-event (data)
  "Record decoded event DATA queued while bootstrapping."
  (opencode-record--remember-event "event.queued" data)
  (opencode-record--write "event.queued" data))

(defun opencode-record--after-event (data)
  "Record compact state after decoded event DATA."
  (when opencode-record-snapshot-after-events
    (let* ((properties (alist-get 'properties data))
           (session-id (or (alist-get 'sessionID properties)
                           (map-nested-elt properties '(part sessionID))
                           (map-nested-elt properties '(info sessionID))))
           (buffer (and session-id
                        (boundp 'opencode-session-buffers)
                        (gethash session-id opencode-session-buffers))))
      (when (buffer-live-p buffer)
        (opencode-record--snapshot "after-event" buffer)))))

(defun opencode-record--send-input (string)
  "Record outbound user STRING."
  (opencode-record--write
   "send.input"
   `((input . ,string)
     (session . ,(opencode-record--session-context)))))

(defun opencode-record--safe-label (label)
  "Return LABEL made safe for recording file names."
  (when (and (stringp label) (not (string-empty-p label)))
    (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" label)))

(defun opencode-record--recent-session-buffer (session-id)
  "Return the live session buffer for SESSION-ID, or nil."
  (when (and (stringp session-id)
             (boundp 'opencode-session-buffers))
    (when-let ((buffer (gethash session-id opencode-session-buffers)))
      (and (buffer-live-p buffer) buffer))))

(defun opencode-record--recent-session-context (session-id entries)
  "Return session context for SESSION-ID and recent ENTRIES."
  (if-let ((buffer (opencode-record--recent-session-buffer session-id)))
      (opencode-record--session-context buffer)
    (let ((directory (cl-loop for entry in entries
                              for dir = (opencode-record--recent-entry-directory
                                         entry)
                              when dir return dir)))
      `((buffer . nil)
        (directory . ,directory)
        (sessionID . ,session-id)
        (status . nil)))))

(defun opencode-record--take-last (items limit)
  "Return the last LIMIT items from ITEMS."
  (if (and (integerp limit) (> limit 0))
      (let ((extra (- (length items) limit)))
        (if (> extra 0)
            (nthcdr extra items)
          items))
    items))

(defun opencode-record--auth-header ()
  "Return the OpenCode auth header when available."
  (when (fboundp 'opencode--auth-header)
    (funcall #'opencode--auth-header)))

(defun opencode-record--session-history-sync (session-id directory)
  "Synchronously fetch persisted history for SESSION-ID in DIRECTORY."
  (when (and opencode-api-url
             (stringp session-id)
             (not (string-empty-p session-id))
             (stringp directory)
             (not (string-empty-p directory)))
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            (delq nil
                  `(("Content-Type" . "application/json")
                    ("x-opencode-directory" . ,directory)
                    ,(opencode-record--auth-header))))
           (url (concat (string-remove-suffix "/" opencode-api-url)
                        "/session/"
                        (url-hexify-string session-id)
                        "/message"))
           (buffer (url-retrieve-synchronously url t t 5)))
      (when (buffer-live-p buffer)
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (when (search-forward "\n\n" nil t)
                (unless (eobp)
                  (json-parse-buffer :array-type 'list :object-type 'alist))))
          (kill-buffer buffer))))))

(defun opencode-record--recent-history-snapshot (session-id session)
  "Return a bounded persisted-history snapshot for SESSION-ID and SESSION."
  (when (and opencode-record-recent-include-history
             (stringp session-id)
             (not (string-empty-p session-id)))
    (let* ((directory (alist-get 'directory session))
           (messages (ignore-errors
                       (opencode-record--session-history-sync
                        session-id directory))))
      (when (consp messages)
        (let ((tail (opencode-record--take-last
                     messages opencode-record-recent-history-limit)))
          `((sessionID . ,session-id)
            (directory . ,directory)
            (messageCount . ,(length messages))
            (tailCount . ,(length tail))
            (messages . ,(opencode-record--sanitize tail))))))))

(defun opencode-record--recent-entry-json (entry)
  "Return replay-compatible JSONL alist for recent ENTRY."
  `((time . ,(opencode-record--recent-entry-time entry))
    (seq . ,(opencode-record--recent-entry-seq entry))
    (type . ,(opencode-record--recent-entry-type entry))
    (data . ,(opencode-record--sanitize
              (opencode-record--recent-entry-data entry)))
    ,@(when-let ((raw (opencode-record--recent-entry-raw entry)))
        `((raw . ,(opencode-record--string raw))))
    (recent . ((sessionID . ,(opencode-record--recent-entry-session-id entry))
               (directory . ,(opencode-record--recent-entry-directory entry))
               (bytes . ,(opencode-record--recent-entry-bytes entry))))))

(defun opencode-record--write-json-entry (entry)
  "Insert JSONL ENTRY into the current buffer."
  (let ((json-encoding-pretty-print nil))
    (insert (json-encode entry) "\n")))

;;;###autoload
(defun opencode-record-save-recent-events (&optional session-id label)
  "Save recent OpenCode events for SESSION-ID to a JSONL recording.
When SESSION-ID is nil, save the global cross-session recent event buffer.
LABEL is included in the output filename when non-empty."
  (interactive
   (let ((current-session (and (boundp 'opencode-session-id)
                               opencode-session-id)))
     (list (if current-prefix-arg
               (let ((input (read-string
                             "Session ID (empty for global): "
                             current-session)))
                 (unless (string-empty-p input) input))
             current-session)
           nil)))
  (let* ((entries (opencode-record--recent-events session-id))
         (scope (if (and (stringp session-id) (not (string-empty-p session-id)))
                    session-id
                  "global"))
         (safe-label (opencode-record--safe-label label))
         (safe-scope (opencode-record--safe-label scope))
         (file (expand-file-name
                (concat (opencode-record--timestamp)
                        "-recent-"
                        safe-scope
                        (when safe-label (concat "-" safe-label))
                        ".jsonl")
                opencode-record-directory))
         (session (opencode-record--recent-session-context session-id entries))
         (history-snapshot
          (opencode-record--recent-history-snapshot session-id session))
         (last-seq (and entries
                        (opencode-record--recent-entry-seq (car (last entries)))))
         (next-seq (1+ (or last-seq 0))))
    (unless entries
      (user-error "No recent OpenCode events retained for %s" scope))
    (make-directory opencode-record-directory t)
    (with-temp-file file
      (opencode-record--write-json-entry
       `((time . ,(format-time-string "%FT%TZ" (current-time) t))
         (seq . 0)
         (type . "record.start")
         (data . ((label . ,label)
                  (recent . t)
                  (scope . ,scope)
                  (session . ,session)
                  (eventCount . ,(length entries))
                  (emacsPID . ,(emacs-pid))
                  (apiURL . ,(and (boundp 'opencode-api-url)
                                  opencode-api-url))))))
      (dolist (entry entries)
        (opencode-record--write-json-entry
         (opencode-record--recent-entry-json entry)))
      (when history-snapshot
        (opencode-record--write-json-entry
         `((time . ,(format-time-string "%FT%TZ" (current-time) t))
           (seq . ,next-seq)
           (type . "history.snapshot")
           (data . ,history-snapshot)))
        (setq next-seq (1+ next-seq)))
      (opencode-record--write-json-entry
       `((time . ,(format-time-string "%FT%TZ" (current-time) t))
         (seq . ,next-seq)
         (type . "record.stop")
         (data . ((path . ,file))))))
    (with-temp-buffer
      (opencode-record--write-json-entry
       `((time . ,(format-time-string "%FT%TZ" (current-time) t))
         (path . ,file)
         (scope . ,scope)
         (eventCount . ,(length entries))))
      (write-region (point-min) (point-max)
                    (expand-file-name "latest-recent.json"
                                      opencode-record-directory)
                    nil 'silent))
    (message "OpenCode recent events saved: %s" file)
    file))

;;;###autoload
(defun opencode-record-recent-status ()
  "Show the in-memory recent OpenCode event recorder status."
  (interactive)
  (message "OpenCode recent events: global=%d/%d events, %.1f MiB, sessions=%d"
           (opencode-record--recent-queue-count
            opencode-record--recent-global-queue)
           opencode-record-recent-global-limit
           (/ (float (opencode-record--recent-queue-bytes
                      opencode-record--recent-global-queue))
              1048576.0)
           (hash-table-count opencode-record--recent-session-queues)))

(defun opencode-record--autosave-recent (label &optional session-id)
  "Auto-save recent events with LABEL for SESSION-ID when enabled."
  (when opencode-record-recent-autosave-on-warning
    (let ((now (float-time)))
      (when (or (not opencode-record--recent-last-autosave-time)
                (>= (- now opencode-record--recent-last-autosave-time)
                    opencode-record-recent-autosave-min-interval))
        (setq opencode-record--recent-last-autosave-time now)
        (condition-case err
            (opencode-record-save-recent-events session-id label)
          (error
           (message "OpenCode recent event autosave failed: %s" err)))))))

;;;###autoload
(defun opencode-record-start (&optional label)
  "Start recording OpenCode frontend events with optional LABEL."
  (interactive "sRecording label: ")
  (make-directory opencode-record-directory t)
  (let* ((session-id (and (boundp 'opencode-session-id)
                          opencode-session-id))
         (safe-label (when (and (stringp label)
                                (not (string-empty-p label)))
                       (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" label)))
         (file (expand-file-name
                (concat (opencode-record--timestamp)
                        (when session-id (concat "-" session-id))
                        (when safe-label (concat "-" safe-label))
                        ".jsonl")
                opencode-record-directory)))
    (setq opencode-record--file file
          opencode-record--seq 0
          opencode-record--last-error nil)
    (opencode-record--write
     "record.start"
     `((label . ,label)
       (session . ,(opencode-record--session-context))
       (emacsPID . ,(emacs-pid))
       (apiURL . ,(and (boundp 'opencode-api-url) opencode-api-url))))
    (opencode-record--snapshot "start")
    (with-temp-buffer
      (insert (json-encode `((time . ,(format-time-string "%FT%TZ" (current-time) t))
                             (path . ,file)))
              "\n")
      (write-region (point-min) (point-max)
                    (expand-file-name "latest.json" opencode-record-directory)
                    nil 'silent))
    (message "OpenCode recording started: %s" file)
    file))

;;;###autoload
(defun opencode-record-stop ()
  "Stop the active OpenCode frontend recording."
  (interactive)
  (unless (opencode-record--active-p)
    (user-error "OpenCode recording is not active"))
  (let ((file opencode-record--file))
    (opencode-record--snapshot "stop")
    (opencode-record--write "record.stop" `((path . ,file)))
    (setq opencode-record--file nil)
    (message "OpenCode recording stopped: %s" file)
    file))

;;;###autoload
(defun opencode-record-status ()
  "Show the active OpenCode frontend recording status."
  (interactive)
  (if (opencode-record--active-p)
      (message "OpenCode recording active: %s (%d entries)"
               opencode-record--file opencode-record--seq)
    (message "OpenCode recording is not active")))

(defun opencode-record--read-events (file)
  "Read JSONL recording FILE and return decoded events."
  (with-temp-buffer
    (insert-file-contents file)
    (let (events)
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (unless (string-empty-p line)
            (push (json-parse-string line :array-type 'list
                                     :object-type 'alist)
                  events)))
        (forward-line 1))
      (nreverse events))))

(defun opencode-record--event-type (entry)
  "Return recording ENTRY type."
  (alist-get 'type entry nil nil #'string=))

(defun opencode-record--event-data (entry)
  "Return recording ENTRY data."
  (alist-get 'data entry nil nil #'string=))

(defun opencode-record--safe-replay-event-p (event)
  "Return non-nil when EVENT can be replayed without backend API calls."
  (member (alist-get 'type event nil nil #'string=)
          '("message.updated" "message.part.updated" "message.part.delta"
            "message.part.removed" "session.status" "session.error"
            "session.next.shell.started" "session.next.shell.ended")))

(defun opencode-record--first-session (events)
  "Return first session context found in EVENTS."
  (or (cl-loop for entry in events
               for data = (opencode-record--event-data entry)
               for session = (alist-get 'session data nil nil #'string=)
               when session return session)
      '((sessionID . "opencode-record-replay")
        (directory . nil)
        (status . "idle"))))

(defun opencode-record--replay-history-snapshot (snapshot)
  "Apply persisted-history SNAPSHOT to the current replay buffer."
  (when-let ((messages (or (alist-get 'messages snapshot)
                           (alist-get "messages" snapshot nil nil #'string=))))
    ;; A recent recording can contain a degraded event stream plus a bounded
    ;; persisted history tail.  Rebuilding the visible replay from the snapshot
    ;; is safer than trying to stitch partial stream text into persisted text:
    ;; it preserves message boundaries and avoids false substring repairs.
    (opencode-record--replay-complete-history-snapshot messages)))

(defun opencode-record--complete-history-snapshot-p (snapshot messages)
  "Return non-nil when SNAPSHOT contains all persisted MESSAGES."
  (let ((message-count (or (alist-get 'messageCount snapshot)
                           (alist-get "messageCount" snapshot nil nil #'string=)))
        (tail-count (or (alist-get 'tailCount snapshot)
                        (alist-get "tailCount" snapshot nil nil #'string=))))
    (and (integerp message-count)
         (integerp tail-count)
         (= message-count tail-count)
         (= message-count (length messages)))))

(defun opencode-record--reset-replay-render-state ()
  "Clear replay buffer text and reducer state before snapshot rebuild."
  (when (fboundp 'opencode-session--cancel-stream-batch-timer)
    (opencode-session--cancel-stream-batch-timer))
  (when (fboundp 'opencode-session--cancel-live-render-timer)
    (opencode-session--cancel-live-render-timer))
  (when (fboundp 'opencode-session--cancel-idle-finalize-timer)
    (opencode-session--cancel-idle-finalize-timer))
  (when (fboundp 'opencode-session--cancel-suspect-reconcile-timer)
    (opencode-session--cancel-suspect-reconcile-timer))
  (when (fboundp 'opencode-session--cancel-stream-quiet-timer)
    (opencode-session--cancel-stream-quiet-timer))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq opencode-part-type (make-hash-table :test 'equal)
        opencode-part-text (make-hash-table :test 'equal)
        opencode-part-sent (make-hash-table :test 'equal)
        opencode-part-replay-text (make-hash-table :test 'equal)
        opencode-part-region-start (make-hash-table :test 'equal)
        opencode-part-region-end (make-hash-table :test 'equal)
        opencode-part-message (make-hash-table :test 'equal)
        opencode-message-roles (make-hash-table :test 'equal)
        opencode-shell-echo (make-hash-table :test 'equal)
        opencode-assistant-messages nil
        opencode-rendered-message-ids (make-hash-table :test 'equal)
        opencode-session--stream-batch-timer nil
        opencode-session--stream-batch-type nil
        opencode-session--stream-batch-start nil
        opencode-session--stream-batch-strings nil
        opencode-session--live-render-timer nil
        opencode-session--live-render-type nil
        opencode-session--live-render-start nil
        opencode-session--idle-finalize-timer nil
        opencode-session--idle-finalize-callback nil
        opencode-session--suspect-reconcile-timer nil
        opencode-session--stream-quiet-timer nil
        opencode-session--stream-message-states (make-hash-table :test 'equal)
        opencode-shell-calls (make-hash-table :test 'equal)
        opencode--tool-calls-displayed (make-hash-table :test 'equal)))

(defun opencode-record--open-message-p (message)
  "Return non-nil when persisted MESSAGE still represents active work."
  (and (equal "assistant" (map-nested-elt message '(info role)))
       (or (not (map-nested-elt message '(info time completed)))
           (seq-some (lambda (part)
                       (and (equal "tool" (alist-get 'type part nil nil #'string=))
                            (member (map-nested-elt part '(state status))
                                    '("pending" "running"))))
                     (alist-get 'parts message)))))

(defun opencode-record--replay-complete-history-snapshot (messages)
  "Render complete persisted MESSAGES into the current replay buffer."
  (opencode-record--reset-replay-render-state)
  (dolist (message messages)
    (let-alist (alist-get 'info message)
      (pcase .role
        ("user" (opencode--replay-user-request message))
        ("assistant"
         (opencode-session--render-complete-assistant-message message)))))
  (if (seq-some #'opencode-record--open-message-p messages)
      (setq opencode-session-status "busy")
    (setq opencode-session-status "idle")
    (opencode--show-prompt)))

(defun opencode-record--stream-part-texts (messages)
  "Return a table of current streamed text for text parts in MESSAGES."
  (let ((parts (make-hash-table :test 'equal)))
    (dolist (message messages)
      (dolist (part (alist-get 'parts message))
        (let ((part-id (alist-get 'id part))
              (type (alist-get 'type part nil nil #'string=)))
          (when (and (equal type "text") part-id)
            (puthash part-id (gethash part-id opencode-part-text) parts)))))
    parts))

(defun opencode-record--message-completed-assistant-p (message)
  "Return non-nil when MESSAGE is a completed assistant message."
  (and (equal "assistant" (map-nested-elt message '(info role)))
       (map-nested-elt message '(info time completed))))

(defun opencode-record--text-tail-snippet (text)
  "Return a stable tail snippet for persisted TEXT."
  (let ((trimmed (string-trim (or text ""))))
    (when (>= (length trimmed) 12)
      (substring trimmed (max 0 (- (length trimmed) 120))))))

(defun opencode-record--message-text-visible-p (message)
  "Return non-nil when MESSAGE text tail is visible in the current buffer."
  (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))
        (snippets nil))
    (dolist (part (alist-get 'parts message))
      (when (equal "text" (alist-get 'type part nil nil #'string=))
        (when-let ((snippet (opencode-record--text-tail-snippet
                             (alist-get 'text part))))
          (push snippet snippets))))
    (or (null snippets)
        (seq-every-p (lambda (snippet)
                       (string-match-p (regexp-quote snippet) buffer-text))
                     snippets))))

(defun opencode-record--replace-visible-partial-message (message stream-part-texts)
  "Replace visible streamed partial text for MESSAGE using STREAM-PART-TEXTS."
  (catch 'replaced
    (dolist (part (let ((parts (alist-get 'parts message)))
                    (cond
                     ((vectorp parts) (append parts nil))
                     ((listp parts) parts)
                     (parts (list parts)))))
      (let* ((part-id (alist-get 'id part))
             (type (alist-get 'type part nil nil #'string=))
             (stored (alist-get 'text part))
             (streamed (and part-id (gethash part-id stream-part-texts))))
        (when (and (equal type "text")
                   (stringp stored)
                   (stringp streamed)
                   (not (string-empty-p streamed))
                   (> (length stored) (length streamed))
                   (string-prefix-p streamed stored))
          (save-excursion
            (let ((start-marker (gethash part-id opencode-part-region-start))
                  (end-marker (gethash part-id opencode-part-region-end)))
              (when (and (markerp start-marker)
                         (markerp end-marker)
                         (marker-buffer start-marker)
                         (eq (marker-buffer start-marker) (current-buffer))
                         (eq (marker-buffer end-marker) (current-buffer))
                         (<= (marker-position start-marker)
                             (marker-position end-marker)))
                (let* ((start (marker-position start-marker))
                       (end (marker-position end-marker))
                       (visible (buffer-substring-no-properties start end))
                       (rendered-streamed
                        (opencode--render-markdown (string-trim streamed)))
                       (rendered (opencode--render-markdown
                                  (string-trim stored))))
                  (when (or (string= visible streamed)
                            (string= visible rendered-streamed)
                            (string-prefix-p visible rendered)
                            (string-prefix-p rendered-streamed rendered))
                    (let ((inhibit-read-only t))
                      (delete-region start end)
                      (goto-char start)
                      (insert rendered "\n\n"))
                    (puthash part-id type opencode-part-type)
                    (puthash part-id (map-nested-elt message '(info id))
                             opencode-part-message)
                    (puthash part-id stored opencode-part-text)
                    (puthash part-id stored opencode-part-replay-text)
                    (puthash part-id (length stored) opencode-part-sent)
                    (puthash part-id (copy-marker start) opencode-part-region-start)
                    (puthash part-id (copy-marker (+ start (length rendered)) t)
                             opencode-part-region-end)
                    (throw 'replaced t)))))))))
    nil))

(defun opencode-record--replay-missing-history-tail (messages stream-part-texts)
  "Render completed history tails missing from replayed MESSAGES."
  (dolist (message messages)
    (let ((message-id (map-nested-elt message '(info id))))
      (when (and message-id
                 (opencode-record--message-completed-assistant-p message)
                 (gethash message-id opencode-rendered-message-ids)
                 (not (opencode-record--message-text-visible-p message)))
        (unless (opencode-record--replace-visible-partial-message
                 message stream-part-texts)
          (remhash message-id opencode-rendered-message-ids)
          (opencode-session--render-complete-assistant-message message))
        (puthash message-id t opencode-rendered-message-ids)
        (setf opencode-assistant-messages
              (assoc-delete-all message-id opencode-assistant-messages))))))

(defun opencode-record--init-replay-buffer (session-id directory)
  "Initialize current buffer as a replay session for SESSION-ID in DIRECTORY."
  (setq default-directory (file-name-as-directory
                           (or (and (stringp directory) directory)
                               temporary-file-directory)))
  (when (fboundp 'opencode-session-mode)
    (opencode-session-mode))
  (setq-local comint-input-sender #'opencode-record--replay-input-sender)
  (setq opencode-record-replay-buffer t
        buffer-read-only nil
        opencode-session-id session-id
        opencode-session-directory default-directory
        opencode-session-status "idle"
        opencode-session-pending-questions nil
        opencode-session-pending-permission nil
        opencode-part-type (make-hash-table :test 'equal)
        opencode-part-text (make-hash-table :test 'equal)
        opencode-part-sent (make-hash-table :test 'equal)
        opencode-part-replay-text (make-hash-table :test 'equal)
        opencode-part-region-start (make-hash-table :test 'equal)
        opencode-part-region-end (make-hash-table :test 'equal)
        opencode-part-message (make-hash-table :test 'equal)
        opencode-message-roles (make-hash-table :test 'equal)
        opencode-shell-echo (make-hash-table :test 'equal)
        opencode-assistant-messages nil
        opencode-rendered-message-ids (make-hash-table :test 'equal)
        opencode-session--interrupted-message-ids (make-hash-table :test 'equal)
        opencode-session--bootstrapping nil
        opencode-session--queued-events nil
        opencode-session--draining-queued-events nil
        opencode-session--stream-batch-timer nil
        opencode-session--stream-batch-type nil
        opencode-session--stream-batch-start nil
        opencode-session--stream-batch-strings nil
        opencode-session--idle-finalize-timer nil
        opencode-session--idle-finalize-callback nil
        opencode-session--suspect-reconcile-timer nil
        opencode-session--stream-quiet-timer nil
        opencode-session--stream-message-states (make-hash-table :test 'equal)
        opencode--pending-question-tools (make-hash-table :test 'equal)
        opencode--completed-question-tools (make-hash-table :test 'equal)
        opencode--displayed-question-ids (make-hash-table :test 'equal)
        opencode--pending-permission-tools (make-hash-table :test 'equal)
        opencode-shell-calls (make-hash-table :test 'equal)
        opencode--tool-calls-displayed (make-hash-table :test 'equal)))

;;;###autoload
(defun opencode-record-replay (file)
  "Replay and evaluate OpenCode recording FILE in a fresh buffer."
  (interactive
   (list (read-file-name "OpenCode recording: " opencode-record-directory nil t)))
  (let* ((events (opencode-record--read-events file))
         (session (opencode-record--first-session events))
         (session-id (or (alist-get 'sessionID session nil nil #'string=)
                         "opencode-record-replay"))
         (directory (alist-get 'directory session nil nil #'string=))
         (buffer (generate-new-buffer
                  (format "*OpenCode Recording Replay: %s*"
                          (file-name-nondirectory file))))
         (opencode-session-buffers (make-hash-table :test 'equal))
         (opencode-record-recent-events-enabled nil)
         (replayed 0)
         (skipped 0)
         skipped-types
         (api-errors 0)
         last-snapshot
         history-snapshot
         errors)
    (dolist (entry events)
      (pcase (opencode-record--event-type entry)
        ("api.error" (cl-incf api-errors))
        ("ui.snapshot" (setq last-snapshot (opencode-record--event-data entry)))
        ("history.snapshot"
         (setq history-snapshot (opencode-record--event-data entry)))))
    (with-current-buffer buffer
      (opencode-record--init-replay-buffer session-id directory)
      (puthash session-id buffer opencode-session-buffers)
      (dolist (entry events)
        (when (equal (opencode-record--event-type entry) "event.decoded")
          (let* ((event (opencode-record--event-data entry))
                 (type (or (alist-get 'type event nil nil #'string=)
                           "<missing type>")))
            (if (opencode-record--safe-replay-event-p event)
                (condition-case err
                    (progn
                      (opencode--handle-message event)
                      (cl-incf replayed))
                  (error
                   (push (format "%s: %s"
                                 (alist-get 'type event nil nil #'string=)
                                 err)
                         errors)))
              (cl-incf skipped)
              (setf (alist-get type skipped-types nil nil #'string=)
                    (1+ (or (alist-get type skipped-types nil nil #'string=)
                            0)))))))
      (when (and (fboundp 'opencode-session--run-idle-finalize)
                 (timerp opencode-session--idle-finalize-timer))
        (opencode-session--run-idle-finalize buffer))
      (when history-snapshot
        (opencode-record--replay-history-snapshot history-snapshot))
      (let* ((recorded-active-ids
              (alist-get 'activeAssistantIDs last-snapshot nil nil #'string=))
             (recorded-status
              (map-nested-elt last-snapshot '(session status)))
             (issues nil))
        (when errors
          (push "Replay reducer errors occurred" issues))
        (when (> api-errors 0)
          (push (format "Recording contains %d API error(s)" api-errors)
                issues))
        (when opencode-assistant-messages
          (push "Replay ended with active assistant messages" issues))
        (when skipped-types
          (push (format "Skipped backend event types: %s"
                        (mapconcat
                         (lambda (entry)
                           (format "%s=%d" (car entry) (cdr entry)))
                         (nreverse skipped-types)
                         ", "))
                issues))
        (when (equal opencode-session-status "busy")
          (push "Replay ended busy" issues))
        (when (and (equal recorded-status "idle") recorded-active-ids)
          (push "Recorded UI snapshot was idle with active assistant messages"
                issues))
        (when (fboundp 'opencode-session--flush-stream-batch)
          (opencode-session--flush-stream-batch))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (format "Recording: %s\nReplayed events: %d\nSkipped events: %d\nErrors: %d\nAPI errors in recording: %d\nFinal status: %s\nActive assistant IDs: %S\nRendered messages: %d\nIssues: %s\n\n"
                        file replayed skipped (length errors) api-errors
                        opencode-session-status
                        (mapcar #'car opencode-assistant-messages)
                        (hash-table-count opencode-rendered-message-ids)
                        (if issues
                            (mapconcat #'identity (nreverse issues) "; ")
                          "none")))
        (when errors
          (insert "Replay errors:\n")
          (dolist (error (nreverse errors))
            (insert "- " error "\n"))
          (insert "\n"))
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)
    buffer))

(provide 'opencode-record)
;;; opencode-record.el ends here
