;;; opencode-benchmark.el --- Synthetic OpenCode frontend benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal, benchmark

;;; Commentary:

;; On-demand benchmarks for the OpenCode frontend reducer/rendering path.
;;
;; Example batch run:
;;   emacs --batch -L vendor/opencode -l vendor/opencode/opencode-benchmark.el \
;;     --eval '(opencode-benchmark-print :sessions 20 :turns 10 :deltas-per-turn 100)'
;;
;; The default workload creates 20 fake session buffers and sends interleaved
;; decoded OpenCode events through `opencode--handle-message'.  This models the
;; global event stream dispatching many active sessions concurrently without
;; requiring a live OpenCode server.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'opencode)
(require 'opencode-sessions)
(require 'subr-x)

(defvar opencode-session--history-validated-message-ids)

(defgroup opencode-benchmark nil
  "Synthetic benchmarks for the OpenCode Emacs frontend."
  :group 'opencode)

(defcustom opencode-benchmark-session-count 20
  "Default number of concurrent sessions for OpenCode benchmarks."
  :type 'integer
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-turn-count 5
  "Default number of assistant turns per synthetic session."
  :type 'integer
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-deltas-per-turn 100
  "Default number of text delta events per synthetic assistant turn."
  :type 'integer
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-delta-bytes 80
  "Default number of ASCII bytes in each synthetic text delta."
  :type 'integer
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-use-process t
  "Whether benchmark buffers should use a comint process.
When non-nil, each fake session gets a sleeping process so output goes through
`comint-output-filter', matching normal OpenCode session buffers more closely."
  :type 'boolean
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-status-events t
  "Whether synthetic turns include busy/idle status events."
  :type 'boolean
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-line-mode 'continuous
  "Shape of synthetic text deltas.
The value `continuous' sends deltas without newlines, stressing repeated current
line rendering.  The value `lines' ends each delta with a newline."
  :type '(choice (const :tag "Continuous text" continuous)
                 (const :tag "Line per delta" lines))
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-render-markdown nil
  "Whether benchmarks should include real markdown rendering.
When nil, `opencode--render-markdown' is rebound to `identity' so the benchmark
focuses on event routing, buffer insertion, and reducer state."
  :type 'boolean
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-json-decode nil
  "Whether benchmarks should include JSON/global-event decoding.
When non-nil, the workload is pre-encoded as global event JSON strings and each
event is handled through `opencode--handle-global-event-data'."
  :type 'boolean
  :group 'opencode-benchmark)

(defcustom opencode-benchmark-interleave 'round-robin
  "How synthetic session events are interleaved.
The value `round-robin' interleaves each phase across sessions and approximates
many active sessions streaming at once.  The value `session' completes each
session's turn before moving to the next session."
  :type '(choice (const :tag "Round-robin sessions" round-robin)
                 (const :tag "One session at a time" session))
  :group 'opencode-benchmark)

(defun opencode-benchmark--session-id (index)
  "Return a synthetic session id for INDEX."
  (format "ses_bench_%02d" index))

(defun opencode-benchmark--message-id (session-index turn-index)
  "Return a synthetic message id for SESSION-INDEX and TURN-INDEX."
  (format "msg_bench_%02d_%04d" session-index turn-index))

(defun opencode-benchmark--part-id (session-index turn-index)
  "Return a synthetic text part id for SESSION-INDEX and TURN-INDEX."
  (format "prt_bench_%02d_%04d" session-index turn-index))

(defun opencode-benchmark--delta-string (bytes line-mode)
  "Return a synthetic text delta of BYTES chars for LINE-MODE."
  (let* ((suffix (if (eq line-mode 'lines) "\n" ""))
         (payload-bytes (max 0 (- bytes (length suffix)))))
    (concat (make-string payload-bytes ?x) suffix)))

(defun opencode-benchmark--status-event (session-id status)
  "Return a synthetic status event for SESSION-ID and STATUS."
  `((type . "session.status")
    (properties . ((sessionID . ,session-id)
                   (status . ((type . ,status)))))))

(defun opencode-benchmark--message-event (session-id message-id)
  "Return a synthetic assistant message event for SESSION-ID and MESSAGE-ID."
  `((type . "message.updated")
    (properties . ((sessionID . ,session-id)
                   (info . ((id . ,message-id)
                            (sessionID . ,session-id)
                            (role . "assistant")
                            (tokens . ((input . 0)
                                       (output . 0)
                                       (reasoning . 0)
                                       (cache . ((read . 0)
                                                 (write . 0)))))))))))

(defun opencode-benchmark--text-seed-event (session-id message-id part-id)
  "Return a synthetic text seed event for SESSION-ID, MESSAGE-ID, and PART-ID."
  `((type . "message.part.updated")
    (properties . ((sessionID . ,session-id)
                   (part . ((id . ,part-id)
                            (sessionID . ,session-id)
                            (messageID . ,message-id)
                            (type . "text")
                            (text . "")))))))

(defun opencode-benchmark--text-delta-event (session-id message-id part-id delta)
  "Return a synthetic text delta event for SESSION-ID and MESSAGE-ID.
PART-ID identifies the text part and DELTA is the streamed text."
  `((type . "message.part.delta")
    (properties . ((sessionID . ,session-id)
                   (messageID . ,message-id)
                   (partID . ,part-id)
                   (field . "text")
                   (delta . ,delta)))))

(defun opencode-benchmark--step-finish-event (session-id message-id turn-index)
  "Return a synthetic stop step for SESSION-ID, MESSAGE-ID, and TURN-INDEX."
  `((type . "message.part.updated")
    (properties . ((sessionID . ,session-id)
                   (part . ((id . ,(format "%s_step_%04d" message-id turn-index))
                            (sessionID . ,session-id)
                            (messageID . ,message-id)
                            (type . "step-finish")
                            (reason . "stop")))))))

(defun opencode-benchmark--push-session-turn-events
    (session-index turn-index delta deltas-per-turn status-events events)
  "Push one session turn onto EVENTS and return the updated list.
SESSION-INDEX and TURN-INDEX identify the turn, DELTA is the text chunk,
DELTAS-PER-TURN is the number of delta events, and STATUS-EVENTS controls
whether busy/idle status events are included."
  (let* ((session-id (opencode-benchmark--session-id session-index))
         (message-id (opencode-benchmark--message-id session-index turn-index))
         (part-id (opencode-benchmark--part-id session-index turn-index)))
    (when status-events
      (push (opencode-benchmark--status-event session-id "busy") events))
    (push (opencode-benchmark--message-event session-id message-id) events)
    (push (opencode-benchmark--text-seed-event session-id message-id part-id)
          events)
    (dotimes (_ deltas-per-turn)
      (push (opencode-benchmark--text-delta-event
             session-id message-id part-id delta)
            events))
    (push (opencode-benchmark--step-finish-event
           session-id message-id turn-index)
          events)
    (when status-events
      (push (opencode-benchmark--status-event session-id "idle") events))
    events))

(cl-defun opencode-benchmark--events (&key sessions turns deltas-per-turn
                                           delta status-events interleave)
  "Return a synthetic event list.
SESSIONS, TURNS, DELTAS-PER-TURN, DELTA, STATUS-EVENTS, and INTERLEAVE describe
the generated workload."
  (let (events)
    (pcase interleave
      ('session
       (dotimes (session-index sessions)
         (dotimes (turn-index turns)
           (setq events
                 (opencode-benchmark--push-session-turn-events
                  session-index turn-index delta deltas-per-turn status-events
                  events)))))
      (_
       (dotimes (turn-index turns)
         (when status-events
           (dotimes (session-index sessions)
             (push (opencode-benchmark--status-event
                    (opencode-benchmark--session-id session-index) "busy")
                   events)))
         (dotimes (session-index sessions)
           (let ((session-id (opencode-benchmark--session-id session-index))
                 (message-id (opencode-benchmark--message-id session-index
                                                             turn-index)))
             (push (opencode-benchmark--message-event session-id message-id)
                   events)))
         (dotimes (session-index sessions)
           (let ((session-id (opencode-benchmark--session-id session-index))
                 (message-id (opencode-benchmark--message-id session-index
                                                             turn-index))
                 (part-id (opencode-benchmark--part-id session-index
                                                       turn-index)))
             (push (opencode-benchmark--text-seed-event
                    session-id message-id part-id)
                   events)))
         (dotimes (_ deltas-per-turn)
           (dotimes (session-index sessions)
             (let ((session-id (opencode-benchmark--session-id session-index))
                   (message-id (opencode-benchmark--message-id session-index
                                                               turn-index))
                   (part-id (opencode-benchmark--part-id session-index
                                                         turn-index)))
               (push (opencode-benchmark--text-delta-event
                      session-id message-id part-id delta)
                     events))))
         (dotimes (session-index sessions)
           (let ((session-id (opencode-benchmark--session-id session-index))
                 (message-id (opencode-benchmark--message-id session-index
                                                             turn-index)))
             (push (opencode-benchmark--step-finish-event
                    session-id message-id turn-index)
                   events)))
         (when status-events
           (dotimes (session-index sessions)
             (push (opencode-benchmark--status-event
                    (opencode-benchmark--session-id session-index) "idle")
                   events))))))
    (nreverse events)))

(defun opencode-benchmark--json-events (events directory)
  "Return EVENTS encoded as global event JSON strings for DIRECTORY."
  (mapcar (lambda (event)
            (json-encode `((directory . ,directory)
                           (payload . ,event))))
          events))

(defun opencode-benchmark--init-buffer (session-id directory use-process)
  "Initialize the current buffer as a fake SESSION-ID in DIRECTORY.
When USE-PROCESS is non-nil, attach a sleeping process for comint output."
  (setq default-directory (file-name-as-directory directory))
  (opencode-session-mode)
  (setq-local comint-input-sender #'ignore)
  (setq buffer-read-only nil
        opencode-session-id session-id
        opencode-session-directory (file-name-as-directory directory)
        opencode-session-status "idle"
        opencode-session-tokens 0
        opencode-session-agent nil
        opencode-session-agents nil
        opencode-session-pending-questions nil
        opencode-session-pending-permission nil
        opencode--tool-calls-displayed (make-hash-table :test 'equal)
        opencode-part-type (make-hash-table :test 'equal)
        opencode-part-text (make-hash-table :test 'equal)
        opencode-part-sent (make-hash-table :test 'equal)
        opencode-part-replay-text (make-hash-table :test 'equal)
        opencode-part-region-start (make-hash-table :test 'equal)
        opencode-part-region-end (make-hash-table :test 'equal)
        opencode-part-message (make-hash-table :test 'equal)
        opencode-message-roles (make-hash-table :test 'equal)
        opencode-session--history-validated-message-ids (make-hash-table :test 'equal)
        opencode-shell-echo (make-hash-table :test 'equal)
        opencode-assistant-messages nil
        opencode-rendered-message-ids (make-hash-table :test 'equal)
        opencode-session--interrupted-message-ids (make-hash-table :test 'equal)
        opencode-session--bootstrapping nil
        opencode-session--queued-events nil
        opencode-session--draining-queued-events nil
        opencode-session--status-poll-timer nil
        opencode-session--status-poll-in-flight nil
        opencode-session--live-render-timer nil
        opencode-session--live-render-type nil
        opencode-session--live-render-start nil
        opencode-session--stream-batch-timer nil
        opencode-session--stream-batch-type nil
        opencode-session--stream-batch-start nil
        opencode-session--stream-batch-strings nil
        opencode--reconcile-timer nil
        opencode-session--suspect-reconcile-timer nil
        opencode-session--stream-quiet-timer nil
        opencode-session--stream-message-states (make-hash-table :test 'equal)
        opencode--pending-question-tools (make-hash-table :test 'equal)
        opencode--completed-question-tools (make-hash-table :test 'equal)
        opencode--displayed-question-ids (make-hash-table :test 'equal)
        opencode--pending-permission-tools (make-hash-table :test 'equal)
        opencode-shell-calls (make-hash-table :test 'equal))
  (when use-process
    (let ((proc (start-process "opencode-benchmark" (current-buffer)
                               "sleep" "600")))
      (set-process-query-on-exit-flag proc nil)
      (set-marker (process-mark proc) (point-min)))))

(defun opencode-benchmark--make-buffers (sessions directory use-process)
  "Create SESSIONS fake OpenCode buffers in DIRECTORY.
When USE-PROCESS is non-nil, each buffer has a sleeping comint process."
  (let (buffers)
    (dotimes (session-index sessions)
      (let* ((session-id (opencode-benchmark--session-id session-index))
             (buffer (generate-new-buffer
                      (format " *opencode-benchmark-%02d*" session-index))))
        (with-current-buffer buffer
          (opencode-benchmark--init-buffer session-id directory use-process))
        (puthash session-id buffer opencode-session-buffers)
        (push buffer buffers)))
    (nreverse buffers)))

(defun opencode-benchmark--kill-buffers (buffers)
  "Kill benchmark BUFFERS and their processes."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag process nil)
        (delete-process process))
      (kill-buffer buffer))))

(defun opencode-benchmark--buffer-stats (buffers)
  "Return aggregate rendering stats for BUFFERS."
  (let ((total-chars 0)
        (max-chars 0)
        (total-overlays 0)
        (rendered-messages 0)
        (active-messages 0))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((chars (buffer-size)))
            (setq total-chars (+ total-chars chars)
                  max-chars (max max-chars chars)
                  total-overlays (+ total-overlays
                                    (length (overlays-in (point-min)
                                                         (point-max))))
                  rendered-messages (+ rendered-messages
                                       (hash-table-count
                                        opencode-rendered-message-ids))
                  active-messages (+ active-messages
                                     (length opencode-assistant-messages)))))))
    `((totalBufferChars . ,total-chars)
      (maxBufferChars . ,max-chars)
      (totalOverlays . ,total-overlays)
      (renderedMessages . ,rendered-messages)
      (activeMessages . ,active-messages))))

(defmacro opencode-benchmark--with-stubs (render-markdown &rest body)
  "Run BODY with benchmark-safe OpenCode stubs.
When RENDER-MARKDOWN is nil, rebind `opencode--render-markdown' to `identity'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'opencode-record--event) #'ignore)
             ((symbol-function 'opencode-record--after-event) #'ignore)
             ((symbol-function 'opencode-record--raw-event) #'ignore)
             ((symbol-function 'opencode--log-event) #'ignore)
             ((symbol-function 'opencode-session--schedule-reconcile)
              #'ignore)
             ((symbol-function 'opencode-session--schedule-status-poll)
              #'ignore)
             ((symbol-function 'opencode--toast-show) #'ignore))
     (if ,render-markdown
         (progn ,@body)
       (cl-letf (((symbol-function 'opencode--render-markdown) #'identity))
         ,@body))))

(defun opencode-benchmark--result
    (sessions turns deltas-per-turn delta-bytes status-events render-markdown
              json-decode use-process line-mode interleave event-count byte-count
              elapsed gc-count gc-elapsed-delta buffers)
  "Return a benchmark result alist.
The arguments describe the workload and measured ELAPSED time.  GC-COUNT and
GC-ELAPSED-DELTA are deltas from before the benchmark, and BUFFERS are the
rendered session buffers."
  (let ((events-per-second (if (> elapsed 0) (/ event-count elapsed) 0.0))
        (bytes-per-second (if (> elapsed 0) (/ byte-count elapsed) 0.0)))
    (append
     `((sessions . ,sessions)
       (turns . ,turns)
       (deltasPerTurn . ,deltas-per-turn)
       (deltaBytes . ,delta-bytes)
       (statusEvents . ,(and status-events t))
       (renderMarkdown . ,(and render-markdown t))
       (jsonDecode . ,(and json-decode t))
       (useProcess . ,(and use-process t))
       (lineMode . ,(symbol-name line-mode))
       (interleave . ,(symbol-name interleave))
       (events . ,event-count)
       (streamedBytes . ,byte-count)
       (elapsedSeconds . ,elapsed)
       (eventsPerSecond . ,events-per-second)
       (streamedMiBPerSecond . ,(/ bytes-per-second 1048576.0))
       (gcCount . ,gc-count)
       (gcElapsedSeconds . ,gc-elapsed-delta))
     (opencode-benchmark--buffer-stats buffers))))

;;;###autoload
(cl-defun opencode-benchmark-run
    (&key (sessions opencode-benchmark-session-count)
          (turns opencode-benchmark-turn-count)
          (deltas-per-turn opencode-benchmark-deltas-per-turn)
          (delta-bytes opencode-benchmark-delta-bytes)
          (status-events opencode-benchmark-status-events)
          (render-markdown opencode-benchmark-render-markdown)
          (json-decode opencode-benchmark-json-decode)
          (use-process opencode-benchmark-use-process)
          (line-mode opencode-benchmark-line-mode)
          (interleave opencode-benchmark-interleave)
          (directory temporary-file-directory)
          keep-buffers)
  "Run a synthetic OpenCode event benchmark and return an alist.
SESSIONS is the number of concurrent fake sessions.  TURNS is the number of
assistant turns per session.  DELTAS-PER-TURN is the number of text delta events
in each turn.  DELTA-BYTES controls the size of each delta.  STATUS-EVENTS
includes busy/idle events when non-nil.  RENDER-MARKDOWN includes real markdown
rendering when non-nil.  JSON-DECODE sends pre-encoded global event JSON through
`opencode--handle-global-event-data' when non-nil.  USE-PROCESS attaches a
sleeping comint process to each fake session buffer when non-nil.  LINE-MODE is
either `continuous' or `lines'.  INTERLEAVE is either `round-robin' or `session'.
DIRECTORY is the fake project directory.  KEEP-BUFFERS leaves benchmark buffers
alive for inspection when non-nil."
  (interactive)
  (unless (and (integerp sessions) (> sessions 0))
    (user-error "SESSIONS must be a positive integer"))
  (unless (and (integerp turns) (> turns 0))
    (user-error "TURNS must be a positive integer"))
  (unless (and (integerp deltas-per-turn) (>= deltas-per-turn 0))
    (user-error "DELTAS-PER-TURN must be a non-negative integer"))
  (unless (and (integerp delta-bytes) (>= delta-bytes 0))
    (user-error "DELTA-BYTES must be a non-negative integer"))
  (unless (memq line-mode '(continuous lines))
    (user-error "LINE-MODE must be `continuous' or `lines'"))
  (unless (memq interleave '(round-robin session))
    (user-error "INTERLEAVE must be `round-robin' or `session'"))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (delta (opencode-benchmark--delta-string delta-bytes line-mode))
         (events (opencode-benchmark--events
                  :sessions sessions
                  :turns turns
                  :deltas-per-turn deltas-per-turn
                  :delta delta
                  :status-events status-events
                  :interleave interleave))
         (event-count (length events))
         (byte-count (* sessions turns deltas-per-turn (length delta)))
         (workload (if json-decode
                       (opencode-benchmark--json-events events directory)
                     events))
         (opencode-session-buffers (make-hash-table :test 'equal))
         buffers
         result)
    (unwind-protect
        (progn
          (setq buffers (opencode-benchmark--make-buffers
                         sessions directory use-process))
          (garbage-collect)
          (let ((gc-start gcs-done)
                (gc-elapsed-start (if (boundp 'gc-elapsed) gc-elapsed 0.0))
                (start (current-time)))
            (opencode-benchmark--with-stubs render-markdown
					    (dolist (event workload)
					      (if json-decode
						  (opencode--handle-global-event-data event)
						(opencode--handle-message event))))
            (let* ((elapsed (float-time (time-subtract (current-time) start)))
                   (gc-count (- gcs-done gc-start))
                   (gc-elapsed-delta
                    (- (if (boundp 'gc-elapsed) gc-elapsed 0.0)
                       gc-elapsed-start)))
              (setq result
                    (opencode-benchmark--result
                     sessions turns deltas-per-turn delta-bytes status-events
                     render-markdown json-decode use-process line-mode interleave
                     event-count byte-count elapsed gc-count gc-elapsed-delta
                     buffers)))))
      (unless keep-buffers
        (opencode-benchmark--kill-buffers buffers)))
    (when (called-interactively-p 'interactive)
      (message "OpenCode benchmark: %.0f events/sec over %.3fs"
               (alist-get 'eventsPerSecond result)
               (alist-get 'elapsedSeconds result)))
    result))

(defun opencode-benchmark-format (result)
  "Format benchmark RESULT from `opencode-benchmark-run'."
  (mapconcat (lambda (entry)
               (format "%s: %s" (car entry) (cdr entry)))
             result
             "\n"))

;;;###autoload
(defun opencode-benchmark-print (&rest args)
  "Run `opencode-benchmark-run' with ARGS and print a text report."
  (let ((result (apply #'opencode-benchmark-run args)))
    (princ (opencode-benchmark-format result))
    (princ "\n")
    result))

(provide 'opencode-benchmark)
;;; opencode-benchmark.el ends here
