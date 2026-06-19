;;; opencode-event-tests.el --- Event reducer regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for OpenCode event-ordering state.  These target the
;; session reducer state directly so they can cover missed/out-of-order events
;; without needing a live OpenCode server.

;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-fuzz)
(require 'opencode-sessions)

(defvar opencode-session--history-validated-message-ids)

(defmacro opencode-event-tests--with-state (&rest body)
  "Run BODY with fresh buffer-local OpenCode reducer state."
  (declare (indent 0))
  `(with-temp-buffer
     (setq opencode-session-id "ses_test"
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
           opencode-session--interrupted-message-ids (make-hash-table :test 'equal)
           opencode-session--bootstrapping nil
           opencode-session--queued-events nil
           opencode-session--draining-queued-events nil
           opencode-session--suspect-reconcile-timer nil
           opencode-session--stream-quiet-timer nil
           opencode-session--stream-message-states (make-hash-table :test 'equal)
           opencode-session-pending-questions nil
           opencode--pending-question-tools (make-hash-table :test 'equal)
           opencode--completed-question-tools (make-hash-table :test 'equal)
           opencode--displayed-question-ids (make-hash-table :test 'equal)
           opencode--pending-permission-tools (make-hash-table :test 'equal)
           opencode-shell-calls (make-hash-table :test 'equal))
     ,@body))

(defmacro opencode-event-tests--with-session-buffer (&rest body)
  "Run BODY in a fake live OpenCode session buffer."
  (declare (indent 0))
  `(let ((opencode-session-buffers (make-hash-table :test 'equal))
         (buffer (generate-new-buffer " *opencode-event-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (let ((proc (start-process "opencode-event-test" buffer
                                      "sleep" "60")))
             (unwind-protect
                 (progn
                   (set-process-query-on-exit-flag proc nil)
                   (set-marker (process-mark proc) (point-min))
                   (setq opencode-session-id "ses_test"
                         opencode-session-directory default-directory
                         opencode-session-status "idle"
                         opencode-session-tokens 0
                         opencode-session-agent nil
                         opencode-session-pending-questions nil
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
                         opencode-session--suspect-reconcile-timer nil
                         opencode-session--stream-quiet-timer nil
                         opencode-session--stream-message-states (make-hash-table :test 'equal)
                         opencode--pending-question-tools (make-hash-table :test 'equal)
                         opencode--completed-question-tools (make-hash-table :test 'equal)
                         opencode--displayed-question-ids (make-hash-table :test 'equal)
                         opencode--pending-permission-tools (make-hash-table :test 'equal)
                         opencode-shell-calls (make-hash-table :test 'equal)
                         opencode--tool-calls-displayed (make-hash-table :test 'equal))
                   (puthash "ses_test" (current-buffer) opencode-session-buffers)
                   ,@body)
               (delete-process proc))))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defmacro opencode-event-tests--with-two-session-buffers (&rest body)
  "Run BODY with two fake live OpenCode session buffers."
  (declare (indent 0))
  `(let ((opencode-session-buffers (make-hash-table :test 'equal))
         (buffer-a (generate-new-buffer " *opencode-event-test-a*"))
         (buffer-b (generate-new-buffer " *opencode-event-test-b*")))
     (unwind-protect
         (progn
           (dolist (entry `((,buffer-a . "ses_a") (,buffer-b . "ses_b")))
             (with-current-buffer (car entry)
               (let ((proc (start-process "opencode-event-test" (car entry)
                                          "sleep" "60")))
                 (set-process-query-on-exit-flag proc nil)
                 (set-marker (process-mark proc) (point-min)))
               (setq opencode-session-id (cdr entry)
                     opencode-session-directory default-directory
                     opencode-session-status "idle"
                     opencode-session-tokens 0
                     opencode-session-agent nil
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
                     opencode-session--history-validated-message-ids (make-hash-table :test 'equal)
                     opencode-shell-echo (make-hash-table :test 'equal)
                     opencode-assistant-messages nil
                     opencode-rendered-message-ids (make-hash-table :test 'equal)
                     opencode-session--interrupted-message-ids (make-hash-table :test 'equal)
                     opencode-session--bootstrapping nil
                     opencode-session--queued-events nil
                     opencode-session--draining-queued-events nil
                     opencode-session--suspect-reconcile-timer nil
                     opencode-session--stream-quiet-timer nil
                     opencode-session--stream-message-states (make-hash-table :test 'equal)
                     opencode--pending-question-tools (make-hash-table :test 'equal)
                     opencode--completed-question-tools (make-hash-table :test 'equal)
                     opencode--displayed-question-ids (make-hash-table :test 'equal)
                     opencode--pending-permission-tools (make-hash-table :test 'equal)
                     opencode-shell-calls (make-hash-table :test 'equal)
                     opencode--tool-calls-displayed (make-hash-table :test 'equal))
               (puthash (cdr entry) (car entry) opencode-session-buffers)))
           ,@body)
       (dolist (buffer (list buffer-a buffer-b))
         (when (buffer-live-p buffer)
           (when-let ((proc (get-buffer-process buffer)))
             (delete-process proc))
           (kill-buffer buffer))))))

(defmacro opencode-event-tests--with-sse-state (&rest body)
  "Run BODY with fresh fast-SSE parser state."
  (declare (indent 0))
  `(with-temp-buffer
     (setq opencode--sse-pending ""
           opencode--sse-discarding-ignored-frame nil
           opencode--sse-discard-tail ""
           opencode--sse-open-logged nil)
     ,@body))

(defun opencode-event-tests--buffer-text ()
  "Return current buffer text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun opencode-event-tests--assistant (id &optional completed)
  "Return a message.updated event for assistant message ID.
When COMPLETED is non-nil include completion metadata."
  `((type . "message.updated")
    (properties . ((sessionID . "ses_test")
                   (info . ((id . ,id)
                            (sessionID . "ses_test")
                            (role . "assistant")
                            (tokens . ((input . 0)
                                       (output . 0)
                                       (reasoning . 0)
                                       (cache . ((read . 0) (write . 0)))))
                            ,@(when completed
                                '((finish . "stop")
                                  (time . ((completed . 1)))))))))))

(defun opencode-event-tests--text-updated (message-id part-id text)
  "Return a text part update for MESSAGE-ID, PART-ID, and TEXT."
  `((type . "message.part.updated")
    (properties . ((sessionID . "ses_test")
                   (part . ((id . ,part-id)
                            (sessionID . "ses_test")
                            (messageID . ,message-id)
                            (type . "text")
                            (text . ,text)))))))

(defun opencode-event-tests--reasoning-updated (message-id part-id text)
  "Return a reasoning part update for MESSAGE-ID, PART-ID, and TEXT."
  `((type . "message.part.updated")
    (properties . ((sessionID . "ses_test")
                   (part . ((id . ,part-id)
                            (sessionID . "ses_test")
                            (messageID . ,message-id)
                            (type . "reasoning")
                            (text . ,text)))))))

(defun opencode-event-tests--text-delta (message-id part-id delta)
  "Return a text delta for MESSAGE-ID, PART-ID, and DELTA."
  `((type . "message.part.delta")
    (properties . ((sessionID . "ses_test")
                   (messageID . ,message-id)
                   (partID . ,part-id)
                   (field . "text")
                   (delta . ,delta)))))

(defun opencode-event-tests--step-finish (message-id)
  "Return a stopped step-finish part update for MESSAGE-ID."
  `((type . "message.part.updated")
    (properties . ((sessionID . "ses_test")
                   (part . ((sessionID . "ses_test")
                            (messageID . ,message-id)
                            (type . "step-finish")
                            (reason . "stop")))))))

(defun opencode-event-tests--stored-assistant (message-id part-id text
                                                          &optional completed)
  "Return a stored assistant message with TEXT part."
  `((info . ((id . ,message-id)
             (sessionID . "ses_test")
             (role . "assistant")
             ,@(when completed '((time . ((completed . 1)))))))
    (parts . [((id . ,part-id)
               (sessionID . "ses_test")
               (messageID . ,message-id)
               (type . "text")
               (text . ,text)
               ,@(when completed '((time . ((end . 1))))))])))

(defun opencode-event-tests--stored-user (message-id text)
  "Return a stored user MESSAGE-ID with TEXT."
  `((info . ((id . ,message-id)
             (sessionID . "ses_test")
             (role . "user")
             (agent . "build")
             (model . ((providerID . "openai")
                       (modelID . "gpt-5.5")))))
    (parts . [((id . ,(concat message-id "_text"))
               (sessionID . "ses_test")
               (messageID . ,message-id)
               (type . "text")
               (text . ,text))])))

(defun opencode-event-tests--tool-updated (message-id part-id call-id tool
                                                      status input)
  "Return a tool part update for MESSAGE-ID, PART-ID, CALL-ID, and TOOL."
  `((type . "message.part.updated")
    (properties . ((sessionID . "ses_test")
                   (part . ((id . ,part-id)
                            (sessionID . "ses_test")
                            (messageID . ,message-id)
                            (callID . ,call-id)
                            (type . "tool")
                            (tool . ,tool)
                            (state . ((status . ,status)
                                      (input . ,input)))))))))

(defun opencode-event-tests--completed-question-tool-part ()
  "Return a completed question tool part with a recorded answer."
  '((id . "prt_question")
    (sessionID . "ses_test")
    (messageID . "msg_question")
    (callID . "call_question")
    (type . "tool")
    (tool . "question")
    (state . ((status . "completed")
              (input . ((questions . [((question . "Are you ready?")
                                       (header . "Physical Ready")
                                       (options . [((label . "Ready now")
                                                    (description . "Start now."))
                                                   ((label . "Not yet")
                                                    (description . "Wait."))])
                                       (multiple . :json-false))])))
              (output . "User has answered your questions.")
              (metadata . ((answers . [["Ready now"]])))))))

(defun opencode-event-tests--shell-started (call-id command)
  "Return a first-class shell started event for CALL-ID and COMMAND."
  `((type . "session.next.shell.started")
    (properties . ((sessionID . "ses_test")
                   (callID . ,call-id)
                   (command . ,command)))))

(defun opencode-event-tests--shell-ended (call-id output)
  "Return a first-class shell ended event for CALL-ID and OUTPUT."
  `((type . "session.next.shell.ended")
    (properties . ((sessionID . "ses_test")
                   (callID . ,call-id)
                   (output . ,output)))))

(defun opencode-event-tests--status (session-id status)
  "Return a session status event for SESSION-ID and STATUS."
  `((type . "session.status")
    (properties . ((sessionID . ,session-id)
                   (status . ((type . ,status)))))))

(defun opencode-event-tests--all-permutations (items)
  "Return all permutations of ITEMS."
  (if (null items)
      (list nil)
    (cl-loop for item in items
             append (mapcar (lambda (rest) (cons item rest))
                            (opencode-event-tests--all-permutations
                             (cl-remove item items :count 1
                                        :test #'equal))))))

(ert-deftest opencode-event-delta-before-type-is-buffered ()
  "Text deltas that arrive before type metadata are not lost."
  (opencode-event-tests--with-state
   (should (equal "Hello "
                  (opencode-session--part-delta
                   '((partID . "prt_1") (messageID . "msg_1"))
                   "Hello ")))
   (should (= 0 (gethash "prt_1" opencode-part-sent 0)))
   (should (equal "Hello " (gethash "prt_1" opencode-part-text)))
   (should (equal "Hello "
                  (opencode-session--part-delta
                   '((id . "prt_1") (messageID . "msg_1")
                     (type . "text") (text . ""))
                   nil)))
   (opencode-session--mark-part-rendered
    '((id . "prt_1") (messageID . "msg_1")))
   (should (equal "world"
                  (opencode-session--part-delta
                   '((partID . "prt_1") (messageID . "msg_1"))
                   "world")))
   (opencode-session--mark-part-rendered
    '((partID . "prt_1") (messageID . "msg_1")))
   (should (equal "Hello world" (gethash "prt_1" opencode-part-text)))
   (should (= 11 (gethash "prt_1" opencode-part-sent)))))

(ert-deftest opencode-event-text-accumulation-survives-permuted-snapshots ()
  "Text accumulation survives plausible snapshot/delta reorderings."
  (dolist (events (opencode-event-tests--all-permutations
                   '((delta "Hello")
                     (snapshot "")
                     (snapshot "Hello world")
                     (delta " world"))))
    (opencode-event-tests--with-state
     (dolist (event events)
       (pcase event
         (`(delta ,text)
          (opencode-session--part-delta
           '((partID . "prt_1") (messageID . "msg_1")) text))
         (`(snapshot ,text)
          (opencode-session--part-delta
           `((id . "prt_1") (messageID . "msg_1")
             (type . "text") (text . ,text)) nil))))
     (should (string-prefix-p "Hello"
                              (gethash "prt_1" opencode-part-text "")))
     (should (>= (length (gethash "prt_1" opencode-part-text ""))
                 5)))))

(ert-deftest opencode-event-short-snapshot-does-not-truncate-buffer ()
  "A shorter/full-empty snapshot must not overwrite accumulated text."
  (opencode-event-tests--with-state
   (puthash "prt_1" "already buffered" opencode-part-text)
   (should (equal "already buffered"
                  (opencode-session--part-delta
                   '((id . "prt_1") (messageID . "msg_1")
                     (type . "text") (text . ""))
                   nil)))
   (should (equal "already buffered"
                  (gethash "prt_1" opencode-part-text)))))

(ert-deftest opencode-event-leading-whitespace-waits-for-content ()
  "Initial whitespace is buffered and emitted with the first real content."
  (opencode-event-tests--with-session-buffer
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'opencode-session--schedule-reconcile)
              (lambda (&rest _args))))
     (opencode--handle-message (opencode-event-tests--assistant "msg_1" nil))
     (opencode--handle-message
      (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" " "))
     (should (string-empty-p (opencode-event-tests--buffer-text)))
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" "Found"))
     (should (equal " Found" (opencode-event-tests--buffer-text)))
     (should (= 6 (gethash "prt_1" opencode-part-sent))))))

(ert-deftest opencode-event-delta-only-live-text-renders-provisionally ()
  "Delta-only assistant text renders live and finalizes as text at idle."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-session-idle-finalize-delay 0))
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode-session--schedule-reconcile)
		(lambda (&rest _args))))
       (opencode--handle-message
	(opencode-event-tests--text-delta "msg_1" "prt_1" "TRACE"))
       (opencode--handle-message
	(opencode-event-tests--text-delta "msg_1" "prt_1" "_OK"))
       (should (equal "TRACE_OK" (opencode-event-tests--buffer-text)))
       (opencode--handle-message
        '((type . "session.status")
          (properties . ((sessionID . "ses_test")
                         (status . ((type . "idle")))))))
       (should (equal "TRACE_OK\n\n" (opencode-event-tests--buffer-text)))
       (should (gethash "msg_1" opencode-rendered-message-ids))
       (should-not opencode-assistant-messages)))))

(ert-deftest opencode-event-streaming-text-renders-markdown-on-finish ()
  "Typed text deltas append raw text and render markdown once on finish."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (let (rendered)
     (cl-letf (((symbol-function 'opencode--render-markdown)
                (lambda (text)
                  (push text rendered)
                  text))
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message (opencode-event-tests--assistant "msg_1"))
       (opencode--handle-message
        (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" "hello"))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" " world"))
       (should (equal "hello world" (opencode-event-tests--buffer-text)))
       (should-not rendered)
       (should-not (timerp opencode-session--live-render-timer))
       (opencode--handle-message (opencode-event-tests--step-finish "msg_1"))
       (should (equal '("hello world") (nreverse rendered)))
       (should (equal "hello world\n\n"
                      (opencode-event-tests--buffer-text)))))))

(ert-deftest opencode-event-streaming-output-batches-until-finish ()
  "Typed text deltas can batch buffer insertion until a finish boundary."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (opencode-session-mode)
   (let ((opencode-session-stream-batch-delay 60)
         rendered)
     (cl-letf (((symbol-function 'opencode--render-markdown)
                (lambda (text)
                  (push text rendered)
                  text))
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message (opencode-event-tests--assistant "msg_1"))
       (opencode--handle-message
        (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" "a"))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" "b"))
       (should (string-empty-p (opencode-event-tests--buffer-text)))
       (should (timerp opencode-session--stream-batch-timer))
       (opencode--handle-message (opencode-event-tests--step-finish "msg_1"))
       (should-not (timerp opencode-session--stream-batch-timer))
       (should (equal '("ab") (nreverse rendered)))
       (should (equal "ab\n\n" (opencode-event-tests--buffer-text)))))))

(ert-deftest opencode-event-live-markdown-rendering-is-coalesced ()
  "Visible live markdown rendering keeps one pending timer for many deltas."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (let ((opencode-session-live-markdown-visible-only nil)
         (opencode-session-live-markdown-delay 60)
         rendered)
     (cl-letf (((symbol-function 'opencode--render-markdown)
                (lambda (text)
                  (push text rendered)
                  text))
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message (opencode-event-tests--assistant "msg_1"))
       (opencode--handle-message
        (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" "a"))
       (let ((timer opencode-session--live-render-timer))
         (should (timerp timer))
         (opencode--handle-message
          (opencode-event-tests--text-delta "msg_1" "prt_1" "b"))
         (should (eq timer opencode-session--live-render-timer))
         (cancel-timer timer)
         (opencode-session--run-live-render (current-buffer)))
       (should (equal '("ab") (nreverse rendered)))
       (should-not (timerp opencode-session--live-render-timer))))))

(ert-deftest opencode-event-late-reasoning-seed-does-not-dump-as-text ()
  "Buffered deltas become reasoning when a late reasoning seed arrives."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-show-reasoning nil))
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" "thinking"))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" " aloud"))
       (should (equal "thinking aloud" (opencode-event-tests--buffer-text)))
       (opencode--handle-message
        (opencode-event-tests--reasoning-updated
         "msg_1" "prt_1" "thinking aloud"))
       (should (string-empty-p (opencode-event-tests--buffer-text)))
       (should (= 14 (gethash "prt_1" opencode-part-sent)))))))

(ert-deftest opencode-event-reconcile-hydrates-provisional-part-types ()
  "Persisted history reclassifies live provisional parts without losing tail."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-show-reasoning nil))
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_reason" "thinking"))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_text" "answer tail"))
       (should (equal "thinkinganswer tail"
                      (opencode-event-tests--buffer-text)))
       (opencode-session--reconcile-active-provisional-messages
        '(((info . ((id . "msg_1")
                    (sessionID . "ses_test")
                    (role . "assistant")))
           (parts . [((id . "prt_reason")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (type . "reasoning")
                      (text . "thinking"))
                     ((id . "prt_text")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (type . "text")
                      (text . "answer"))]))))
       (should (equal "answer tail" (opencode-event-tests--buffer-text)))
       (should (equal "reasoning" (gethash "prt_reason" opencode-part-type)))
       (should (equal "text" (gethash "prt_text" opencode-part-type)))
       (should (= 8 (gethash "prt_reason" opencode-part-sent)))
       (should (= 11 (gethash "prt_text" opencode-part-sent)))))))

(ert-deftest opencode-event-reconcile-completes-active-text-tail ()
  "Persisted history fills in a completed tail missed by stream events."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (let ((opencode-session-stream-batch-delay nil))
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message (opencode-event-tests--assistant "msg_1"))
       (opencode--handle-message
        (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1"
                                          "kernel drivers"))
       (should (equal "kernel drivers" (opencode-event-tests--buffer-text)))
       (opencode-session--reconcile-active-provisional-messages
        (list (opencode-event-tests--stored-assistant
	       "msg_1" "prt_1" "kernel drivers/device nodes present." t)))
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "kernel drivers/device nodes present" text))
         (should-not (string-match-p "kernel driverskernel" text)))
       (should (gethash "msg_1" opencode-rendered-message-ids))))))

(ert-deftest opencode-event-reconcile-extends-rendered-text-tail ()
  "Persisted history extends an already-rendered partial text part."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity))
     (let ((start (copy-marker (point) t))
           end)
       (insert "source-level naming\n-")
       (setq end (copy-marker (point) t))
       (puthash "msg_1" t opencode-rendered-message-ids)
       (puthash "msg_1" "assistant" opencode-message-roles)
       (puthash "prt_1" "msg_1" opencode-part-message)
       (puthash "prt_1" "text" opencode-part-type)
       (puthash "prt_1" "source-level naming\n-" opencode-part-text)
       (puthash "prt_1" 21 opencode-part-sent)
       (puthash "prt_1" start opencode-part-region-start)
       (puthash "prt_1" end opencode-part-region-end)
       (opencode-session--reconcile-missed-history-messages
        (list (opencode-event-tests--stored-assistant
	       "msg_1" "prt_1"
	       "source-level naming\n- actionability\n- latency\n\nnext fix" t)))
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "source-level naming" text))
         (should (string-match-p "actionability" text))
         (should (string-match-p "latency" text))
         (should (string-match-p "next fix" text))
         (should-not (string-match-p "source-level namingsource-level" text)))
       (should (equal "source-level naming\n- actionability\n- latency\n\nnext fix"
                      (gethash "prt_1" opencode-part-text)))))))

(ert-deftest opencode-event-reconcile-inserts-missing-rendered-text-part ()
  "Persisted history inserts a text part missing from a rendered message."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity))
     (let ((start (copy-marker (point) t))
           end)
       (insert "**thinking**")
       (setq end (copy-marker (point) t))
       (puthash "msg_1" t opencode-rendered-message-ids)
       (puthash "msg_1" "assistant" opencode-message-roles)
       (puthash "prt_reason" "msg_1" opencode-part-message)
       (puthash "prt_reason" "reasoning" opencode-part-type)
       (puthash "prt_reason" "**thinking**" opencode-part-text)
       (puthash "prt_reason" 12 opencode-part-sent)
       (puthash "prt_reason" start opencode-part-region-start)
       (puthash "prt_reason" end opencode-part-region-end)
       (opencode-session--reconcile-missed-history-messages
        '(((info . ((id . "msg_1")
                    (sessionID . "ses_test")
                    (role . "assistant")
                    (time . ((completed . 1)))))
           (parts . [((id . "prt_reason")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (type . "reasoning")
                      (text . "**thinking**"))
                     ((id . "prt_text")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (type . "text")
                      (text . "final answer"))]))))
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "thinking" text))
         (should (string-match-p "final answer" text))
         (should-not (string-match-p "final answerfinal answer" text)))
       (should (equal "final answer" (gethash "prt_text" opencode-part-text)))))))

(ert-deftest opencode-event-reconcile-message-repairs-rendered-tail ()
  "Finished-message reconciliation repairs already-rendered partial output."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'plz)
              (lambda (_method url &rest plist)
                (should (string-match-p
                         "/session/ses_test/message/msg_1\\'" url))
                (funcall (plist-get plist :then)
                         (opencode-event-tests--stored-assistant
                          "msg_1" "prt_1" "partial complete" t)))))
     (let ((opencode-api-url "http://localhost:4096")
           (start (copy-marker (point) t))
           end)
       (insert "partial")
       (setq end (copy-marker (point) t))
       (puthash "msg_1" t opencode-rendered-message-ids)
       (puthash "msg_1" "assistant" opencode-message-roles)
       (puthash "prt_1" "msg_1" opencode-part-message)
       (puthash "prt_1" "text" opencode-part-type)
       (puthash "prt_1" "partial" opencode-part-text)
       (puthash "prt_1" 7 opencode-part-sent)
       (puthash "prt_1" start opencode-part-region-start)
       (puthash "prt_1" end opencode-part-region-end)
       (opencode-session--reconcile-message "ses_test" "msg_1")
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "partial complete" text))
         (should-not (string-match-p "partialpartial" text)))
       (should (equal "partial complete" (gethash "prt_1" opencode-part-text)))))))

(ert-deftest opencode-event-suspect-reconcile-hydrates-only-suspect-message ()
  "Suspect reconciliation fetches targeted message details only."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'plz)
              (lambda (_method url &rest plist)
                (should (string-match-p
                         "/session/ses_test/message/msg_suspect\\'" url))
                (funcall (plist-get plist :then)
                         (opencode-event-tests--stored-assistant
                          "msg_suspect" "prt_suspect"
                          "suspect tail" t)))))
     (let ((opencode-api-url "http://localhost:4096"))
       (opencode-session--note-message-stream-event
        "msg_suspect" "prt_suspect" "suspect" "text")
       (opencode-session--mark-stream-suspect "unit" "msg_suspect")
       (opencode-session--reconcile-suspect-history "ses_test")
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "suspect tail" text))
         (should-not (string-match-p "other tail" text)))))))

(ert-deftest opencode-event-sync-history-tail-renders-broad-history ()
  "Unified history sync renders missed user and completed assistant history."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'plz)
              (lambda (_method url &rest plist)
                (should (string-match-p
                         "/session/ses_test/message[?]limit=7\\'" url))
                (funcall (plist-get plist :then)
                         (list (opencode-event-tests--stored-user
                                "msg_user" "Broad prompt")
                               (opencode-event-tests--stored-assistant
                                "msg_assistant" "prt_assistant"
                                "Broad answer" t))))))
     (let ((opencode-api-url "http://localhost:4096")
           (opencode-session-history-sync-limit 7))
       ;; Existing session buffers only use broad assistant catch-up after some
       ;; rendered state exists; this mirrors a long-running live buffer.
       (puthash "msg_seen" t opencode-rendered-message-ids)
       (opencode-session--sync-history-tail "ses_test" "unit")
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "Broad prompt" text))
         (should (string-match-p "Broad answer" text)))
       (should (gethash "msg_user" opencode-rendered-message-ids))
       (should (gethash "msg_assistant" opencode-rendered-message-ids))))))

(ert-deftest opencode-event-sync-history-tail-targets-requested-message ()
  "Unified history sync uses message details for targeted repairs."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'plz)
              (lambda (_method url &rest plist)
                (should (string-match-p
                         "/session/ses_test/message/msg_target\\'" url))
                (funcall (plist-get plist :then)
                         (opencode-event-tests--stored-assistant
                          "msg_target" "prt_target" "target tail" t)))))
     (let ((opencode-api-url "http://localhost:4096"))
       (opencode-session--sync-history-tail
        "ses_test" "unit" nil '("msg_target") 2)
       (let ((text (opencode-event-tests--buffer-text)))
         (should (string-match-p "target tail" text))
         (should-not (string-match-p "other tail" text)))))))

(ert-deftest opencode-event-suspect-message-ids-prunes-stale-stream-state ()
  "Old unfinished stream state does not keep widening suspect reconciliation."
  (opencode-event-tests--with-session-buffer
   (let* ((opencode-session-suspect-stream-max-age 5)
          (now (float-time)))
     (puthash "msg_old" (list :last-delta (- now 60) :finished nil)
              opencode-session--stream-message-states)
     (puthash "msg_recent" (list :last-delta now :finished nil)
              opencode-session--stream-message-states)
     (puthash "msg_finished" (list :last-delta now :finished t)
              opencode-session--stream-message-states)
     (setq opencode-assistant-messages
           (list (cons "msg_active" (cons 'text (copy-marker (point))))))
     (should (equal '("msg_active" "msg_recent")
                    (sort (opencode-session--suspect-message-ids)
                          #'string<))))))

(ert-deftest opencode-event-provisional-delta-schedules-history-hydration ()
  "Delta-only live output schedules persisted-history hydration."
  (opencode-event-tests--with-session-buffer
   (let ((scheduled nil))
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (session-id delay)
                  (setq scheduled (list session-id delay)))))
       (opencode--handle-message
        (opencode-event-tests--text-delta "msg_1" "prt_1" "hello")))
     (should (equal '("ses_test" 1) scheduled)))))

(ert-deftest opencode-event-reasoning-is-hidden-when-disabled ()
  "Reasoning updates advance reducer state without rendering when disabled."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-show-reasoning nil)
         (inserted nil))
     (cl-letf (((symbol-function 'opencode--insert-reasoning-block)
                (lambda (&rest _args) (setq inserted t)))
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args))))
       (opencode--handle-message (opencode-event-tests--assistant "msg_1" nil))
       (opencode--handle-message
        (opencode-event-tests--reasoning-updated "msg_1" "prt_1" "hidden")))
     (should-not inserted)
     (should (string-empty-p (opencode-event-tests--buffer-text)))
     (should (= 6 (gethash "prt_1" opencode-part-sent))))))

(ert-deftest opencode-event-replayed-reasoning-is-hidden-when-disabled ()
  "Replay seeds reasoning state without output when reasoning is disabled."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-show-reasoning nil)
         (inserted nil))
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode--insert-reasoning-block)
                (lambda (&rest _args) (setq inserted t))))
       (opencode-session--render-complete-assistant-message
        '((info . ((id . "msg_1")
                   (sessionID . "ses_test")
                   (role . "assistant")))
          (parts . [((id . "prt_1")
                     (sessionID . "ses_test")
                     (messageID . "msg_1")
                     (type . "reasoning")
                     (text . "hidden"))]))))
     (should-not inserted)
     (should (string-empty-p (opencode-event-tests--buffer-text)))
     (should (= 6 (gethash "prt_1" opencode-part-sent)))
     (should (gethash "msg_1" opencode-rendered-message-ids)))))

(ert-deftest opencode-event-replay-user-assistant-history-in-order ()
  "Persisted user then assistant messages replay in chronological order."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-agents
         '(((name . "build")
            (model . ((providerID . "openai")
                      (modelID . "gpt-5.5"))))))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode--replay-user-request
      (opencode-event-tests--stored-user "msg_user" "Question"))
     (opencode-session--render-complete-assistant-message
      (opencode-event-tests--stored-assistant
       "msg_assistant" "prt_answer" "Answer" t)))
   (let ((text (opencode-event-tests--buffer-text)))
     (should (string-match-p "Question" text))
     (should (string-match-p "Answer" text))
     (should (< (string-match-p "Question" text)
                (string-match-p "Answer" text))))))

(ert-deftest opencode-event-replayed-user-message-is-marked-rendered ()
  "Replayed persisted user requests are not replayed by later reconciliation."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-agents
         '(((name . "build")
            (model . ((providerID . "openai")
                      (modelID . "gpt-5.5"))))))
   (cl-letf (((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode--replay-user-request
      (opencode-event-tests--stored-user "msg_user" "Question")))
   (should (gethash "msg_user" opencode-rendered-message-ids))
   (should (equal "user" (gethash "msg_user" opencode-message-roles)))
   (should (= 1 (opencode-session--visible-user-request-count)))))

(ert-deftest opencode-event-visible-user-count-ignores-empty-prompt ()
  "The current empty input prompt is not a persisted user request."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (opencode--show-prompt)
   (should (= 0 (opencode-session--visible-user-request-count)))
   (erase-buffer)
   (setq opencode-session-agents
         '(((name . "build")
            (model . ((providerID . "openai")
                      (modelID . "gpt-5.5"))))))
   (cl-letf (((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode--replay-user-request
      (opencode-event-tests--stored-user "msg_user" "Question")))
   (opencode--show-prompt)
   (should (= 1 (opencode-session--visible-user-request-count)))))

(ert-deftest opencode-event-reconcile-renders-external-user-in-order ()
  "Reconciliation renders external user messages and replies in history order."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-agents
         '(((name . "build")
            (model . ((providerID . "openai")
                      (modelID . "gpt-5.5"))))))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode--replay-user-request
      (opencode-event-tests--stored-user "msg_user_1" "Local prompt"))
     (opencode-session--render-complete-assistant-message
      (opencode-event-tests--stored-assistant
       "msg_assistant_1" "prt_answer_1" "Local answer" t))
     (opencode-session--reconcile-missed-history-messages
      (list (opencode-event-tests--stored-user "msg_user_1" "Local prompt")
            (opencode-event-tests--stored-assistant
             "msg_assistant_1" "prt_answer_1" "Local answer" t)
            (opencode-event-tests--stored-user "msg_user_2" "External prompt")
            (opencode-event-tests--stored-assistant
             "msg_assistant_2" "prt_answer_2" "External answer" t))))
   (let ((text (opencode-event-tests--buffer-text)))
     (should (= 1 (with-temp-buffer
                    (insert text)
                    (count-matches "Local prompt" (point-min) (point-max)))))
     (should (string-match-p "External prompt" text))
     (should (string-match-p "External answer" text))
     (should (< (string-match-p "External prompt" text)
                (string-match-p "External answer" text)))
     (should (gethash "msg_user_2" opencode-rendered-message-ids))
     (should (gethash "msg_assistant_2" opencode-rendered-message-ids)))))

(ert-deftest opencode-event-reconcile-renders-false-rendered-tail ()
  "Messages marked rendered but absent are rendered from persisted history."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-agents
         '(((name . "build")
            (model . ((providerID . "openai")
                      (modelID . "gpt-5.5"))))))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode--replay-user-request
      (opencode-event-tests--stored-user "msg_user_1" "Visible prompt"))
     (opencode-session--render-complete-assistant-message
      (opencode-event-tests--stored-assistant
       "msg_assistant_1" "prt_answer_1" "Visible answer" t))
     (opencode--show-prompt)
     (puthash "msg_user_2" t opencode-rendered-message-ids)
     (puthash "msg_user_2" "user" opencode-message-roles)
     (puthash "msg_assistant_2" t opencode-rendered-message-ids)
     (puthash "msg_assistant_2" "assistant" opencode-message-roles)
     (opencode-session--reconcile-missed-history-messages
      (list (opencode-event-tests--stored-user "msg_user_1" "Visible prompt")
            (opencode-event-tests--stored-assistant
             "msg_assistant_1" "prt_answer_1" "Visible answer" t)
            (opencode-event-tests--stored-user "msg_user_2" "Missing prompt")
            (opencode-event-tests--stored-assistant
             "msg_assistant_2" "prt_answer_2" "Missing answer" t))))
   (let ((text (opencode-event-tests--buffer-text)))
     (should (string-match-p "Missing prompt" text))
     (should (string-match-p "Missing answer" text))
     (should (< (string-match-p "Missing prompt" text)
                (string-match-p "Missing answer" text))))))

(ert-deftest opencode-event-reconcile-skips-validated-visible-scan ()
  "Already validated rendered messages do not rescan the whole buffer."
  (opencode-event-tests--with-session-buffer
   (let ((message (opencode-event-tests--stored-assistant
                   "msg_1" "prt_1" "Visible answer" t))
         (visible-calls 0))
     (puthash "msg_1" t opencode-rendered-message-ids)
     (opencode-session--mark-history-validated "msg_1")
     (cl-letf (((symbol-function 'opencode-session--message-visible-p)
                (lambda (&rest _args)
                  (setq visible-calls (1+ visible-calls))
                  t))
               ((symbol-function 'opencode-session--reconcile-rendered-message-tail)
                (lambda (&rest _args)
                  (error "validated messages should not repair tails"))))
       (opencode-session--reconcile-completed-assistant-history message))
     (should (= 0 visible-calls))
     (should (gethash "msg_1" opencode-rendered-message-ids)))))

(ert-deftest opencode-event-reconcile-renders-missing-tool-only-message ()
  "Tool-only messages marked rendered are repaired when no tool block is visible."
  (opencode-event-tests--with-session-buffer
   (let ((message '((info . ((id . "msg_tool")
                             (sessionID . "ses_test")
                             (role . "assistant")
                             (time . ((completed . 1)))))
                    (parts . (((id . "prt_tool")
                               (sessionID . "ses_test")
                               (messageID . "msg_tool")
                               (callID . "call_bash")
                               (type . "tool")
                               (tool . "bash")
                               (state . ((status . "completed")
                                         (input . ((command . "echo ok")))
                                         (output . "ok")))))))))
     (puthash "msg_tool" t opencode-rendered-message-ids)
     (puthash "call_bash" t opencode--tool-calls-displayed)
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity))
       (opencode-session--reconcile-completed-assistant-history message))
     (let ((text (opencode-event-tests--buffer-text)))
       (should (string-match-p (regexp-quote "$ echo ok") text))
       (should (string-match-p (regexp-quote "ok") text))
       (should (gethash "msg_tool" opencode-rendered-message-ids))
       (should (opencode-session--history-validated-p "msg_tool"))))))

(ert-deftest opencode-event-reconcile-skips-visible-legacy-user-prompts ()
  "Legacy visible user prompts are marked rendered instead of duplicated."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-agents
         '(((name . "build")
            (model . ((providerID . "openai")
                      (modelID . "gpt-5.5"))))))
   (cl-letf (((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode--replay-user-request
      (opencode-event-tests--stored-user "msg_user_1" "Already visible")))
   (setq opencode-rendered-message-ids (make-hash-table :test 'equal)
         opencode-message-roles (make-hash-table :test 'equal))
   (cl-letf (((symbol-function 'comint-send-input) (lambda () nil)))
     (opencode-session--reconcile-missed-history-messages
      (list (opencode-event-tests--stored-user "msg_user_1" "Already visible")
            (opencode-event-tests--stored-user "msg_user_2" "External prompt"))))
   (let ((text (opencode-event-tests--buffer-text)))
     (should (= 1 (with-temp-buffer
                    (insert text)
                    (count-matches "Already visible" (point-min) (point-max)))))
     (should (string-match-p "External prompt" text))
     (should (gethash "msg_user_1" opencode-rendered-message-ids))
     (should (gethash "msg_user_2" opencode-rendered-message-ids)))))

(ert-deftest opencode-event-user-role-buffered-text-is-dropped ()
  "Buffered backend text for user-role messages is discarded, not echoed."
  (opencode-event-tests--with-state
   (puthash "prt_1" "msg_user" opencode-part-message)
   (puthash "prt_1" "text" opencode-part-type)
   (puthash "prt_1" "do not echo" opencode-part-text)
   (puthash "msg_user" "user" opencode-message-roles)
   (opencode-session--flush-buffered-message-parts "msg_user")
   (should-not (gethash "prt_1" opencode-part-message))
   (should-not (gethash "prt_1" opencode-part-type))
   (should-not (gethash "prt_1" opencode-part-text))
   (should-not (gethash "prt_1" opencode-part-sent))))

(ert-deftest opencode-event-question-tool-completion-clears-request ()
  "Completed question tools clear matching pending question requests."
  (opencode-event-tests--with-state
   (setq opencode-session-pending-questions
         (cons "que_test" []))
   (opencode-session--record-question-tool
    "que_test" '((messageID . "msg_1") (callID . "call_1")))
   (should (= 1 (hash-table-count opencode--pending-question-tools)))
   (opencode-session--clear-question-for-tool
    '((tool . "question")
      (messageID . "msg_1")
      (callID . "call_1")
      (state . ((status . "completed")))))
   (should-not opencode-session-pending-questions)
   (should (= 0 (hash-table-count opencode--pending-question-tools)))))

(ert-deftest opencode-event-completed-question-tool-renders-in-history ()
  "Completed question tools from stored history remain visible."
  (opencode-event-tests--with-session-buffer
   (opencode-session--render-complete-assistant-message
    `((info . ((id . "msg_question")
	       (sessionID . "ses_test")
	       (role . "assistant")
	       (time . ((completed . 1)))))
      (parts . [,(opencode-event-tests--completed-question-tool-part)])))
   (let ((text (opencode-event-tests--buffer-text)))
     (should (string-match-p (regexp-quote "Are you ready?") text))
     (should (string-match-p (regexp-quote "Answer: Ready now") text))
     (should-not opencode-session-pending-questions)
     (should (gethash "call_question" opencode--tool-calls-displayed)))))

(ert-deftest opencode-event-completed-question-tool-renders-if-event-missed ()
  "Completed question tool updates render when question.asked was missed."
  (opencode-event-tests--with-session-buffer
   (let ((part (opencode-event-tests--completed-question-tool-part)))
     (opencode-session--update-part part nil "tool")
     (let ((text (opencode-event-tests--buffer-text)))
       (should (string-match-p (regexp-quote "Are you ready?") text))
       (should (string-match-p (regexp-quote "Answer: Ready now") text))
       (should-not opencode-session-pending-questions))
     (let ((first-render (opencode-event-tests--buffer-text)))
       (opencode-session--update-part part nil "tool")
       (should (equal first-render
                      (opencode-event-tests--buffer-text)))))))

(ert-deftest opencode-event-stale-question-list-after-tool-completion-is-ignored ()
  "Question recovery must not resurrect a request after its tool completed."
  (opencode-event-tests--with-state
   (setq opencode-api-url "http://localhost:4098")
   (opencode-session--clear-question-for-tool
    '((tool . "question")
      (messageID . "msg_1")
      (callID . "call_1")
      (state . ((status . "completed")))))
   (let ((queued nil)
         (question '((id . "que_1")
                     (sessionID . "ses_test")
                     (questions . (((question . "Proceed?")
                                    (options . (((label . "Yes")))))))
                     (tool . ((messageID . "msg_1")
                              (callID . "call_1"))))))
     (cl-letf (((symbol-function 'opencode--auth-header)
                (lambda () (cons "authorization" "Basic test")))
	       ((symbol-function 'opencode--queue-questions)
                (lambda (&rest _args) (setq queued t)))
	       ((symbol-function 'plz)
                (lambda (&rest _args) (list question))))
       (should-not (opencode-session--sync-pending-question))
       (should-not queued)
       (should-not opencode-session-pending-questions)))))

(ert-deftest opencode-event-recovers-question-from-question-list ()
  "Question recovery queues a live backend question when its event was missed."
  (opencode-event-tests--with-state
   (setq opencode-api-url "http://localhost:4098")
   (let* ((queued nil)
          (question '((id . "que_1")
                      (sessionID . "ses_test")
                      (questions . (((question . "Proceed?")
                                     (options . (((label . "Yes")))))))
                      (tool . ((messageID . "msg_1")
			       (callID . "call_1"))))))
     (cl-letf (((symbol-function 'opencode--auth-header)
                (lambda () (cons "authorization" "Basic test")))
	       ((symbol-function 'opencode--queue-questions)
                (lambda (buffer question-id questions)
                  (setq queued (list buffer question-id questions))
                  (with-current-buffer buffer
                    (setq opencode-session-pending-questions
                          (cons question-id questions)))))
	       ((symbol-function 'plz)
                (lambda (&rest _args) (list question))))
       (should (opencode-session--sync-pending-question))
       (should (equal "que_1" (cadr queued)))
       (should (vectorp (caddr queued)))
       (should (vectorp (alist-get 'options (aref (caddr queued) 0))))
       (should (equal "que_1"
                      (gethash (opencode-session--question-tool-key
                                "msg_1" "call_1")
			       opencode--pending-question-tools)))))))

(ert-deftest opencode-event-active-question-queue-is-deduped ()
  "Active question prompts are still marked pending and not re-queued."
  (opencode-event-tests--with-session-buffer
   (let* ((questions [((question . "Proceed?")
		       (options . [((label . "Yes"))]))])
          (output-count 0)
          (prompt-count 0))
     (cl-letf (((symbol-function 'opencode--buffer-active-p)
                (lambda (_buffer) t))
	       ((symbol-function 'opencode--output-questions)
                (lambda (&rest _args) (cl-incf output-count)))
	       ((symbol-function 'opencode--prompt-questions)
                (lambda (&rest _args) (cl-incf prompt-count))))
       (opencode--queue-questions (current-buffer) "que_1" questions)
       (should (equal "que_1" (car opencode-session-pending-questions)))
       (should (eq questions (cdr opencode-session-pending-questions)))
       (should (= 1 output-count))
       (should (= 1 prompt-count))
       (opencode--queue-questions (current-buffer) "que_1" questions)
       (should (= 1 output-count))
       (should (= 1 prompt-count))
       (setq opencode-session-pending-questions nil)
       (opencode--queue-questions (current-buffer) "que_1" questions)
       (should (= 1 output-count))
       (should (= 1 prompt-count))
       (should (equal "que_1" (car opencode-session-pending-questions)))))))

(ert-deftest opencode-event-question-recovery-stops-after-success ()
  "Question recovery clears its timer and does not retry after success."
  (opencode-event-tests--with-session-buffer
   (let (timer-fn timer-args
		  (sync-calls 0)
		  (reschedules 0))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_delay _repeat fn &rest args)
                  (setq timer-fn fn
                        timer-args args)
                  (timer-create)))
	       ((symbol-function 'opencode-session--sync-pending-question)
                (lambda ()
                  (cl-incf sync-calls)
                  t)))
       (opencode-session--schedule-question-recover "ses_test" 0.01 3)
       (should (timerp opencode--question-recover-timer))
       (cl-letf (((symbol-function 'opencode-session--schedule-question-recover)
                  (lambda (&rest _args)
                    (cl-incf reschedules))))
         (apply timer-fn timer-args))
       (should (= 1 sync-calls))
       (should (= 0 reschedules))
       (should-not opencode--question-recover-timer)))))

(ert-deftest opencode-event-question-recovery-stops-after-attempts-exhausted ()
  "Question recovery does not reschedule after the final retry attempt."
  (opencode-event-tests--with-session-buffer
   (let (timer-fn timer-args
		  (sync-calls 0)
		  (reschedules 0))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_delay _repeat fn &rest args)
                  (setq timer-fn fn
                        timer-args args)
                  (timer-create)))
	       ((symbol-function 'opencode-session--sync-pending-question)
                (lambda ()
                  (cl-incf sync-calls)
                  nil)))
       (opencode-session--schedule-question-recover "ses_test" 0.01 1)
       (cl-letf (((symbol-function 'opencode-session--schedule-question-recover)
                  (lambda (&rest _args)
                    (cl-incf reschedules))))
         (apply timer-fn timer-args))
       (should (= 1 sync-calls))
       (should (= 0 reschedules))
       (should-not opencode--question-recover-timer)))))

(ert-deftest opencode-event-question-recovery-uses-configured-backoff ()
  "Question recovery retries with the configured backoff interval."
  (opencode-event-tests--with-session-buffer
   (let (timer-fn timer-args
                  reschedule)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_delay _repeat fn &rest args)
                  (setq timer-fn fn
                        timer-args args)
                  (timer-create)))
               ((symbol-function 'opencode-session--sync-pending-question)
                (lambda () nil)))
       (let ((opencode-session-question-recover-interval 2.5))
         (opencode-session--schedule-question-recover "ses_test" 0.01 3)
         (cl-letf (((symbol-function 'opencode-session--schedule-question-recover)
                    (lambda (&rest args)
                      (setq reschedule args))))
           (apply timer-fn timer-args)))
       (should (equal '("ses_test" 2.5 2) reschedule))
       (should-not opencode--question-recover-timer)))))

(ert-deftest opencode-event-status-busy-starts-question-recovery ()
  "Busy status events poll /question when question.asked was missed."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-api-url "http://localhost:4098")
         calls)
     (cl-letf (((symbol-function 'opencode-session--schedule-question-recover)
                (lambda (session-id &optional delay attempts)
                  (push (list session-id delay attempts) calls)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--status "ses_test" "busy")))
     (should (equal '(("ses_test" 0.25 nil)) calls)))))

(ert-deftest opencode-event-question-recovery-is-not-duplicated ()
  "Status events do not restart an already active question recovery timer."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-api-url "http://localhost:4098")
         (opencode--question-recover-timer (timer-create))
         calls)
     (unwind-protect
         (cl-letf (((symbol-function 'opencode-session--schedule-question-recover)
                    (lambda (&rest args) (push args calls)))
                   ((symbol-function 'opencode-session--schedule-status-poll)
                    (lambda (&rest _args))))
           (opencode--handle-message
            (opencode-event-tests--status "ses_test" "busy"))
           (should-not calls))
       (when (timerp opencode--question-recover-timer)
         (cancel-timer opencode--question-recover-timer))))))

(ert-deftest opencode-event-question-recovery-honors-cooldown ()
  "Status events do not restart question recovery during cooldown."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-api-url "http://localhost:4098")
         (opencode-session-question-recover-cooldown 30)
         (opencode-session--question-recover-last-finished (float-time))
         calls)
     (cl-letf (((symbol-function 'opencode-session--schedule-question-recover)
                (lambda (&rest args) (push args calls)))
               ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--status "ses_test" "busy"))
       (should-not calls)))))

(ert-deftest opencode-event-question-reply-event-clears-alternate-id ()
  "Question reply/reject events clear pending questions by any server id key."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-pending-questions (cons "que_1" []))
   (opencode--handle-message
    '((type . "question.replied")
      (properties . ((sessionID . "ses_test")
                     (id . "que_1")))))
   (should-not opencode-session-pending-questions)))

(ert-deftest opencode-event-normalize-question-options ()
  "Question replay/sync converts nested options to vectors for the UI."
  (let* ((questions '(((question . "Proceed?")
                       (options . (((label . "Yes"))
                                   ((label . "No")))))))
         (normalized (opencode-session--normalize-questions questions)))
    (should (vectorp normalized))
    (should (vectorp (alist-get 'options (aref normalized 0))))))

(ert-deftest opencode-event-permission-precedes-question-prompt ()
  "Permissions are prompted before queued questions, matching upstream."
  (opencode-event-tests--with-state
   (setq opencode-session-pending-permission
         (list (list :id "perm_1" :type "bash" :title "Permission"))
         opencode-session-pending-questions (cons "que_1" []))
   (let (prompted)
     (cl-letf (((symbol-function 'opencode-permission--prompt)
                (lambda (&rest _args) (setq prompted 'permission)))
	       ((symbol-function 'opencode--prompt-questions)
                (lambda (&rest _args) (setq prompted 'question))))
       (opencode-respond-permission))
     (should (eq prompted 'permission))
     (should opencode-session-pending-questions))))

(ert-deftest opencode-event-permission-title-refreshes-from-tool-input ()
  "Later matching tool input enriches a pending permission request."
  (opencode-event-tests--with-state
   (setq opencode-session-pending-permission
         (list (list :id "perm_1"
                     :session-id "ses_test"
                     :type "bash"
                     :patterns nil
                     :title "Shell command: ")))
   (puthash (opencode-session--tool-key "msg_1" "call_1")
            "perm_1" opencode--pending-permission-tools)
   (opencode-session--refresh-permission-for-tool
    '((tool . "bash")
      (messageID . "msg_1")
      (callID . "call_1")
      (state . ((status . "running")
                (input . ((command . "pwd")))))))
   (should (equal "Shell command: pwd"
                  (plist-get (car opencode-session-pending-permission)
                             :title)))))

(ert-deftest opencode-event-permission-reply-event-clears-pending ()
  "Permission reply/reject events clear local permission queues."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-pending-permission
         (list (list :id "perm_1" :session-id "ses_test")))
   (puthash (opencode-session--tool-key "msg_1" "call_1")
            "perm_1" opencode--pending-permission-tools)
   (opencode--handle-message
    '((type . "permission.rejected")
      (properties . ((sessionID . "ses_test")
                     (requestID . "perm_1")))))
   (should-not opencode-session-pending-permission)
   (should (= 0 (hash-table-count opencode--pending-permission-tools)))))

(ert-deftest opencode-event-permission-reply-reveals-queued-question ()
  "After permission reply, the next blocker is the queued question."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-pending-permission
         (list (list :id "perm_1" :session-id "ses_test"))
         opencode-session-pending-questions (cons "que_1" []))
   (let (prompted)
     (cl-letf (((symbol-function 'opencode--prompt-questions)
                (lambda (question-id _questions)
                  (setq prompted question-id))))
       (opencode--handle-message
        '((type . "permission.replied")
          (properties . ((sessionID . "ses_test")
                         (requestID . "perm_1")))))
       (opencode-respond-permission))
     (should (equal "que_1" prompted))
     (should-not opencode-session-pending-questions))))

(ert-deftest opencode-event-shell-echo-is-stripped-on-first-text-chunk ()
  "Completed bash output is stripped from the next assistant text prefix."
  (opencode-event-tests--with-state
   (opencode-session--stash-shell-echo
    '((tool . "bash")
      (messageID . "msg_1")
      (state . ((status . "completed")
                (output . "\nresult\n")))))
   (should (equal "explanation"
                  (opencode-session--strip-shell-echo
                   "msg_1" "result\nexplanation")))
   (should-not (gethash "msg_1" opencode-shell-echo))))

(ert-deftest opencode-event-shell-events-dedupe-before-legacy-bash-part ()
  "First-class shell start claims a call before legacy bash tool updates."
  (opencode-event-tests--with-session-buffer
   (let (inserted)
     (cl-letf (((symbol-function 'opencode--insert-tool-block)
                (lambda (tool input) (push (list tool input) inserted)))
	       ((symbol-function 'opencode--maybe-insert-tool-output)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--shell-started "call_1" "echo hi"))
       (opencode--handle-message
        (opencode-event-tests--tool-updated
         "msg_1" "prt_1" "call_1" "bash" "running"
         '((command . "echo hi"))))
       (should (= 1 (length inserted)))
       (should (eq 'shell-event
                   (opencode-session--shell-call-source "call_1")))))))

(ert-deftest opencode-event-shell-ended-renders-output-once ()
  "First-class shell ended events render output through the tool output path."
  (opencode-event-tests--with-session-buffer
   (let (outputs)
     (cl-letf (((symbol-function 'opencode--insert-tool-block)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--maybe-insert-tool-output)
                (lambda (part) (push part outputs))))
       (opencode--handle-message
        (opencode-event-tests--shell-started "call_1" "echo hi"))
       (opencode--handle-message
        (opencode-event-tests--shell-ended "call_1" "hi\n"))
       (should (= 1 (length outputs)))
       (should (equal "hi\n" (map-nested-elt (car outputs)
                                             '(state output))))))))

(ert-deftest opencode-event-legacy-bash-part-dedupes-later-shell-event ()
  "Legacy bash tool updates claim a call before shell start events."
  (opencode-event-tests--with-session-buffer
   (let (inserted)
     (cl-letf (((symbol-function 'opencode--insert-tool-block)
                (lambda (tool input) (push (list tool input) inserted)))
	       ((symbol-function 'opencode--maybe-insert-tool-output)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--tool-updated
         "msg_1" "prt_1" "call_1" "bash" "running"
         '((command . "echo hi"))))
       (opencode--handle-message
        (opencode-event-tests--shell-started "call_1" "echo hi"))
       (should (= 1 (length inserted)))
       (should (eq 'legacy-tool
                   (opencode-session--shell-call-source "call_1")))))))

(ert-deftest opencode-event-final-tool-update-synthesizes-missed-start ()
  "Completed/error tool updates render a tool block if running was missed."
  (opencode-event-tests--with-session-buffer
   (let (inserted)
     (cl-letf (((symbol-function 'opencode--insert-tool-block)
                (lambda (tool input) (push (list tool input) inserted)))
	       ((symbol-function 'opencode--maybe-insert-tool-output)
                (lambda (&rest _args))))
       (opencode--handle-message
        (opencode-event-tests--tool-updated
         "msg_1" "prt_1" "call_1" "edit" "error"
         '((filePath . "a.txt"))))
       (should (= 1 (length inserted)))
       (should (equal "edit" (caar inserted)))))))

(ert-deftest opencode-event-replay-running-tool-keeps-session-busy ()
  "Replaying a running tool preserves busy status until status polling settles."
  (opencode-event-tests--with-session-buffer
   (let (poll-session inserted)
     (cl-letf (((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (session-id &optional _delay)
                  (setq poll-session session-id)))
	       ((symbol-function 'opencode--insert-tool-block)
                (lambda (tool input) (push (list tool input) inserted)))
	       ((symbol-function 'opencode--maybe-insert-tool-output)
                (lambda (&rest _args))))
       (opencode-session--render-complete-assistant-message
        '((info . ((id . "msg_1")
                   (sessionID . "ses_test")
                   (role . "assistant")))
          (parts . [((id . "prt_1")
                     (sessionID . "ses_test")
                     (messageID . "msg_1")
                     (type . "tool")
                     (tool . "bash")
                     (callID . "call_1")
                     (state . ((status . "running")
			       (input . ((command . "pwd"))))))]))))
     (should (equal "busy" opencode-session-status))
     (should (equal "ses_test" poll-session))
     (should (= 1 (length inserted))))))

(ert-deftest opencode-event-session-error-flushes-active-output ()
  "Session error events flush active output once and show the error."
  (opencode-event-tests--with-session-buffer
   (insert "partial")
   (setq opencode-assistant-messages
         `(("msg_1" text . ,(point-min))))
   (cl-letf (((symbol-function 'opencode--render-region)
              (lambda (&rest _args))))
     (opencode--handle-message
      '((type . "session.error")
        (properties . ((sessionID . "ses_test")
		       (error . ((data . ((message . "boom"))))))))))
   (should (string-match-p "\\[interrupted\\]"
                           (opencode-event-tests--buffer-text)))
   (should (string-match-p "boom" (opencode-event-tests--buffer-text)))
   (should-not opencode-assistant-messages)))

(ert-deftest opencode-event-session-updated-renames-only-target-buffer ()
  "Session update events rename only the matching session buffer."
  (opencode-event-tests--with-two-session-buffers
   (with-current-buffer buffer-a (rename-buffer "*old-a*" t))
   (with-current-buffer buffer-b (rename-buffer "*old-b*" t))
   (opencode--handle-message
    '((type . "session.updated")
      (properties . ((info . ((id . "ses_a")
                              (projectID . "proj_1")
                              (title . "New Title")))))))
   (should (equal "*OpenCode: New Title*" (buffer-name buffer-a)))
   (should (equal "*old-b*" (buffer-name buffer-b)))))

(ert-deftest opencode-event-session-deleted-kills-only-target-process ()
  "Session deletion kills only the deleted session's process."
  (opencode-event-tests--with-two-session-buffers
   (let ((proc-a (get-buffer-process buffer-a))
         (proc-b (get-buffer-process buffer-b))
         deleted)
     (let ((opencode-session-deleted-functions
            (list (lambda (session-id) (setq deleted session-id)))))
       (opencode--handle-message
        '((type . "session.deleted")
          (properties . ((info . ((id . "ses_a")
                                  (projectID . "proj_1"))))))))
     (should (equal "ses_a" deleted))
     (should-not (process-live-p proc-a))
     (should (process-live-p proc-b)))))

(ert-deftest opencode-event-file-edited-hook-runs-on-completed-write ()
  "Completed file-writing tools run `opencode-file-edited-functions'."
  (let (called)
    (let ((opencode-file-edited-functions
           (list (lambda (file ranges) (setq called (list file ranges))))))
      (opencode--handle-message
       '((type . "message.part.updated")
         (properties . ((sessionID . "ses_test")
                        (part . ((sessionID . "ses_test")
                                 (type . "tool")
                                 (tool . "write")
                                 (state . ((status . "completed")
                                           (input . ((filePath . "/tmp/a.txt"))))))))))))
    (should (equal '("/tmp/a.txt" nil) called))))

(ert-deftest opencode-event-session-idle-runs-files-finished-hooks-once ()
  "Summary diff files are handed to finished-editing hooks once on idle."
  (opencode-event-tests--with-session-buffer
   (let ((file (make-temp-file "opencode-event-test"))
         calls)
     (unwind-protect
         (let ((opencode-files-finished-editing-functions
                (list (lambda (files) (push files calls))))
	       (opencode-project-files-finished-editing-functions nil))
           (cl-letf (((symbol-function 'opencode--show-prompt) (lambda ()))
                     ((symbol-function 'opencode--toast-show) (lambda (&rest _args)))
                     ((symbol-function 'opencode--buffer-active-p) (lambda (&rest _args) t))
                     ((symbol-function 'plz)
                      (lambda (_method _url &rest plist)
                        (funcall (plist-get plist :then)
                                 '((id . "ses_test")
                                   (title . "Session"))))))
             (opencode--handle-message
              `((type . "message.updated")
                (properties . ((sessionID . "ses_test")
			       (info . ((id . "msg_1")
                                        (sessionID . "ses_test")
                                        (role . "assistant")
                                        (summary . ((diffs . [((file . ,file))])))
                                        (tokens . ((input . 0)
                                                   (output . 0)
                                                   (reasoning . 0)
                                                   (cache . ((read . 0)
                                                             (write . 0)))))))))))
             (opencode--handle-message
              '((type . "session.idle")
                (properties . ((sessionID . "ses_test")))))
             (opencode--handle-message
              '((type . "session.idle")
                (properties . ((sessionID . "ses_test"))))))
           (should (equal (list (list file)) calls)))
       (when (file-exists-p file)
         (delete-file file))))))

(ert-deftest opencode-event-interrupted-flush-is-one-shot ()
  "Interrupted active assistant output is marked once and then cleared."
  (opencode-event-tests--with-session-buffer
   (insert "partial")
   (setq opencode-assistant-messages
         `(("msg_1" text . ,(point-min))))
   (cl-letf (((symbol-function 'opencode--render-region)
              (lambda (&rest _args))))
     (opencode-session--flush-interrupted)
     (opencode-session--flush-interrupted))
   (should (= 1 (how-many "\\[interrupted\\]" (point-min) (point-max))))
   (should-not opencode-assistant-messages)))

(ert-deftest opencode-event-interrupted-message-drops-late-deltas ()
  "Late deltas for an interrupted message are ignored."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'opencode-session--schedule-reconcile)
              (lambda (&rest _args))))
     (opencode--handle-message (opencode-event-tests--assistant "msg_1"))
     (opencode--handle-message
      (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" "partial"))
     (opencode-session--flush-interrupted)
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" " late")))
   (should (string-match-p "partial" (opencode-event-tests--buffer-text)))
   (should (string-match-p "\\[interrupted\\]"
                           (opencode-event-tests--buffer-text)))
   (should-not (string-match-p "late" (opencode-event-tests--buffer-text)))
   (should-not opencode-assistant-messages)))

(ert-deftest opencode-event-idle-during-stream-waits-for-quiet-period ()
  "Idle events in the middle of a stream defer finalization and prompt display."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (let ((opencode-session-idle-finalize-delay 60)
         (opencode-session-status "busy")
         prompt-shown
         poll-session)
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (session-id &rest _args)
                  (setq poll-session session-id)))
	       ((symbol-function 'opencode--show-prompt)
                (lambda () (setq prompt-shown t))))
       (unwind-protect
           (progn
             (opencode--handle-message (opencode-event-tests--assistant "msg_1"))
             (opencode--handle-message
              (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
             (opencode--handle-message
              (opencode-event-tests--text-delta "msg_1" "prt_1" "Since"))
             (opencode--handle-message (opencode-event-tests--status
                                        "ses_test" "idle"))
             (opencode--handle-message
              '((type . "session.idle")
                (properties . ((sessionID . "ses_test")))))
             (should (equal "idle" opencode-session-status))
             (should (timerp opencode-session--idle-finalize-timer))
             (should-not prompt-shown)
             (opencode--handle-message
              (opencode-event-tests--text-delta "msg_1" "prt_1" " it's"))
             (should-not (timerp opencode-session--idle-finalize-timer))
             (should (equal "busy" opencode-session-status))
             (should (equal "ses_test" poll-session))
             (should (string-match-p "Since it's"
                                     (opencode-event-tests--buffer-text)))
             (should-not prompt-shown))
         (when (timerp opencode-session--idle-finalize-timer)
           (cancel-timer opencode-session--idle-finalize-timer)))))))

(ert-deftest opencode-event-abort-session-uses-session-id-variable ()
  "Aborting a session builds the abort URL from the buffer-local id."
  (opencode-event-tests--with-state
   (setq opencode-api-url "http://localhost:4098")
   (let (sent-path status-session)
     (cl-letf (((symbol-function 'opencode--auth-header)
                (lambda () (cons "authorization" "Basic test")))
	       ((symbol-function 'opencode-session--flush-interrupted)
                (lambda ()))
	       ((symbol-function 'opencode-session--set-status)
                (lambda (session-id _status)
                  (setq status-session session-id)))
	       ((symbol-function 'opencode--show-prompt)
                (lambda ()))
	       ((symbol-function 'plz)
                (lambda (_method url &rest plist)
                  (setq sent-path
                        (string-remove-prefix opencode-api-url url))
                  (funcall (plist-get plist :then) t))))
       (opencode-abort-session))
     (should (equal "/session/ses_test/abort" sent-path))
     (should (equal "ses_test" status-session)))))

(ert-deftest opencode-event-status-type-supports-alists-and-hashes ()
  "Status polling decodes both alist and hash-table response shapes."
  (let ((table (make-hash-table :test 'equal))
        (status (make-hash-table :test 'equal)))
    (puthash "type" "busy" status)
    (puthash "ses_test" status table)
    (should (equal "idle"
                   (opencode-session--status-type-for-session
                    '(("ses_test" . ((type . "idle")))) "ses_test")))
    (should (equal "busy"
                   (opencode-session--status-type-for-session
                    table "ses_test")))))

(ert-deftest opencode-event-missing-polled-status-means-idle-when-inactive ()
  "A missing inactive session in `/session/status' is idle and reconciles."
  (let (status-call reconcile-call)
    (cl-letf (((symbol-function 'opencode-session--set-status)
               (lambda (session-id status)
                 (setq status-call (list session-id status))))
              ((symbol-function 'opencode-session--reconcile-pending)
               (lambda (session-id &optional _callback)
                 (setq reconcile-call session-id))))
      (should (equal "idle"
                     (opencode-session--apply-polled-status "ses_test" nil)))
      (should (equal '("ses_test" "idle") status-call))
      (should (equal "ses_test" reconcile-call)))))

(ert-deftest opencode-event-missing-polled-status-reconciles-active-status ()
  "An empty `/session/status' response reconciles a possibly completed stream."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "busy")
   (let (status-call suspect-call callback-present scheduled-timers)
     (cl-letf (((symbol-function 'opencode-session--set-status)
                (lambda (session-id status)
                  (setq status-call (list session-id status))))
	       ((symbol-function 'opencode-session--schedule-suspect-reconcile)
                (lambda (session-id reason delay &optional callback)
                  (setq suspect-call (list session-id reason delay)
                        callback-present (functionp callback))))
	       ((symbol-function 'run-at-time)
                (lambda (delay repeat function &rest args)
                  (push (list delay repeat function args) scheduled-timers)
                  (timer-create))))
       (should (equal "busy"
                      (opencode-session--apply-polled-status "ses_test" nil)))
       (should-not status-call)
       (should (equal '("ses_test" "missing-polled-status" 0.1)
                      suspect-call))
       (should callback-present)
       (should (seq-find
                (lambda (timer)
                  (and (equal 0.5 (car timer))
		       (eq #'opencode-session--finish-after-missing-polled-status-in-buffer
                           (nth 2 timer))))
                scheduled-timers))))))

(ert-deftest opencode-event-missing-polled-status-finish-callback-prompts ()
  "Missing polled status can finish after reconciliation clears output."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "busy"
         opencode-assistant-messages '(("msg_1" text . 1)))
   (let (status-call prompt-called)
     (cl-letf (((symbol-function 'opencode-session--set-status)
                (lambda (session-id status)
                  (setq status-call (list session-id status))
                  (setq opencode-session-status status)))
	       ((symbol-function 'opencode-session--finalize-active-output)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--show-prompt)
                (lambda () (setq prompt-called t))))
       (setq opencode-assistant-messages nil)
       (opencode-session--finish-after-missing-polled-status)
       (should (equal '("ses_test" "idle") status-call))
       (should prompt-called)))))

(ert-deftest opencode-event-idle-finish-callback-prompts-after-reconcile ()
  "A stale idle session prompts after reconciliation clears active output."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "idle")
   (let (status-call prompt-called)
     (cl-letf (((symbol-function 'opencode-session--set-status)
                (lambda (session-id status)
                  (setq status-call (list session-id status))
                  (setq opencode-session-status status)))
	       ((symbol-function 'opencode-session--finalize-active-output)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--show-prompt)
                (lambda () (setq prompt-called t))))
       (setq opencode-assistant-messages nil)
       (opencode-session--finish-after-missing-polled-status)
       (should-not status-call)
       (should prompt-called)))))

(ert-deftest opencode-event-stream-after-idle-restores-busy-without-idle-timer ()
  "Late stream output after idle reopens status polling even without a timer."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "idle"
         opencode-session--idle-finalize-timer nil)
   (let (status-call poll-call)
     (cl-letf (((symbol-function 'opencode-session--set-status)
                (lambda (session-id status)
                  (setq status-call (list session-id status))
                  (setq opencode-session-status status)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (session-id &optional _delay)
                  (setq poll-call session-id))))
       (opencode-session--note-stream-event)
       (should (equal '("ses_test" "busy") status-call))
       (should (equal "ses_test" poll-call))))))

(ert-deftest opencode-event-show-prompt-inserts-block-spacing ()
  "Prompts do not attach to the previous assistant sentence."
  (opencode-event-tests--with-session-buffer
   (when-let ((process (get-buffer-process (current-buffer))))
     (set-process-buffer process nil)
     (delete-process process))
   (insert "answer.")
   (opencode--show-prompt)
   (should (equal "answer.\n\n> "
                  (opencode-event-tests--buffer-text)))
   (opencode--show-prompt)
   (should (equal "answer.\n\n> "
                  (opencode-event-tests--buffer-text)))
   (erase-buffer)
   (opencode--show-prompt)
   (should (equal "> "
                  (opencode-event-tests--buffer-text)))))

(ert-deftest opencode-event-present-busy-status-does-not-reconcile ()
  "A present busy status keeps the session busy without reconciliation."
  (let (status-call reconcile-call)
    (cl-letf (((symbol-function 'opencode-session--set-status)
               (lambda (session-id status)
                 (setq status-call (list session-id status))))
              ((symbol-function 'opencode-session--reconcile-pending)
               (lambda (session-id &optional _callback)
                 (setq reconcile-call session-id))))
      (should (equal "busy"
                     (opencode-session--apply-polled-status
                      "ses_test" '(("ses_test" . ((type . "busy")))))))
      (should (equal '("ses_test" "busy") status-call))
      (should-not reconcile-call))))

(ert-deftest opencode-event-busy-status-schedules-reconcile-with-no-active-output ()
  "Busy polling schedules hydration when no live output is active."
  (opencode-event-tests--with-session-buffer
   (let (scheduled)
     (cl-letf (((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (session-id delay)
                  (setq scheduled (list session-id delay)))))
       (should (equal "busy"
                      (opencode-session--apply-polled-status
                       "ses_test" '(("ses_test" . ((type . "busy"))))))))
     (should (equal '("ses_test" 0.2) scheduled)))))

(ert-deftest opencode-event-busy-status-reconciles-quiet-active-output ()
  "Busy polling hydrates missed running tools when active output is stale."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-session-stream-quiet-reconcile-delay 2.0)
         scheduled)
     (setq opencode-session-status "busy"
           opencode-assistant-messages '(("msg_1" text . 1)))
     (puthash "msg_1" (list :last-delta (- (float-time) 10))
              opencode-session--stream-message-states)
     (cl-letf (((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (session-id delay)
                  (setq scheduled (list session-id delay)))))
       (should (equal "busy"
                      (opencode-session--apply-polled-status
		       "ses_test" '(("ses_test" . ((type . "busy"))))))))
     (should (equal '("ses_test" 0.2) scheduled)))))

(ert-deftest opencode-event-polled-busy-status-starts-question-recovery ()
  "Fallback status polling also recovers missed pending questions."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-api-url "http://localhost:4098")
         recover-call)
     (cl-letf (((symbol-function 'opencode-session--schedule-reconcile)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode-session--schedule-question-recover)
                (lambda (session-id &optional delay attempts)
                  (setq recover-call (list session-id delay attempts)))))
       (should (equal "busy"
                      (opencode-session--apply-polled-status
		       "ses_test" '(("ses_test" . ((type . "busy"))))))))
     (should (equal '("ses_test" 0.25 nil) recover-call)))))

(ert-deftest opencode-event-status-poll-uses-session-directory ()
  "Status polling sends the session buffer directory in the API header."
  (opencode-event-tests--with-session-buffer
   (let ((session-dir (file-name-as-directory (make-temp-file "opencode-session" t)))
         (wrong-buffer (generate-new-buffer " *opencode-wrong-project*"))
         captured-dir)
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (setq opencode-session-directory session-dir
                   default-directory (file-name-as-directory temporary-file-directory)
                   opencode-session-status "busy"))
           (with-current-buffer wrong-buffer
             (setq default-directory "/tmp/wrong-project/")
             (let ((opencode-api-url "http://localhost:4097"))
	       (cl-letf (((symbol-function 'plz)
                          (lambda (_method _url &rest plist)
                            (setq captured-dir
                                  (cdr (assoc "x-opencode-directory"
                                              (plist-get plist :headers))))
                            (funcall (plist-get plist :then)
                                     '(("ses_test" . ((type . "busy"))))))))
                 (opencode-session--poll-status "ses_test"))))
           (should (equal session-dir captured-dir))
           (should (equal "busy"
                          (with-current-buffer buffer
                            opencode-session-status))))
       (delete-directory session-dir t)
       (when (buffer-live-p wrong-buffer)
         (kill-buffer wrong-buffer))))))

(ert-deftest opencode-event-status-poll-reschedules-at-configured-interval ()
  "Busy status polling backs off to the configured interval."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-session-status-poll-interval 7)
         scheduled)
     (setq opencode-session-status "busy")
     (cl-letf (((symbol-function 'plz)
                (lambda (_method _url &rest plist)
                  (funcall (plist-get plist :then)
                           '(("ses_test" . ((type . "busy")))))))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (session-id &optional delay)
                  (setq scheduled (list session-id delay)))))
       (opencode-session--poll-status "ses_test"))
     (should (equal '("ses_test" 7) scheduled)))))

(ert-deftest opencode-event-status-poll-skips-idle-session ()
  "Fallback status polling does not call the backend for idle sessions."
  (opencode-event-tests--with-session-buffer
   (let ((called nil))
     (setq opencode-session-status "idle"
           opencode-session--status-poll-in-flight nil
           opencode-session--status-poll-timer (timer-create))
     (unwind-protect
         (cl-letf (((symbol-function 'plz)
                    (lambda (&rest _args)
                      (setq called t))))
           (opencode-session--poll-status "ses_test")
           (should-not called)
           (should-not opencode-session--status-poll-timer)
           (should-not opencode-session--status-poll-in-flight))
       (when (timerp opencode-session--status-poll-timer)
         (cancel-timer opencode-session--status-poll-timer))))))

(ert-deftest opencode-event-status-poll-schedule-skips-idle-session ()
  "Fallback status polling is not scheduled for idle sessions."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "idle")
   (unwind-protect
       (progn
         (opencode-session--schedule-status-poll "ses_test" 1000)
         (should-not
          (seq-find
           (lambda (timer)
             (and (eq (timer--function timer)
                      #'opencode-session--poll-status)
                  (equal (timer--args timer) '("ses_test"))))
           timer-list)))
     (opencode-session--cancel-status-poll-timers "ses_test"))))

(ert-deftest opencode-event-history-reconcile-queues-while-in-flight ()
  "Persisted-history reconciliation queues callbacks while one request runs."
  (opencode-event-tests--with-session-buffer
   (let (calls)
     (should (opencode-session--history-reconcile-start
              (lambda () (push :first calls))))
     (should opencode-session--history-reconcile-in-flight)
     (should-not
      (opencode-session--history-reconcile-start
       (lambda () (push :second calls))))
     (opencode-session--history-reconcile-finish)
     (should (equal '(:second :first) calls))
     (should-not opencode-session--history-reconcile-in-flight)
     (should-not opencode-session--history-reconcile-callbacks))))

(ert-deftest opencode-event-reconcile-pending-skips-duplicate-in-flight-request ()
  "Full history reconciliation does not stack duplicate HTTP requests."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-api-url "http://localhost:4098")
         (opencode-session--history-reconcile-in-flight t)
         (plz-calls 0)
         (callback-ran nil))
     (cl-letf (((symbol-function 'plz)
                (lambda (&rest _args)
                  (cl-incf plz-calls))))
       (opencode-session--reconcile-pending
        "ses_test" (lambda () (setq callback-ran t)))
       (should (= 0 plz-calls))
       (should-not callback-ran)
       (should opencode-session--history-reconcile-in-flight)
       (should (= 1 (length opencode-session--history-reconcile-callbacks)))))))

(ert-deftest opencode-event-status-poll-schedule-replaces-existing-timer ()
  "Scheduling status polling keeps one timer per session."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "busy")
   (unwind-protect
       (progn
         (opencode-session--schedule-status-poll "ses_test" 1000)
         (opencode-session--schedule-status-poll "ses_test" 1000)
         (should
          (= 1
             (length
              (seq-filter
	       (lambda (timer)
                 (and (eq (timer--function timer)
                          #'opencode-session--poll-status)
                      (equal (timer--args timer) '("ses_test"))))
	       timer-list)))))
     (opencode-session--cancel-status-poll-timers "ses_test"))))

(ert-deftest opencode-event-reconcile-renders-completed-pending-message ()
  "Pending assistant entries render from stored completed messages."
  (opencode-event-tests--with-state
   (setq opencode-assistant-messages '(("msg_1"))
         opencode-rendered-message-ids (make-hash-table :test 'equal))
   (let ((messages-by-id (make-hash-table :test 'equal))
         rendered)
     (puthash "msg_1"
              '((info . ((id . "msg_1")
                         (role . "assistant")
                         (time . ((completed . 1))))))
              messages-by-id)
     (cl-letf (((symbol-function 'opencode-session--render-complete-assistant-message)
                (lambda (message)
                  (setq rendered (map-nested-elt message '(info id)))
                  (puthash rendered t opencode-rendered-message-ids))))
       (opencode-session--reconcile-pending-messages messages-by-id))
     (should (equal "msg_1" rendered))
     (should (gethash "msg_1" opencode-rendered-message-ids))
     (should-not opencode-assistant-messages))))

(ert-deftest opencode-event-reconcile-skips-incomplete-positioned-message ()
  "Reconciliation does not duplicate a live-rendering unfinished message."
  (opencode-event-tests--with-state
   (setq opencode-assistant-messages '(("msg_1" text . 42))
         opencode-rendered-message-ids (make-hash-table :test 'equal))
   (let ((messages-by-id (make-hash-table :test 'equal))
         rendered)
     (puthash "msg_1"
              '((info . ((id . "msg_1") (role . "assistant"))))
              messages-by-id)
     (cl-letf (((symbol-function 'opencode-session--render-complete-assistant-message)
                (lambda (_message) (setq rendered t))))
       (opencode-session--reconcile-pending-messages messages-by-id))
     (should-not rendered)
     (should opencode-assistant-messages))))

(ert-deftest opencode-event-reconcile-renders-missed-completed-assistant ()
  "Idle reconciliation renders completed assistant messages missed entirely."
  (opencode-event-tests--with-state
   (puthash "msg_rendered" t opencode-rendered-message-ids)
   (let (rendered)
     (cl-letf (((symbol-function 'opencode-session--render-complete-assistant-message)
                (lambda (message)
                  (let ((message-id (map-nested-elt message '(info id))))
                    (push message-id rendered)
                    (puthash message-id t opencode-rendered-message-ids)))))
       (opencode-session--reconcile-missed-completed-assistants
        (list (opencode-event-tests--stored-assistant
	       "msg_rendered" "prt_rendered" "Already shown" t)
              (opencode-event-tests--stored-assistant
	       "msg_missing" "prt_missing" "Missed reply" t))))
     (should (equal '("msg_missing") rendered))
     (should (gethash "msg_missing" opencode-rendered-message-ids)))))

(ert-deftest opencode-event-reconcile-renders-running-tool-message ()
  "Busy reconciliation renders running tool messages missed by live events."
  (opencode-event-tests--with-session-buffer
   (let (blocks status-poll)
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode--insert-tool-block)
                (lambda (tool input)
                  (push (list tool input) blocks)
                  (insert (opencode--format-tool-call tool input))))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (session-id &optional _delay)
                  (setq status-poll session-id))))
       (opencode-session--reconcile-running-assistant-messages
        '(((info . ((id . "msg_1")
                    (sessionID . "ses_test")
                    (role . "assistant")))
           (parts . [((id . "prt_text")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (type . "text")
                      (text . "Working"))
                     ((id . "prt_tool")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (callID . "call_1")
                      (type . "tool")
                      (tool . "bash")
                      (state . ((status . "running")
                                (input . ((command . "sleep 1"))))))])))))
     (should (string-match-p "Working" (opencode-event-tests--buffer-text)))
     (should (equal '(("bash" ((command . "sleep 1")))) blocks))
     (should (gethash "call_1" opencode--tool-calls-displayed))
     (should (assoc-string "msg_1" opencode-assistant-messages))
     (should (equal "busy" opencode-session-status))
     (should (equal "ses_test" status-poll)))))

(ert-deftest opencode-event-completed-history-dedupes-running-tool-block ()
  "Completed history rendering does not duplicate already rendered tools."
  (opencode-event-tests--with-session-buffer
   (let (blocks)
     (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
	       ((symbol-function 'opencode--insert-tool-block)
                (lambda (tool input)
                  (push (list tool input) blocks)
                  (insert (opencode--format-tool-call tool input))))
	       ((symbol-function 'opencode--maybe-insert-tool-output)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (&rest _args))))
       (opencode-session--reconcile-running-assistant-messages
        '(((info . ((id . "msg_1")
                    (sessionID . "ses_test")
                    (role . "assistant")))
           (parts . [((id . "prt_tool")
                      (sessionID . "ses_test")
                      (messageID . "msg_1")
                      (callID . "call_1")
                      (type . "tool")
                      (tool . "task")
                      (state . ((status . "running")
                                (input . ((description . "Prototype MOS templates"))))))]))))
       (opencode-session--render-complete-assistant-message
        '((info . ((id . "msg_1")
                   (sessionID . "ses_test")
                   (role . "assistant")
                   (time . ((completed . 1)))))
          (parts . [((id . "prt_tool")
                     (sessionID . "ses_test")
                     (messageID . "msg_1")
                     (callID . "call_1")
                     (type . "tool")
                     (tool . "task")
                     (state . ((status . "completed")
			       (input . ((description . "Prototype MOS templates"))))))]))))
     (should (= 1 (length blocks)))
     (should (gethash "call_1" opencode--tool-calls-displayed)))))

(ert-deftest opencode-event-completed-visible-message-does-not-duplicate ()
  "Completing a visibly streaming message does not replay stale history over it."
  (opencode-event-tests--with-session-buffer
   (insert "Hello")
   (setq opencode-assistant-messages
         `(("msg_1" text . ,(point-min))))
   (puthash "msg_1" "assistant" opencode-message-roles)
   (puthash "prt_1" "msg_1" opencode-part-message)
   (puthash "prt_1" "text" opencode-part-type)
   (puthash "prt_1" "Hello" opencode-part-text)
   (puthash "prt_1" 5 opencode-part-sent)
   (let ((old-plz (symbol-function 'plz)))
     (unwind-protect
         (progn
           (fset 'plz
                 (lambda (&rest args)
                   (let ((then (plist-get (cddr args) :then))
                         (value (list (opencode-event-tests--stored-assistant
				       "msg_1" "prt_1" "Hello" t))))
                     (funcall then value))))
           (cl-letf (((symbol-function 'opencode-session--schedule-reconcile)
                      (lambda (&rest _args))))
             (opencode--handle-message
              (opencode-event-tests--assistant "msg_1" t))))
       (fset 'plz old-plz)))
   (should (equal "Hello" (opencode-event-tests--buffer-text)))
   (should-not opencode-assistant-messages)))

(ert-deftest opencode-event-text-before-role-schedules-reconcile ()
  "A text part without role metadata schedules reconciliation recovery."
  (let ((opencode-session-buffers (make-hash-table :test 'equal))
        scheduled)
    (with-temp-buffer
      (opencode-event-tests--with-state
       (let ((proc (start-process "opencode-test-dummy" (current-buffer) nil)))
         (unwind-protect
             (progn
	       (set-process-query-on-exit-flag proc nil)
	       (puthash "ses_test" (current-buffer) opencode-session-buffers)
	       (cl-letf (((symbol-function 'opencode-session--schedule-reconcile)
                          (lambda (session-id &optional delay)
                            (setq scheduled (list session-id delay)))))
                 (opencode-session--update-part
                  '((sessionID . "ses_test")
                    (id . "prt_1")
                    (messageID . "msg_1")
                    (type . "text")
                    (text . "hello"))
                  nil "text")))
           (delete-process proc))))
      (should (equal '("ses_test" 1) scheduled)))))

(ert-deftest opencode-event-bootstrap-skips-delta-covered-by-history ()
  "Queued pre-bootstrap deltas already in replay history are not duplicated."
  (opencode-event-tests--with-session-buffer
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'opencode-session--schedule-reconcile)
              (lambda (&rest _args))))
     (setq opencode-session--bootstrapping t)
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" "lo"))
     (should (= 1 (length opencode-session--queued-events)))
     (opencode-session--render-complete-assistant-message
      (opencode-event-tests--stored-assistant "msg_1" "prt_1" "Hello" t))
     (setq opencode-session--bootstrapping nil)
     (let ((opencode-session--draining-queued-events t))
       (dolist (event (nreverse opencode-session--queued-events))
         (opencode--handle-message event)))
     (setq opencode-session--queued-events nil)
     (should (equal "Hello\n\n" (opencode-event-tests--buffer-text)))
     (should (gethash "msg_1" opencode-rendered-message-ids)))))

(ert-deftest opencode-event-bootstrap-applies-delta-not-yet-persisted ()
  "Queued pre-bootstrap deltas absent from replay history are rendered later."
  (opencode-event-tests--with-session-buffer
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'opencode-session--schedule-reconcile)
              (lambda (&rest _args))))
     (setq opencode-session--bootstrapping t)
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" "Hello"))
     (opencode-session--render-complete-assistant-message
      (opencode-event-tests--stored-assistant "msg_1" "prt_1" "" nil))
     (setq opencode-session--bootstrapping nil)
     (let ((opencode-session--draining-queued-events t))
       (dolist (event (nreverse opencode-session--queued-events))
         (opencode--handle-message event)))
     (setq opencode-session--queued-events nil)
     (opencode--handle-message
      (opencode-event-tests--assistant "msg_1" nil))
     (opencode--handle-message
      (opencode-event-tests--text-updated "msg_1" "prt_1" ""))
     (should (equal "Hello" (string-trim (opencode-event-tests--buffer-text)))))))

(ert-deftest opencode-event-live-delta-after-replay-continues-from-sent-offset ()
  "Live deltas after replay continue from replayed state without duplication."
  (opencode-event-tests--with-session-buffer
   (cl-letf (((symbol-function 'opencode--render-markdown) #'identity)
             ((symbol-function 'opencode-session--schedule-reconcile)
              (lambda (&rest _args))))
     (opencode-session--render-complete-assistant-message
      (opencode-event-tests--stored-assistant "msg_1" "prt_1" "Hello" nil))
     (opencode--handle-message
      (opencode-event-tests--assistant "msg_1" nil))
     (opencode--handle-message
      (opencode-event-tests--text-updated "msg_1" "prt_1" "Hello"))
     (opencode--handle-message
      (opencode-event-tests--text-delta "msg_1" "prt_1" " world"))
     (should (equal "Hello\n\n world"
                    (string-trim-right
                     (opencode-event-tests--buffer-text)))))))

(ert-deftest opencode-event-completed-message-after-missed-live-events-reconciles ()
  "A finished assistant message with missed text events is recovered from history."
  (opencode-event-tests--with-session-buffer
   (let ((old-render-markdown (symbol-function 'opencode--render-markdown))
         (old-plz (symbol-function 'plz))
         (old-schedule (symbol-function 'opencode-session--schedule-reconcile)))
     (unwind-protect
         (progn
           (fset 'opencode--render-markdown #'identity)
           (fset 'opencode-session--schedule-reconcile (lambda (&rest _args)))
           (fset 'plz
                 (lambda (&rest args)
                   (let ((then (plist-get (cddr args) :then))
                         (value (list (opencode-event-tests--stored-assistant
				       "msg_1" "prt_1" "Recovered" t))))
                     (funcall then value))))
           (opencode--handle-message (opencode-event-tests--assistant "msg_1" t))
           (should (equal "Recovered" (string-trim (opencode-event-tests--buffer-text))))
           (should (gethash "msg_1" opencode-rendered-message-ids)))
       (fset 'opencode--render-markdown old-render-markdown)
       (fset 'plz old-plz)
       (fset 'opencode-session--schedule-reconcile old-schedule)))))

(ert-deftest opencode-event-prefilter-keeps-user-message-updated ()
  "Raw SSE prefilter must not discard role metadata for user messages."
  (should-not
   (opencode--ignored-message-data-p
    "{\"type\":\"message.updated\",\"properties\":{\"info\":{\"role\":\"user\"}}}")))

(ert-deftest opencode-event-prefilter-ignores-heartbeat-and-sync ()
  "Raw SSE prefilter drops high-volume events before JSON decoding."
  (should (opencode--ignored-message-data-p
           "{\"type\":\"server.heartbeat\",\"properties\":{}}"))
  (should (opencode--ignored-message-data-p
           "{\"type\":\"sync\",\"properties\":{}}")))

(ert-deftest opencode-event-redacts-hidden-non-bash-tool-output ()
  "Hidden non-bash tool output is redacted before JSON decoding."
  (let ((opencode-show-tool-output nil)
        (opencode-redact-hidden-tool-output t)
        (opencode-redacted-tool-output-placeholder "[redacted]")
        (raw "{\"type\":\"message.part.updated\",\"properties\":{\"part\":{\"type\":\"tool\",\"tool\":\"write\",\"state\":{\"status\":\"completed\",\"output\":\"secret\"}}}}"))
    (let ((redacted (opencode--redact-tool-output-data raw)))
      (should (string-match-p "\\[redacted\\]" redacted))
      (should-not (string-match-p "secret" redacted)))))

(ert-deftest opencode-event-preserves-bash-tool-output-redaction ()
  "Bash output is preserved because the UI may display shell command output."
  (let ((opencode-show-tool-output nil)
        (opencode-redact-hidden-tool-output t)
        (raw "{\"type\":\"message.part.updated\",\"properties\":{\"part\":{\"type\":\"tool\",\"tool\":\"bash\",\"state\":{\"status\":\"completed\",\"output\":\"shell output\"}}}}"))
    (should (equal raw (opencode--redact-tool-output-data raw)))))

(ert-deftest opencode-event-non-text-delta-is-ignored ()
  "Non-text deltas do not reach the text renderer."
  (let ((called nil))
    (cl-letf (((symbol-function 'opencode-session--update-part)
               (lambda (&rest _args) (setq called t))))
      (opencode--handle-message
       '((type . "message.part.delta")
         (properties . ((sessionID . "ses_test")
                        (partID . "prt_1")
                        (field . "metadata")
                        (delta . "ignored"))))))
    (should-not called)))

(ert-deftest opencode-event-sse-split-frame-dispatches-on-delimiter ()
  "The fast SSE parser dispatches a frame split across chunks."
  (opencode-event-tests--with-sse-state
   (let (received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk
        "data: {\"type\":\"message.updated\",")
       (should-not received)
       (opencode--sse-process-chunk
        "\"properties\":{}}\n\n"))
     (should (equal '("{\"type\":\"message.updated\",\"properties\":{}}")
                    received))
     (should (string-empty-p opencode--sse-pending)))))

(ert-deftest opencode-event-sse-multiline-data-joins-with-newline ()
  "The fast SSE parser joins multiple data lines in one frame."
  (opencode-event-tests--with-sse-state
   (let (received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk
        "event: message\ndata: first\ndata: second\n\n"))
     (should (equal '("first\nsecond") received)))))

(ert-deftest opencode-event-sse-ignored-frame-discard-survives-split ()
  "Ignored SSE frames can be discarded even when their delimiter is split."
  (opencode-event-tests--with-sse-state
   (let (received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk
        "data: {\"type\":\"server.heartbeat\",")
       (should opencode--sse-discarding-ignored-frame)
       (opencode--sse-process-chunk
        "\"properties\":{}}\n")
       (should opencode--sse-discarding-ignored-frame)
       (opencode--sse-process-chunk
        "\ndata: {\"type\":\"message.updated\",\"properties\":{}}\n\n"))
     (should (equal '("{\"type\":\"message.updated\",\"properties\":{}}")
                    received))
     (should-not opencode--sse-discarding-ignored-frame)
     (should (string-empty-p opencode--sse-pending)))))

(ert-deftest opencode-event-sse-dispatches-multiple-frames-in-one-chunk ()
  "The fast SSE parser dispatches all complete frames in a chunk."
  (opencode-event-tests--with-sse-state
   (let (received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk
        (concat "data: {\"type\":\"message.updated\",\"properties\":{\"n\":1}}\n\n"
                "data: {\"type\":\"message.updated\",\"properties\":{\"n\":2}}\n\n")))
     (should (equal '("{\"type\":\"message.updated\",\"properties\":{\"n\":2}}"
                      "{\"type\":\"message.updated\",\"properties\":{\"n\":1}}")
                    received)))))

(ert-deftest opencode-event-sse-supports-crlf-and-cr-delimiters ()
  "The fast SSE parser accepts CRLF and CR frame delimiters."
  (opencode-event-tests--with-sse-state
   (let (received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk "data: one\r\n\r\ndata: two\r\r"))
     (should (equal '("two" "one") received)))))

(ert-deftest opencode-event-sse-ignores-comments-and-non-message-events ()
  "SSE comments are ignored and non-message event frames are skipped."
  (opencode-event-tests--with-sse-state
   (let (received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk
        (concat ": keep-alive\n\n"
                "event: ping\ndata: ignored\n\n"
                "event: message\ndata: handled\n\n")))
     (should (equal '("handled") received)))))

(ert-deftest opencode-event-global-wrapper-binds-project-directory ()
  "Global events bind `default-directory' to the event project directory."
  (let* ((dir (make-temp-file "opencode-event-test" t))
         (raw (json-encode
               `((directory . ,dir)
                 (payload . ((type . "message.updated")
                             (properties . ((sessionID . "ses_test"))))))))
         captured)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'opencode--handle-message)
                     (lambda (data)
                       (setq captured (list default-directory data)))))
            (opencode--handle-global-event-data raw))
          (should (equal (opencode--normalize-directory dir)
                         (car captured)))
          (should (equal "message.updated"
                         (alist-get 'type (cadr captured)))))
      (delete-directory dir t))))

(ert-deftest opencode-event-sse-parse-error-is-retained-recently ()
  "Malformed SSE data is retained as a recent diagnostic event."
  (let ((opencode-record-recent-events-enabled t)
        (opencode-record-recent-sse-diagnostics-enabled t)
        (opencode-record--recent-global-queue
         (opencode-record--recent-queue-create))
        (opencode-record--recent-session-queues (make-hash-table :test 'equal))
        (opencode-record--recent-seq 0)
        (opencode-record--file nil))
    (cl-letf (((symbol-function 'opencode--log-event) #'ignore)
              ((symbol-function 'opencode-record--autosave-recent) #'ignore))
      (opencode--handle-global-event-data "{not-json"))
    (let ((entries (opencode-record--recent-events)))
      (should (= 1 (length entries)))
      (should (equal "sse.parse-error"
                     (opencode-record--recent-entry-type (car entries))))
      (should (equal "{not-json"
                     (opencode-record--recent-entry-raw (car entries)))))))

(ert-deftest opencode-event-sse-prefilter-ignored-frame-is-retained-recently ()
  "Fast SSE prefilter records ignored frames before discarding them."
  (opencode-event-tests--with-sse-state
   (let ((opencode-record-recent-events-enabled t)
         (opencode-record-recent-sse-diagnostics-enabled t)
         (opencode-record--recent-global-queue
          (opencode-record--recent-queue-create))
         (opencode-record--recent-session-queues (make-hash-table :test 'equal))
         (opencode-record--recent-seq 0)
         (opencode-record--file nil)
         received)
     (cl-letf (((symbol-function 'opencode--handle-global-event-data)
                (lambda (data) (push data received))))
       (opencode--sse-process-chunk
        "data: {\"type\":\"server.heartbeat\",")
       (opencode--sse-process-chunk
        "\"properties\":{}}\n\ndata: {\"type\":\"message.updated\",\"properties\":{}}\n\n"))
     (should (equal '("{\"type\":\"message.updated\",\"properties\":{}}")
                    received))
     (let ((entries (opencode-record--recent-events)))
       (should (= 1 (length entries)))
       (should (equal "sse.ignored"
                      (opencode-record--recent-entry-type (car entries))))
       (should (string-match-p "server\\.heartbeat"
                               (or (opencode-record--recent-entry-raw
                                    (car entries))
                                   "")))))))

(ert-deftest opencode-event-queues-while-session-bootstraps ()
  "Live events for a bootstrapping session are queued instead of applied."
  (let ((opencode-session-buffers (make-hash-table :test 'equal))
        (event '((type . "message.updated")
                 (properties . ((sessionID . "ses_test")
                                (info . ((id . "msg_1")
                                         (role . "assistant"))))))))
    (with-temp-buffer
      (setq opencode-session-id "ses_test"
            opencode-session--bootstrapping t
            opencode-session--queued-events nil)
      (puthash "ses_test" (current-buffer) opencode-session-buffers)
      (should (opencode--queue-message-while-bootstrapping event))
      (should (equal (list event) opencode-session--queued-events)))))

(ert-deftest opencode-event-queues-blockers-and-shell-while-bootstrapping ()
  "Permission, question, and shell events queue while the session replays."
  (let ((opencode-session-buffers (make-hash-table :test 'equal))
        (events '(((type . "permission.asked")
                   (properties . ((sessionID . "ses_test")
                                  (id . "perm_1"))))
                  ((type . "question.asked")
                   (properties . ((sessionID . "ses_test")
                                  (id . "que_1"))))
                  ((type . "session.next.shell.started")
                   (properties . ((sessionID . "ses_test")
                                  (callID . "call_1")
                                  (command . "pwd")))))))
    (with-temp-buffer
      (setq opencode-session-id "ses_test"
            opencode-session--bootstrapping t
            opencode-session--queued-events nil)
      (puthash "ses_test" (current-buffer) opencode-session-buffers)
      (dolist (event events)
        (should (opencode--queue-message-while-bootstrapping event)))
      (should (equal events (nreverse opencode-session--queued-events))))))

(ert-deftest opencode-event-multi-session-status-is-isolated ()
  "A status event for one session does not mutate another session buffer."
  (opencode-event-tests--with-two-session-buffers
   (opencode--handle-message (opencode-event-tests--status "ses_a" "busy"))
   (with-current-buffer buffer-a
     (should (equal "busy" opencode-session-status)))
   (with-current-buffer buffer-b
     (should (equal "idle" opencode-session-status)))))

(ert-deftest opencode-event-multi-session-question-is-isolated ()
  "A question event queues only in its target session buffer."
  (opencode-event-tests--with-two-session-buffers
   (cl-letf (((symbol-function 'opencode--buffer-active-p) (lambda (&rest _args) nil))
             ((symbol-function 'opencode--toast-show) (lambda (&rest _args)))
             ((symbol-function 'opencode-api-session) (lambda (&rest _args))))
     (opencode--handle-message
      '((type . "question.asked")
        (properties . ((sessionID . "ses_a")
		       (id . "que_1")
		       (questions . [((question . "Proceed?")
                                      (options . [((label . "Yes"))]))]))))))
   (with-current-buffer buffer-a
     (should (equal "que_1" (car opencode-session-pending-questions))))
   (with-current-buffer buffer-b
     (should-not opencode-session-pending-questions))))

(ert-deftest opencode-event-bootstrap-drains-blockers-and-shell-in-order ()
  "Queued bootstrap events are reduced in original arrival order after replay."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session--bootstrapping t)
   (let ((events (list
                  '((type . "permission.asked")
                    (properties . ((sessionID . "ses_test")
                                   (id . "perm_1")
                                   (permission . "bash")
                                   (metadata . ((command . "pwd")))
                                   (patterns . [])
                                   (always . [])
                                   (tool . ((messageID . "msg_1")
                                            (callID . "call_1"))))))
                  '((type . "question.asked")
                    (properties . ((sessionID . "ses_test")
                                   (id . "que_1")
                                   (questions . [((question . "Proceed?")
                                                  (options . [((label . "Yes"))]))]))))
                  (opencode-event-tests--shell-started "call_shell" "echo hi")))
         applied)
     (dolist (event events)
       (opencode--handle-message event))
     (should (= 3 (length opencode-session--queued-events)))
     (setq opencode-session--bootstrapping nil)
     (cl-letf (((symbol-function 'opencode--permission-request)
                (lambda (&rest _args)
                  (setq applied (append applied '(permission)))))
	       ((symbol-function 'opencode--question-request)
                (lambda (&rest _args)
                  (setq applied (append applied '(question)))))
	       ((symbol-function 'opencode-session--handle-shell-started)
                (lambda (&rest _args)
                  (setq applied (append applied '(shell))))))
       (let ((opencode-session--draining-queued-events t))
         (dolist (event (nreverse opencode-session--queued-events))
           (opencode--handle-message event))))
     (setq opencode-session--queued-events nil)
     (should (equal '(permission question shell) applied)))))

(ert-deftest opencode-event-mode-line-tolerates-partial-session-state ()
  "Mode line rendering does not fail while a session buffer initializes."
  (opencode-event-tests--with-state
   (setq opencode-session-agent nil
         opencode-session-status nil
         opencode-session-tokens 0)
   (cl-letf (((symbol-function 'opencode--current-model)
              (lambda () nil)))
     (should (stringp (opencode--session-status-indicator))))))

(ert-deftest opencode-event-send-input-uses-session-id-variable ()
  "Sending input must not call `opencode-session-id' as a function."
  (opencode-event-tests--with-state
   (setq opencode-session-id "ses_test"
         opencode-api-url "http://localhost:4098"
         opencode-session-agent
         '((name . "build")
           (model . ((providerID . "openai") (modelID . "gpt-5.5")))))
   (let ((status-session nil)
         (poll-session nil)
         (sent-path nil))
     (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
                (lambda () nil))
	       ((symbol-function 'opencode--highlight-input)
                (lambda ()))
	       ((symbol-function 'opencode--output)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--effective-session-agent)
                (lambda () opencode-session-agent))
	       ((symbol-function 'opencode-session--set-status)
                (lambda (session-id _status)
                  (setq status-session session-id)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (session-id &optional _delay)
                  (setq poll-session session-id)))
	       ((symbol-function 'opencode--auth-header)
                (lambda () (cons "authorization" "Basic test")))
	       ((symbol-function 'plz)
                (lambda (_method url &rest _plist)
                  (setq sent-path
                        (string-remove-prefix opencode-api-url url)))))
       (opencode--send-input nil "hello"))
     (should (equal "ses_test" status-session))
     (should (equal "ses_test" poll-session))
     (should (equal "/session/ses_test/prompt_async" sent-path)))))

(ert-deftest opencode-event-send-input-blocks-while-busy ()
  "A second prompt cannot be sent while the session is already busy."
  (opencode-event-tests--with-state
   (setq opencode-session-status "busy")
   (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
              (lambda () nil)))
     (should-error (opencode--send-input nil "hello")
                   :type 'user-error))))

(ert-deftest opencode-event-send-input-includes-and-clears-extra-parts ()
  "Prompt payload includes queued extra parts once, then clears them."
  (opencode-event-tests--with-state
   (setq opencode-session-id "ses_test"
         opencode-session-status "idle"
         opencode-api-url "http://localhost:4098"
         opencode--extra-parts '(((type . "file")
                                  (url . "file:///tmp/a.ts")
                                  (filename . "a.ts")))
         opencode-session-agent
         '((name . "build")
           (model . ((providerID . "openai") (modelID . "gpt-5.5")))))
   (let (payload)
     (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
                (lambda () nil))
	       ((symbol-function 'opencode--highlight-input)
                (lambda ()))
	       ((symbol-function 'opencode--output)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--effective-session-agent)
                (lambda () opencode-session-agent))
	       ((symbol-function 'opencode-session--set-status)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--auth-header)
                (lambda () (cons "authorization" "Basic test")))
	       ((symbol-function 'plz)
                (lambda (_method _url &rest plist)
                  (setq payload
                        (json-parse-string (plist-get plist :body)
                                           :array-type 'list
                                           :object-type 'alist)))))
       (opencode--send-input nil "hello"))
     (let ((parts (alist-get 'parts payload)))
       (should (= 2 (length parts)))
       (should (equal "file" (alist-get 'type (car parts))))
       (should (equal "hello" (alist-get 'text (cadr parts))))
       (should-not opencode--extra-parts)))))

(ert-deftest opencode-event-send-input-normal-deletes-temp-on-callback ()
  "Normal prompt temp files are deleted after the send callback runs."
  (opencode-event-tests--with-state
   (let ((temp-file (make-temp-file "opencode-event-test")))
     (unwind-protect
         (progn
           (setq opencode-session-id "ses_test"
                 opencode-session-status "idle"
                 opencode-api-url "http://localhost:4098"
                 opencode--temp-files (list temp-file)
                 opencode-session-agent
                 '((name . "build")
                   (model . ((providerID . "openai") (modelID . "gpt-5.5")))))
           (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
                      (lambda () nil))
                     ((symbol-function 'opencode--highlight-input)
                      (lambda ()))
                     ((symbol-function 'opencode--output)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode--effective-session-agent)
                      (lambda () opencode-session-agent))
                     ((symbol-function 'opencode-session--set-status)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode-session--schedule-status-poll)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode--auth-header)
                      (lambda () (cons "authorization" "Basic test")))
                     ((symbol-function 'plz)
                      (lambda (_method _url &rest plist)
                        (funcall (plist-get plist :then) t))))
             (opencode--send-input nil "hello"))
           (should-not opencode--temp-files)
           (should-not (file-exists-p temp-file)))
       (when (file-exists-p temp-file)
         (delete-file temp-file))))))

(ert-deftest opencode-event-send-input-slash-command-payload-and-cleanup ()
  "Slash commands send command payloads and clean unused prompt context."
  (opencode-event-tests--with-state
   (let ((temp-file (make-temp-file "opencode-event-test"))
         payload path)
     (unwind-protect
         (progn
           (setq opencode-session-id "ses_test"
                 opencode-session-status "idle"
                 opencode-api-url "http://localhost:4098"
                 opencode--temp-files (list temp-file)
                 opencode--extra-parts '(((type . "file") (url . "file:///tmp/a")))
                 opencode-session-agent
                 '((name . "build")
                   (model . ((providerID . "openai") (modelID . "gpt-5.5")))))
           (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
                      (lambda () nil))
                     ((symbol-function 'opencode--highlight-input)
                      (lambda ()))
                     ((symbol-function 'opencode--output)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode--effective-session-agent)
                      (lambda () opencode-session-agent))
                     ((symbol-function 'opencode-session--set-status)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode-session--schedule-status-poll)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode--auth-header)
                      (lambda () (cons "authorization" "Basic test")))
                     ((symbol-function 'plz)
                      (lambda (_method url &rest plist)
                        (setq path (string-remove-prefix opencode-api-url url)
                              payload (json-parse-string (plist-get plist :body)
                                                         :array-type 'list
                                                         :object-type 'alist)))))
             (opencode--send-input nil "/build now"))
           (should (equal "/session/ses_test/command" path))
           (should (equal "build" (alist-get 'command payload)))
           (should (equal "now" (alist-get 'arguments payload)))
           (should (equal "openai/gpt-5.5" (alist-get 'model payload)))
           (should-not opencode--extra-parts)
           (should-not opencode--temp-files)
           (should-not (file-exists-p temp-file)))
       (when (file-exists-p temp-file)
         (delete-file temp-file))))))

(ert-deftest opencode-event-send-input-compact-uses-summarize-endpoint ()
  "The built-in /compact command calls session summarize, not project commands."
  (opencode-event-tests--with-state
   (let (payload path)
     (setq opencode-session-id "ses_test"
           opencode-session-status "idle"
           opencode-api-url "http://localhost:4098"
           opencode-session-agent
           '((name . "build")
             (model . ((providerID . "openai") (modelID . "gpt-5.5")))))
     (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
                (lambda () nil))
	       ((symbol-function 'opencode--highlight-input)
                (lambda ()))
	       ((symbol-function 'opencode--output)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--effective-session-agent)
                (lambda () opencode-session-agent))
	       ((symbol-function 'opencode-session--set-status)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode-session--schedule-status-poll)
                (lambda (&rest _args)))
	       ((symbol-function 'opencode--auth-header)
                (lambda () (cons "authorization" "Basic test")))
	       ((symbol-function 'plz)
                (lambda (_method url &rest plist)
                  (setq path (string-remove-prefix opencode-api-url url)
                        payload (json-parse-string (plist-get plist :body)
                                                   :array-type 'list
                                                   :object-type 'alist)))))
       (opencode--send-input nil "/compact"))
     (should (equal "/session/ses_test/summarize" path))
     (should (equal "openai" (alist-get 'providerID payload)))
     (should (equal "gpt-5.5" (alist-get 'modelID payload)))
     (should-not (alist-get 'command payload)))))

(ert-deftest opencode-event-send-input-shell-payload-and-cleanup ()
  "Shell prompts send shell payloads and clean unused prompt context."
  (opencode-event-tests--with-state
   (let ((temp-file (make-temp-file "opencode-event-test"))
         payload path)
     (unwind-protect
         (progn
           (setq opencode-session-id "ses_test"
                 opencode-session-status "idle"
                 opencode-api-url "http://localhost:4098"
                 opencode--temp-files (list temp-file)
                 opencode--extra-parts '(((type . "file") (url . "file:///tmp/a")))
                 opencode-session-agent
                 '((name . "build")
                   (model . ((providerID . "openai") (modelID . "gpt-5.5")))))
           (cl-letf (((symbol-function 'opencode-session--sync-pending-question)
                      (lambda () nil))
                     ((symbol-function 'opencode--highlight-input)
                      (lambda ()))
                     ((symbol-function 'opencode--output)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode--effective-session-agent)
                      (lambda () opencode-session-agent))
                     ((symbol-function 'opencode-session--set-status)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode-session--schedule-status-poll)
                      (lambda (&rest _args)))
                     ((symbol-function 'opencode--auth-header)
                      (lambda () (cons "authorization" "Basic test")))
                     ((symbol-function 'plz)
                      (lambda (_method url &rest plist)
                        (setq path (string-remove-prefix opencode-api-url url)
                              payload (json-parse-string (plist-get plist :body)
                                                         :array-type 'list
                                                         :object-type 'alist)))))
             (opencode--send-input nil "!echo hi"))
           (should (equal "/session/ses_test/shell" path))
           (should (equal "echo hi" (alist-get 'command payload)))
           (should-not opencode--extra-parts)
           (should-not opencode--temp-files)
           (should-not (file-exists-p temp-file)))
       (when (file-exists-p temp-file)
         (delete-file temp-file))))))

(ert-deftest opencode-event-slash-command-fetches-on-cache-miss ()
  "Slash command completion fetches project commands on cache miss."
  (opencode-event-tests--with-session-buffer
   (let ((directory (file-name-as-directory temporary-file-directory))
         candidates
         requested-directory)
     (setq opencode-api-url "http://localhost:4098"
           opencode--slash-commands-by-directory nil)
     (let ((default-directory directory)
           (comint-last-prompt (cons (point) (point))))
       (cl-letf (((symbol-function 'opencode--auth-header)
                  (lambda () (cons "authorization" "Basic test")))
                 ((symbol-function 'plz)
                  (lambda (_method url &rest plist)
                    (should (equal "/command"
                                   (string-remove-prefix opencode-api-url url)))
                    (setq requested-directory
                          (cdr (assoc "x-opencode-directory"
                                      (plist-get plist :headers))))
                    '(((name . "review")
		       (description . "desc")))))
                 ((symbol-function 'opencode--annotated-completion)
                  (lambda (_prompt completion-candidates)
                    (setq candidates completion-candidates)
                    "review")))
         (opencode-insert-slash-command)))
     (let ((normalized-directory (opencode--normalize-directory directory)))
       (should (equal "/review" (opencode-event-tests--buffer-text)))
       (should (equal '(("compact" "compact" "summarize the session to reduce context size")
                        ("summarize" "summarize" "alias for compact")
                        ("review" "review" "desc"))
                      candidates))
       (should (equal normalized-directory requested-directory))
       (should (= 1 (length (alist-get normalized-directory
				       opencode--slash-commands-by-directory
				       nil nil #'string=))))))))

(ert-deftest opencode-event-disconnect-flushes-active-session ()
  "Event stream disconnect marks active output interrupted instead of hanging."
  (opencode-event-tests--with-session-buffer
   (insert "partial")
   (setq opencode-session-status "busy"
         opencode-assistant-messages
         `(("msg_1" text . ,(point-min))))
   (cl-letf (((symbol-function 'opencode--render-region)
              (lambda (&rest _args))))
     (opencode-disconnect 'fault))
   (should (equal "idle" opencode-session-status))
   (should (string-match-p "\\[interrupted\\]"
                           (opencode-event-tests--buffer-text)))
   (should (string-match-p "OpenCode event stream disconnected"
                           (opencode-event-tests--buffer-text)))))

(ert-deftest opencode-event-instance-disposed-flushes-directory-session ()
  "Instance disposal for a project flushes active buffers in that directory."
  (opencode-event-tests--with-session-buffer
   (let ((default-directory (file-name-as-directory temporary-file-directory)))
     (insert "partial")
     (setq opencode-session-status "busy"
           opencode-assistant-messages
           `(("msg_1" text . ,(point-min))))
     (cl-letf (((symbol-function 'opencode--render-region)
                (lambda (&rest _args))))
       (opencode--handle-message
        `((type . "server.instance.disposed")
          (properties . ((directory . ,temporary-file-directory))))))
     (should (equal "idle" opencode-session-status))
     (should (string-match-p "OpenCode instance disposed"
                             (opencode-event-tests--buffer-text))))))

(ert-deftest opencode-event-disconnect-without-event-does-not-reject-turn ()
  "A nil disconnect event does not mark active output as failed."
  (opencode-event-tests--with-session-buffer
   (insert "partial")
   (setq opencode-session-status "busy"
         opencode-assistant-messages
         `(("msg_1" text . ,(point-min))))
   (opencode-disconnect nil)
   (should (equal "busy" opencode-session-status))
   (should opencode-assistant-messages)
   (should-not (string-match-p "\\[interrupted\\]"
			       (opencode-event-tests--buffer-text)))))

(ert-deftest opencode-event-autoconnect-does-not-start-server ()
  "Autoconnect never starts an OpenCode server, even if legacy option is set."
  (let ((opencode-api-url nil)
        (opencode--event-subscription nil)
        (opencode--process nil)
        (opencode-auto-start-server t)
        started)
    (cl-letf (((symbol-function 'opencode--connected-p)
               (lambda () nil))
              ((symbol-function 'opencode--server-running-p)
               (lambda () nil))
              ((symbol-function 'opencode--start-server)
               (lambda (&rest _args) (setq started t))))
      (should-error (opencode-autoconnect #'ignore) :type 'user-error)
      (should-not started))))

(ert-deftest opencode-event-instance-disposed-ignores-other-directory ()
  "Instance disposal is scoped to the disposed project directory."
  (opencode-event-tests--with-session-buffer
   (setq default-directory (file-name-as-directory temporary-file-directory))
   (insert "partial")
   (setq opencode-session-status "busy"
         opencode-assistant-messages
         `(("msg_1" text . ,(point-min))))
   (opencode--handle-message
    `((type . "server.instance.disposed")
      (properties . ((directory . ,(expand-file-name "other/"
                                                     temporary-file-directory))))))
   (should (equal "busy" opencode-session-status))
   (should opencode-assistant-messages)
   (should-not (string-match-p "OpenCode instance disposed"
			       (opencode-event-tests--buffer-text)))))

(ert-deftest opencode-event-idle-status-cancels-status-poll-timer ()
  "Setting a session idle cancels its outstanding status-poll timer."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session--status-poll-timer
         (run-at-time 1000 nil #'ignore))
   (unwind-protect
       (progn
         (opencode-session--set-status "ses_test" "idle")
         (should-not opencode-session--status-poll-timer))
     (when (timerp opencode-session--status-poll-timer)
       (cancel-timer opencode-session--status-poll-timer)))))

(ert-deftest opencode-event-repeated-status-skips-mode-line-update ()
  "Repeated identical statuses do not force mode-line redisplay."
  (opencode-event-tests--with-session-buffer
   (setq opencode-session-status "busy")
   (let ((calls 0))
     (cl-letf (((symbol-function 'force-mode-line-update)
                (lambda (&optional _all-frames)
                  (setq calls (1+ calls)))))
       (opencode-session--set-status "ses_test" "busy")
       (should (= calls 0))
       (opencode-session--set-status "ses_test" "idle")
       (should (= calls 1))))))

(ert-deftest opencode-event-record-writes-jsonl-and-snapshots ()
  "Recording writes JSONL entries without using slash commands."
  (let ((opencode-record-directory (make-temp-file "opencode-record" t))
        (opencode-record--file nil)
        (opencode-record--seq 0)
        file)
    (unwind-protect
        (opencode-event-tests--with-session-buffer
         (setq file (opencode-record-start "unit"))
         (opencode-record--send-input "hello")
         (setq file (opencode-record-stop))
         (let* ((events (opencode-record--read-events file))
                (types (mapcar #'opencode-record--event-type events)))
           (should (member "record.start" types))
           (should (member "send.input" types))
           (should (member "ui.snapshot" types))
           (should (member "record.stop" types))))
      (setq opencode-record--file nil)
      (when file
	(delete-file file))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-recent-keeps-session-and-global-bounds ()
  "Recent event retention keeps bounded global and per-session queues."
  (let ((opencode-record-recent-events-enabled t)
        (opencode-record-recent-session-limit 2)
        (opencode-record-recent-global-limit 3)
        (opencode-record-recent-session-byte-limit 100000)
        (opencode-record-recent-global-byte-limit 100000)
        (opencode-record-recent-include-history nil)
        (opencode-record--recent-global-queue
         (opencode-record--recent-queue-create))
        (opencode-record--recent-session-queues (make-hash-table :test 'equal))
        (opencode-record--recent-seq 0)
        (opencode-record--file nil))
    (cl-labels ((event (session-id delta)
                  `((type . "message.part.delta")
                    (properties . ((sessionID . ,session-id)
                                   (messageID . "msg_1")
                                   (partID . "prt_1")
                                   (field . "text")
                                   (delta . ,delta))))))
      (opencode-record--event (event "ses_a" "a1"))
      (opencode-record--event (event "ses_b" "b1"))
      (opencode-record--event (event "ses_a" "a2"))
      (opencode-record--event (event "ses_a" "a3"))
      (should (equal (mapcar #'opencode-record--recent-entry-session-id
                             (opencode-record--recent-events))
                     '("ses_b" "ses_a" "ses_a")))
      (should (equal (mapcar (lambda (entry)
                               (map-nested-elt
                                (opencode-record--recent-entry-data entry)
                                '(properties delta)))
                             (opencode-record--recent-events "ses_a"))
                     '("a2" "a3"))))))

(ert-deftest opencode-event-record-recent-honors-byte-limit ()
  "Recent event byte limits keep the newest oversized event." 
  (let ((opencode-record-recent-events-enabled t)
        (opencode-record-recent-session-limit 10)
        (opencode-record-recent-global-limit 10)
        (opencode-record-recent-session-byte-limit 80)
        (opencode-record-recent-global-byte-limit 100000)
        (opencode-record--recent-global-queue
         (opencode-record--recent-queue-create))
        (opencode-record--recent-session-queues (make-hash-table :test 'equal))
        (opencode-record--recent-seq 0)
        (opencode-record--file nil))
    (dotimes (index 3)
      (opencode-record--event
       `((type . "message.part.delta")
         (properties . ((sessionID . "ses_big")
                        (messageID . "msg_1")
                        (partID . "prt_1")
                        (field . "text")
                        (delta . ,(concat (number-to-string index)
                                          (make-string 200 ?x))))))))
    (let ((events (opencode-record--recent-events "ses_big")))
      (should (= (length events) 1))
      (should (string-prefix-p
               "2"
               (map-nested-elt (opencode-record--recent-entry-data (car events))
                               '(properties delta)))))))

(ert-deftest opencode-event-record-save-recent-events-replays-jsonl ()
  "Saving recent events writes replay-compatible JSONL."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (opencode-record-recent-events-enabled t)
         (opencode-record-recent-session-limit 10)
         (opencode-record-recent-global-limit 10)
         (opencode-record-recent-session-byte-limit 100000)
         (opencode-record-recent-global-byte-limit 100000)
         (opencode-record--recent-global-queue
          (opencode-record--recent-queue-create))
         (opencode-record--recent-session-queues (make-hash-table :test 'equal))
         (opencode-record--recent-seq 0)
         (opencode-record--file nil)
         file
         buffer)
    (unwind-protect
        (progn
          (let ((opencode-record--current-raw-event
                 "{\"directory\":\"/tmp\",\"payload\":{\"type\":\"message.part.delta\"}}")
                (opencode-record--current-directory temporary-file-directory))
            (opencode-record--event
             (opencode-event-tests--text-delta "msg_1" "prt_1" "RECENT"))
            (opencode-record--event
             (opencode-event-tests--text-delta "msg_1" "prt_1" "_OK"))
            (opencode-record--event
             (opencode-event-tests--status "ses_test" "idle")))
          (setq file (opencode-record-save-recent-events "ses_test" "unit"))
          (let* ((events (opencode-record--read-events file))
                 (types (mapcar #'opencode-record--event-type events))
                 (decoded (seq-filter (lambda (entry)
                                        (equal (opencode-record--event-type entry)
                                               "event.decoded"))
                                      events)))
            (should (member "record.start" types))
            (should (= (length decoded) 3))
            (should (alist-get 'raw (car decoded))))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (should (string-match-p "RECENT_OK"
                                    (opencode-event-tests--buffer-text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and file (file-exists-p file))
        (delete-file file))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-save-recent-events-includes-history-snapshot ()
  "Saving recent session events includes a bounded persisted-history tail."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (opencode-record-recent-events-enabled t)
         (opencode-record-recent-session-limit 10)
         (opencode-record-recent-global-limit 10)
         (opencode-record-recent-session-byte-limit 100000)
         (opencode-record-recent-global-byte-limit 100000)
         (opencode-record-recent-include-history t)
         (opencode-record-recent-history-limit 1)
         (opencode-record--recent-global-queue
          (opencode-record--recent-queue-create))
         (opencode-record--recent-session-queues (make-hash-table :test 'equal))
         (opencode-record--recent-seq 0)
         (opencode-record--file nil)
         file)
    (unwind-protect
        (progn
          (let ((opencode-record--current-directory temporary-file-directory))
            (opencode-record--event
             (opencode-event-tests--text-delta "msg_1" "prt_1" "PARTIAL")))
          (cl-letf (((symbol-function 'opencode-record--session-history-sync)
                     (lambda (_session-id _directory)
                       (list
                        (opencode-event-tests--stored-assistant
                         "msg_old" "prt_old" "old" t)
                        (opencode-event-tests--stored-assistant
                         "msg_1" "prt_1" "PARTIAL tail" t)))))
            (setq file (opencode-record-save-recent-events "ses_test" "unit")))
          (let* ((events (opencode-record--read-events file))
                 (history (seq-find
                           (lambda (entry)
                             (equal (opencode-record--event-type entry)
                                    "history.snapshot"))
                           events))
                 (messages (alist-get 'messages
                                      (opencode-record--event-data history))))
            (should history)
            (should (= 1 (length messages)))
            (should (equal "msg_1" (map-nested-elt (car messages) '(info id))))))
      (when (and file (file-exists-p file))
        (delete-file file))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-renders-delta-only-trace ()
  "Recording replay feeds captured events through the reducer."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (dolist (event
                     (list
                      `((time . "t")
                        (seq . 1)
                        (type . "record.start")
                        (data . ((session . ((sessionID . "ses_test")
                                             (directory . ,temporary-file-directory)
                                             (status . "idle"))))))
                      '((time . "t")
                        (seq . 2)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_1")
                                                (partID . "prt_1")
                                                (field . "text")
                                                (delta . "TRACE"))))))
                      '((time . "t")
                        (seq . 3)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_1")
                                                (partID . "prt_1")
                                                (field . "text")
                                                (delta . "_OK"))))))
                      '((time . "t")
                        (seq . 4)
                        (type . "event.decoded")
                        (data . ((type . "session.status")
                                 (properties . ((sessionID . "ses_test")
                                                (status . ((type . "idle"))))))))))
              (insert (json-encode event) "\n")))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (should (string-match-p "TRACE_OK"
                                    (opencode-event-tests--buffer-text)))
            (should (gethash "msg_1" opencode-rendered-message-ids))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-renders-delta-only-without-idle ()
  "Recording replay shows delta-only output even before an idle event."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (dolist (event
                     (list
                      `((time . "t")
                        (seq . 1)
                        (type . "record.start")
                        (data . ((session . ((sessionID . "ses_test")
                                             (directory . ,temporary-file-directory)
                                             (status . "busy"))))))
                      '((time . "t")
                        (seq . 2)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_1")
                                                (partID . "prt_1")
                                                (field . "text")
                                                (delta . "LIVE"))))))
                      '((time . "t")
                        (seq . 3)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_1")
                                                (partID . "prt_1")
                                                (field . "text")
                                                (delta . "_NOW"))))))))
              (insert (json-encode event) "\n")))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (should (string-match-p "LIVE_NOW"
                                    (opencode-event-tests--buffer-text)))
            (should opencode-assistant-messages)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-history-snapshot-fills-missed-tail ()
  "Recording replay applies history snapshots when stream tails are missing."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (dolist (event
                     (list
                      `((time . "t")
                        (seq . 1)
                        (type . "record.start")
                        (data . ((session . ((sessionID . "ses_test")
                                             (directory . ,temporary-file-directory)
                                             (status . "busy"))))))
                      '((time . "t")
                        (seq . 2)
                        (type . "event.decoded")
                        (data . ((type . "message.updated")
                                 (properties . ((sessionID . "ses_test")
                                                (info . ((id . "msg_1")
                                                         (sessionID . "ses_test")
                                                         (role . "assistant"))))))))
                      '((time . "t")
                        (seq . 3)
                        (type . "event.decoded")
                        (data . ((type . "message.part.updated")
                                 (properties . ((sessionID . "ses_test")
                                                (part . ((id . "prt_1")
                                                         (sessionID . "ses_test")
                                                         (messageID . "msg_1")
                                                         (type . "text")
                                                         (text . ""))))))))
                      '((time . "t")
                        (seq . 4)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_1")
                                                (partID . "prt_1")
                                                (field . "text")
                                                (delta . "kernel drivers"))))))
                      `((time . "t")
                        (seq . 5)
                        (type . "history.snapshot")
                        (data . ((sessionID . "ses_test")
                                 (directory . ,temporary-file-directory)
                                 (messageCount . 1)
                                 (tailCount . 1)
                                 (messages . ,(list
                                               (opencode-event-tests--stored-assistant
                                                "msg_1" "prt_1"
                                                "kernel drivers/device nodes present."
                                                t))))))
                      '((time . "t")
                        (seq . 6)
                        (type . "record.stop")
                        (data . ((path . "trace.jsonl"))))))
              (insert (json-encode event) "\n")))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (let ((text (opencode-event-tests--buffer-text)))
              (should (string-match-p "kernel drivers/device nodes present" text))
              (should-not (string-match-p "kernel driverskernel" text)))
            (should (gethash "msg_1" opencode-rendered-message-ids))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-history-snapshot-updates-rendered-tail ()
  "History snapshots complete text that was already finalized from events."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (dolist (event
                     (list
                      `((time . "t")
                        (seq . 1)
                        (type . "record.start")
                        (data . ((session . ((sessionID . "ses_test")
                                             (directory . ,temporary-file-directory)
                                             (status . "busy"))))))
                      '((time . "t")
                        (seq . 2)
                        (type . "event.decoded")
                        (data . ((type . "message.updated")
                                 (properties . ((sessionID . "ses_test")
                                                (info . ((id . "msg_1")
                                                         (sessionID . "ses_test")
                                                         (role . "assistant"))))))))
                      '((time . "t")
                        (seq . 3)
                        (type . "event.decoded")
                        (data . ((type . "message.part.updated")
                                 (properties . ((sessionID . "ses_test")
                                                (part . ((id . "prt_1")
                                                         (sessionID . "ses_test")
                                                         (messageID . "msg_1")
                                                         (type . "text")
                                                         (text . ""))))))))
                      '((time . "t")
                        (seq . 4)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_1")
                                                (partID . "prt_1")
                                                (field . "text")
                                                (delta . "kernel drivers"))))))
                      `((time . "t")
                        (seq . 5)
                        (type . "event.decoded")
                        (data . ,(opencode-event-tests--step-finish "msg_1")))
                      `((time . "t")
                        (seq . 6)
                        (type . "history.snapshot")
                        (data . ((sessionID . "ses_test")
                                 (directory . ,temporary-file-directory)
                                 (messageCount . 1)
                                 (tailCount . 1)
                                 (messages . ,(list
                                               (opencode-event-tests--stored-assistant
                                                "msg_1" "prt_1"
                                                "kernel drivers/device nodes present."
                                                t))))))
                      '((time . "t")
                        (seq . 7)
                        (type . "record.stop")
                        (data . ((path . "trace.jsonl"))))))
              (insert (json-encode event) "\n")))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (let ((text (opencode-event-tests--buffer-text)))
              (should (string-match-p "kernel drivers/device nodes present" text))
              (should-not (string-match-p "kernel driverskernel" text)))
            (should (gethash "msg_1" opencode-rendered-message-ids))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-complete-history-snapshot-rebuilds-buffer ()
  "Complete history snapshots rebuild replay output in persisted order."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (dolist (event
                     (list
                      `((time . "t")
                        (seq . 1)
                        (type . "record.start")
                        (data . ((session . ((sessionID . "ses_test")
                                             (directory . ,temporary-file-directory)
                                             (status . "busy"))))))
                      '((time . "t")
                        (seq . 2)
                        (type . "event.decoded")
                        (data . ((type . "message.part.delta")
                                 (properties . ((sessionID . "ses_test")
                                                (messageID . "msg_a")
                                                (partID . "prt_a")
                                                (field . "text")
                                                (delta . "Downloaded"))))))
                      `((time . "t")
                        (seq . 3)
                        (type . "history.snapshot")
                        (data . ((sessionID . "ses_test")
                                 (directory . ,temporary-file-directory)
                                 (messageCount . 3)
                                 (tailCount . 3)
                                 (messages . ,(list
                                               (opencode-event-tests--stored-user
                                                "msg_user" "Please download the book")
                                               (opencode-event-tests--stored-assistant
                                                "msg_a" "prt_a"
                                                "The index is an Apache directory listing."
                                                t)
                                               (opencode-event-tests--stored-assistant
                                                "msg_b" "prt_b"
                                                "Downloaded the Markdown sources into `book/`."
                                                t))))))
                      '((time . "t")
                        (seq . 4)
                        (type . "record.stop")
                        (data . ((path . "trace.jsonl"))))))
              (insert (json-encode event) "\n")))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (let ((text (opencode-event-tests--buffer-text)))
              (should (string-match-p "^> Please download the book" text))
              (should (string-match-p "The index is an Apache directory listing" text))
              (should (string-match-p "Downloaded the Markdown sources" text))
              (should-not (string-match-p "Downloaded The index" text))
              (should-not (string-match-p "book/`\.> " text))
              (should (string-match-p "book/`\.\n\n> " text))
              (should-not (string-match-p "Recording contains 0 API error" text))
              (should (string-match-p "Issues: none" text))
              (should (equal "idle" opencode-session-status))
              (should-not opencode-assistant-messages)
              (should (>= (how-many "^> " (point-min) (point-max)) 2)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-partial-replace-needs-markers ()
  "History-tail repair must not replace unrelated global substrings."
  (opencode-event-tests--with-state
   (insert "Downloaded The index is an Apache directory listing.")
   (let ((stream-part-texts (make-hash-table :test 'equal))
         (message (opencode-event-tests--stored-assistant
                   "msg_1" "prt_1"
                   "Downloaded the Markdown sources into `book/`." t)))
     (puthash "prt_1" "Downloaded" stream-part-texts)
     (should-not (opencode-record--replace-visible-partial-message
                  message stream-part-texts))
     (should (equal "Downloaded The index is an Apache directory listing."
                    (opencode-event-tests--buffer-text))))))

(ert-deftest opencode-event-stream-quiet-schedules-and-runs-suspect-reconcile ()
  "Quiet streamed deltas trigger bounded suspect reconciliation."
  (opencode-event-tests--with-session-buffer
   (let ((opencode-session-stream-quiet-reconcile-delay 60)
         scheduled
         reconcile-call
         callback-present)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (delay repeat function &rest args)
                  (setq scheduled (list delay repeat function args))
                  (timer-create))))
       (opencode-session--note-message-stream-event
        "msg_1" "prt_1" "hello" "text"))
     (should (timerp opencode-session--stream-quiet-timer))
     (should (equal 60 (car scheduled)))
     (should (eq #'opencode-session--run-stream-quiet-reconcile
                 (nth 2 scheduled)))
     (setq opencode-session-status "busy")
     (cl-letf (((symbol-function 'opencode-session--reconcile-suspect-history)
                (lambda (session-id &optional callback)
                  (setq reconcile-call session-id
                        callback-present (functionp callback)))))
       (opencode-session--run-stream-quiet-reconcile
        "ses_test" (current-buffer) "msg_1"))
     (should-not opencode-session--stream-quiet-timer)
     (should (equal "ses_test" reconcile-call))
     (should callback-present)
     (should (equal "stream-quiet"
                    (plist-get (gethash "msg_1" opencode-session--stream-message-states)
			       :suspect)))
     (setq opencode-session-status "idle"
           opencode-assistant-messages '(("msg_1" text . 1))
           reconcile-call nil
           callback-present nil)
     (cl-letf (((symbol-function 'opencode-session--reconcile-suspect-history)
                (lambda (session-id &optional callback)
                  (setq reconcile-call session-id
                        callback-present (functionp callback)))))
       (opencode-session--run-stream-quiet-reconcile
        "ses_test" (current-buffer) "msg_1"))
     (should (equal "ses_test" reconcile-call))
     (should callback-present))))

(ert-deftest opencode-event-record-replay-report-does-not-steal-reasoning-margin ()
  "Replay report must not be inserted inside reasoning overlays."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (dolist (event
                     (list
                      `((time . "t")
                        (seq . 1)
                        (type . "record.start")
                        (data . ((session . ((sessionID . "ses_test")
                                             (directory . ,temporary-file-directory)
                                             (status . "idle"))))))
                      `((time . "t")
                        (seq . 2)
                        (type . "event.decoded")
                        (data . ,(opencode-event-tests--reasoning-updated
                                  "msg_1" "prt_1" "hidden thought")))))
              (insert (json-encode event) "\n")))
          (let ((opencode-show-reasoning t))
            (setq buffer (opencode-record-replay file)))
          (with-current-buffer buffer
            (let* ((report-pos (save-excursion
                                 (goto-char (point-min))
                                 (and (search-forward "Recording:" nil t)
                                      (match-beginning 0))))
                   (reasoning-pos (save-excursion
                                    (goto-char (point-min))
                                    (and (search-forward "hidden thought" nil t)
                                         (match-beginning 0)))))
              (should report-pos)
              (should reasoning-pos)
              (should-not (seq-some (lambda (overlay)
                                      (overlay-get overlay 'line-prefix))
                                    (overlays-at report-pos)))
              (should (seq-some (lambda (overlay)
                                  (overlay-get overlay 'line-prefix))
                                (overlays-at reasoning-pos))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-record-replay-reports-unknown-event-types ()
  "Recording replay reports backend event types it intentionally skips."
  (let* ((opencode-record-directory (make-temp-file "opencode-record" t))
         (file (expand-file-name "trace.jsonl" opencode-record-directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (json-encode
              '((time . "t")
                (seq . 1)
                (type . "record.start")
                (data . ((session . ((sessionID . "ses_test")
                                     (directory . nil)
                                     (status . "idle")))))))
             "\n")
            (insert
             (json-encode
              '((time . "t")
                (seq . 2)
                (type . "event.decoded")
                (data . ((type . "backend.future.command")
                         (properties . ((sessionID . "ses_test")
                                        (payload . "kept")))))))
             "\n"))
          (setq buffer (opencode-record-replay file))
          (with-current-buffer buffer
            (let ((text (opencode-event-tests--buffer-text)))
              (should (string-match-p "Skipped events: 1" text))
              (should (string-match-p
                       "Skipped backend event types: backend.future.command=1"
                       text)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory opencode-record-directory t))))

(ert-deftest opencode-event-fuzz-smoke-replay-invariants ()
  "Stateful fuzz smoke catches replay, prompt, and merge regressions."
  (should (equal '(:passed 12 :start-seed 1)
                 (opencode-fuzz-run :seeds 12 :start-seed 1))))

(provide 'opencode-event-tests)
;;; opencode-event-tests.el ends here
