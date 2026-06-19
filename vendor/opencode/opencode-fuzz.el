;;; opencode-fuzz.el --- Stateful OpenCode reducer fuzzing -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal, test

;;; Commentary:

;; Deterministic fuzz cases for the OpenCode Emacs frontend.  These generate a
;; persisted session history plus an imperfect SSE recording, replay it through
;; the real reducer/recorder path, and assert invariants that catch message
;; loss, duplicated tool output, prompt attachment, and cross-message merges.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'opencode)
(require 'opencode-record)
(require 'opencode-sessions)
(require 'subr-x)

(defgroup opencode-fuzz nil
  "Stateful fuzzing for the OpenCode Emacs frontend."
  :group 'opencode)

(defcustom opencode-fuzz-default-seeds 50
  "Default number of seeds to run in `opencode-fuzz-run'."
  :type 'integer
  :group 'opencode-fuzz)

(defcustom opencode-fuzz-keep-failures nil
  "Whether `opencode-fuzz-run' keeps failing trace files."
  :type 'boolean
  :group 'opencode-fuzz)

(defvar opencode-fuzz--modulus 2147483647
  "Modulus used by the deterministic OpenCode fuzz PRNG.")

(cl-defstruct opencode-fuzz--case
  seed session-id directory messages events complete-history-p metadata file)

(defun opencode-fuzz--rng (seed)
  "Return a mutable deterministic PRNG state for SEED."
  (cons (max 1 (abs (or seed 1))) nil))

(defun opencode-fuzz--random (rng limit)
  "Return a deterministic integer below LIMIT using RNG."
  (setcar rng (mod (+ (* 1103515245 (car rng)) 12345)
                   opencode-fuzz--modulus))
  (if (and (integerp limit) (> limit 0))
      (mod (car rng) limit)
    (car rng)))

(defun opencode-fuzz--chance-p (rng numerator denominator)
  "Return non-nil with NUMERATOR/DENOMINATOR probability using RNG."
  (< (opencode-fuzz--random rng denominator) numerator))

(defun opencode-fuzz--pick (rng items)
  "Return a deterministic item from ITEMS using RNG."
  (nth (opencode-fuzz--random rng (length items)) items))

(defun opencode-fuzz--sequence-list (value)
  "Return VALUE as a list, accepting vectors and singleton values."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun opencode-fuzz--session-id (seed)
  "Return a session id for fuzz SEED."
  (format "ses_fuzz_%06d" seed))

(defun opencode-fuzz--message-id (seed index role)
  "Return a message id for SEED, INDEX, and ROLE."
  (format "msg_fuzz_%06d_%s_%02d" seed role index))

(defun opencode-fuzz--part-id (seed index kind &optional subindex)
  "Return a part id for SEED, INDEX, KIND, and optional SUBINDEX."
  (format "prt_fuzz_%06d_%02d_%s_%02d" seed index kind (or subindex 0)))

(defun opencode-fuzz--call-id (seed index subindex)
  "Return a tool call id for SEED, INDEX, and SUBINDEX."
  (format "call_fuzz_%06d_%02d_%02d" seed index subindex))

(defun opencode-fuzz--user-message (session-id seed index text)
  "Return a persisted user message for SESSION-ID, SEED, INDEX, and TEXT."
  `((info . ((id . ,(opencode-fuzz--message-id seed index "user"))
             (sessionID . ,session-id)
             (role . "user")
             (time . ((created . ,(+ 1000 index))))))
    (parts . [((id . ,(opencode-fuzz--part-id seed index "user"))
               (sessionID . ,session-id)
               (messageID . ,(opencode-fuzz--message-id seed index "user"))
               (type . "text")
               (text . ,text))])))

(defun opencode-fuzz--task-part (session-id seed index subindex title status)
  "Return a persisted task tool part for SESSION-ID and fuzz identifiers."
  (let ((message-id (opencode-fuzz--message-id seed index "assistant"))
        (call-id (opencode-fuzz--call-id seed index subindex)))
    `((id . ,(opencode-fuzz--part-id seed index "tool" subindex))
      (sessionID . ,session-id)
      (messageID . ,message-id)
      (type . "tool")
      (callID . ,call-id)
      (tool . "task")
      (state . ((status . ,status)
                (input . ((description . ,title)
                          (prompt . ,(format "%s prompt" title))))
                (title . ,title)
                ,@(when (equal status "completed")
                    `((output . ,(format "%s done" title)))))))))

(defun opencode-fuzz--question-part (session-id seed index title answer)
  "Return a completed question tool part for SESSION-ID and fuzz identifiers."
  (let ((message-id (opencode-fuzz--message-id seed index "assistant")))
    `((id . ,(opencode-fuzz--part-id seed index "question"))
      (sessionID . ,session-id)
      (messageID . ,message-id)
      (type . "tool")
      (callID . ,(opencode-fuzz--call-id seed index 9))
      (tool . "question")
      (state . ((status . "completed")
                (input . ((questions . [((question . ,title)
                                         (header . "Fuzz Question")
                                         (options . [((label . "Yes")
                                                      (description . "Proceed"))
                                                     ((label . "No")
                                                      (description . "Stop"))])
                                         (multiple . :json-false))])))
                (output . ,(format "Answered %s" answer))
                (title . "Asked 1 question")
                (metadata . ((answers . [[,answer]])
                             (truncated . :json-false))))))))

(defun opencode-fuzz--assistant-message
    (session-id seed index text &optional reasoning tools question)
  "Return a completed assistant message for SESSION-ID and fuzz identifiers."
  (let ((message-id (opencode-fuzz--message-id seed index "assistant"))
        parts)
    (when reasoning
      (push `((id . ,(opencode-fuzz--part-id seed index "reasoning"))
              (sessionID . ,session-id)
              (messageID . ,message-id)
              (type . "reasoning")
              (text . ,reasoning)
              (time . ((end . ,(+ 2000 index)))))
            parts))
    (dolist (tool tools)
      (push tool parts))
    (when question
      (push question parts))
    (push `((id . ,(opencode-fuzz--part-id seed index "text"))
            (sessionID . ,session-id)
            (messageID . ,message-id)
            (type . "text")
            (text . ,text)
            (time . ((end . ,(+ 3000 index)))))
          parts)
    `((info . ((id . ,message-id)
               (sessionID . ,session-id)
               (role . "assistant")
               (finish . "stop")
               (time . ((completed . ,(+ 4000 index))))))
      (parts . ,(vconcat (nreverse parts))))))

(defun opencode-fuzz--assistant-event (session-id message-id &optional completed)
  "Return a message.updated assistant event for SESSION-ID and MESSAGE-ID."
  `((type . "message.updated")
    (properties . ((sessionID . ,session-id)
                   (info . ((id . ,message-id)
                            (sessionID . ,session-id)
                            (role . "assistant")
                            (tokens . ((input . 0)
                                       (output . 0)
                                       (reasoning . 0)
                                       (cache . ((read . 0) (write . 0)))))
                            ,@(when completed
                                '((finish . "stop")
                                  (time . ((completed . 1)))))))))))

(defun opencode-fuzz--text-seed-event (session-id message-id part-id)
  "Return a text seed event for SESSION-ID, MESSAGE-ID, and PART-ID."
  `((type . "message.part.updated")
    (properties . ((sessionID . ,session-id)
                   (part . ((id . ,part-id)
                            (sessionID . ,session-id)
                            (messageID . ,message-id)
                            (type . "text")
                            (text . "")))))))

(defun opencode-fuzz--text-delta-event (session-id message-id part-id delta)
  "Return a text delta event for SESSION-ID, MESSAGE-ID, PART-ID, and DELTA."
  `((type . "message.part.delta")
    (properties . ((sessionID . ,session-id)
                   (messageID . ,message-id)
                   (partID . ,part-id)
                   (field . "text")
                   (delta . ,delta)))))

(defun opencode-fuzz--step-finish-event (session-id message-id index)
  "Return a step-finish event for SESSION-ID, MESSAGE-ID, and INDEX."
  `((type . "message.part.updated")
    (properties . ((sessionID . ,session-id)
                   (part . ((id . ,(format "%s_step_%02d" message-id index))
                            (sessionID . ,session-id)
                            (messageID . ,message-id)
                            (type . "step-finish")
                            (reason . "stop")))))))

(defun opencode-fuzz--tool-event (session-id part)
  "Return a tool update event for SESSION-ID and persisted PART."
  `((type . "message.part.updated")
    (properties . ((sessionID . ,session-id)
                   (part . ,part)))))

(defun opencode-fuzz--status-event (session-id status)
  "Return a session.status event for SESSION-ID and STATUS."
  `((type . "session.status")
    (properties . ((sessionID . ,session-id)
                   (status . ((type . ,status)))))))

(defun opencode-fuzz--split-text (rng text)
  "Split TEXT into deterministic stream chunks using RNG."
  (let ((pos 0)
        chunks)
    (while (< pos (length text))
      (let* ((remaining (- (length text) pos))
             (size (min remaining (+ 1 (opencode-fuzz--random rng 18)))))
        (push (substring text pos (+ pos size)) chunks)
        (setq pos (+ pos size))))
    (nreverse chunks)))

(defun opencode-fuzz--message-text (message)
  "Return first persisted text part from MESSAGE."
  (cl-loop for part in (opencode-fuzz--sequence-list (alist-get 'parts message))
           when (and (equal "text" (alist-get 'type part nil nil #'string=))
                     (stringp (alist-get 'text part)))
           return (alist-get 'text part)))

(defun opencode-fuzz--message-text-part-id (message)
  "Return first persisted text part id from MESSAGE."
  (cl-loop for part in (opencode-fuzz--sequence-list (alist-get 'parts message))
           when (equal "text" (alist-get 'type part nil nil #'string=))
           return (alist-get 'id part)))

(defun opencode-fuzz--message-tool-parts (message)
  "Return persisted tool parts from MESSAGE."
  (cl-loop for part in (opencode-fuzz--sequence-list (alist-get 'parts message))
           when (equal "tool" (alist-get 'type part nil nil #'string=))
           collect part))

(defun opencode-fuzz--push-stream-events (rng session-id message events)
  "Push degraded stream EVENTS for MESSAGE and return events."
  (let* ((message-id (map-nested-elt message '(info id)))
         (part-id (opencode-fuzz--message-text-part-id message))
         (text (or (opencode-fuzz--message-text message) ""))
         (chunks (opencode-fuzz--split-text rng text))
         (drop-tail (opencode-fuzz--chance-p rng 1 3))
         (send-finish (opencode-fuzz--chance-p rng 2 3))
         (send-complete-message (opencode-fuzz--chance-p rng 1 4))
         (limit (if drop-tail
                    (max 1 (- (length chunks)
                              (+ 1 (opencode-fuzz--random rng 2))))
                  (length chunks))))
    (push (opencode-fuzz--status-event session-id "busy") events)
    (push (opencode-fuzz--assistant-event session-id message-id) events)
    (when (opencode-fuzz--chance-p rng 3 4)
      (push (opencode-fuzz--text-seed-event session-id message-id part-id)
            events))
    (dotimes (i limit)
      (push (opencode-fuzz--text-delta-event
             session-id message-id part-id (nth i chunks))
            events))
    (when (opencode-fuzz--chance-p rng 1 3)
      ;; Idle may arrive too early, before a late delta.
      (push (opencode-fuzz--status-event session-id "idle") events)
      (when (< limit (length chunks))
        (push (opencode-fuzz--text-delta-event
               session-id message-id part-id (nth limit chunks))
              events)))
    (dolist (tool (opencode-fuzz--message-tool-parts message))
      (when (opencode-fuzz--chance-p rng 1 2)
        (let ((running (copy-tree tool)))
          (setf (alist-get 'status (alist-get 'state running)) "running")
          (push (opencode-fuzz--tool-event session-id running) events)))
      (when (opencode-fuzz--chance-p rng 1 2)
        (push (opencode-fuzz--tool-event session-id tool) events)))
    (when send-finish
      (push (opencode-fuzz--step-finish-event session-id message-id 0) events))
    (when send-complete-message
      (push (opencode-fuzz--assistant-event session-id message-id t) events))
    (when (opencode-fuzz--chance-p rng 1 2)
      (push (opencode-fuzz--status-event session-id "busy") events))
    events))

(defun opencode-fuzz--make-case (seed)
  "Generate a deterministic fuzz case for SEED."
  (let* ((rng (opencode-fuzz--rng seed))
         (session-id (opencode-fuzz--session-id seed))
         (directory temporary-file-directory)
         (turns (+ 2 (opencode-fuzz--random rng 4)))
         (complete-history-p (not (zerop (mod seed 3))))
         messages metadata events)
    (dotimes (turn turns)
      (let* ((common (opencode-fuzz--pick
                      rng '("Downloaded" "The index" "Prototype" "biasing"
                            "RAPTOR" "subagent" "question" "book")))
             (user-tag (format "FUZZ-%06d-U%02d" seed turn))
             (assistant-tag (format "FUZZ-%06d-A%02d" seed turn))
             (user-text (format "%s user asks about %s." user-tag common))
             (assistant-text
              (format "%s-BEGIN %s answer mentions Downloaded The index and %s. %s-END"
                      assistant-tag common common assistant-tag))
             (reasoning (and (opencode-fuzz--chance-p rng 1 3)
                             (format "%s-REASON checking %s" assistant-tag common)))
             (tools nil)
             question)
        (when (opencode-fuzz--chance-p rng 1 3)
          (let ((title (format "%s-TASK-%02d" assistant-tag turn)))
            (push (opencode-fuzz--task-part session-id seed turn 1 title
                                            "completed")
                  tools)))
        (when (opencode-fuzz--chance-p rng 1 5)
          (setq question
                (opencode-fuzz--question-part
                 session-id seed turn
                 (format "%s-QUESTION proceed?" assistant-tag)
                 "Yes")))
        (let ((user (opencode-fuzz--user-message session-id seed turn user-text))
              (assistant (opencode-fuzz--assistant-message
                          session-id seed turn assistant-text reasoning
                          (nreverse tools) question)))
          (push user messages)
          (push assistant messages)
          (push (list :role 'user :tag user-tag :text user-text) metadata)
          (push (list :role 'assistant :tag assistant-tag
                      :begin (format "%s-BEGIN" assistant-tag)
                      :end (format "%s-END" assistant-tag)
                      :text assistant-text
                      :tool-titles (delq
                                    nil
                                    (mapcar
                                     (lambda (part)
                                       (unless (equal "question"
                                                      (alist-get 'tool part nil
                                                                 nil #'string=))
                                         (map-nested-elt part '(state title))))
                                     (opencode-fuzz--message-tool-parts
                                      assistant))))
                metadata)
          (setq events (opencode-fuzz--push-stream-events
                        rng session-id assistant events)))))
    (make-opencode-fuzz--case
     :seed seed
     :session-id session-id
     :directory directory
     :messages (nreverse messages)
     :events (nreverse events)
     :complete-history-p complete-history-p
     :metadata (nreverse metadata))))

(defun opencode-fuzz--record-entry (seq type data)
  "Return a JSONL record entry with SEQ, TYPE, and DATA."
  `((time . "fuzz")
    (seq . ,seq)
    (type . ,type)
    (data . ,data)))

(defun opencode-fuzz--write-case (case directory)
  "Write CASE JSONL into DIRECTORY and return the file path."
  (let ((file (expand-file-name
               (format "opencode-fuzz-%06d.jsonl"
                       (opencode-fuzz--case-seed case))
               directory))
        (seq 0))
    (with-temp-file file
      (insert (json-encode
               (opencode-fuzz--record-entry
                (cl-incf seq) "record.start"
                `((session . ((sessionID . ,(opencode-fuzz--case-session-id case))
                              (directory . ,(opencode-fuzz--case-directory case))
                              (status . "busy"))))))
              "\n")
      (dolist (event (opencode-fuzz--case-events case))
        (insert (json-encode
                 (opencode-fuzz--record-entry
                  (cl-incf seq) "event.decoded" event))
                "\n"))
      (let* ((messages (opencode-fuzz--case-messages case))
             (message-count (length messages))
             (tail-count message-count))
        (insert (json-encode
                 (opencode-fuzz--record-entry
                  (cl-incf seq) "history.snapshot"
                  `((sessionID . ,(opencode-fuzz--case-session-id case))
                    (directory . ,(opencode-fuzz--case-directory case))
                    (messageCount . ,(if (opencode-fuzz--case-complete-history-p case)
                                         message-count
                                       (+ message-count 3)))
                    (tailCount . ,tail-count)
                    (messages . ,messages))))
                "\n"))
      (insert (json-encode
               (opencode-fuzz--record-entry
                (cl-incf seq) "record.stop" `((path . ,file))))
              "\n"))
    (setf (opencode-fuzz--case-file case) file)
    file))

(defun opencode-fuzz--count-literal (needle haystack)
  "Return the number of literal NEEDLE occurrences in HAYSTACK."
  (let ((start 0)
        (count 0))
    (while (and (not (string-empty-p needle))
                (string-match-p (regexp-quote needle) haystack start))
      (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun opencode-fuzz--position (needle haystack)
  "Return first literal NEEDLE position in HAYSTACK."
  (and (string-match (regexp-quote needle) haystack)
       (match-beginning 0)))

(defun opencode-fuzz--fail (case message &rest args)
  "Signal a fuzz failure for CASE with formatted MESSAGE and ARGS."
  (error "OpenCode fuzz seed %s failed: %s"
         (opencode-fuzz--case-seed case)
         (apply #'format message args)))

(defun opencode-fuzz--assert-replay-invariants (case buffer)
  "Assert replay invariants for CASE in BUFFER."
  (with-current-buffer buffer
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (last-pos -1))
      (dolist (entry (opencode-fuzz--case-metadata case))
        (pcase (plist-get entry :role)
          ('user
           (let* ((tag (plist-get entry :tag))
                  (count (opencode-fuzz--count-literal tag text))
                  (pos (opencode-fuzz--position tag text)))
             (unless (= count 1)
               (opencode-fuzz--fail case "user tag %s count=%d" tag count))
             (unless (and pos (> pos last-pos))
               (opencode-fuzz--fail case "user tag %s is out of order" tag))
             (setq last-pos pos)))
          ('assistant
           (let* ((begin (plist-get entry :begin))
                  (end (plist-get entry :end))
                  (begin-count (opencode-fuzz--count-literal begin text))
                  (end-count (opencode-fuzz--count-literal end text))
                  (pos (opencode-fuzz--position begin text)))
             (unless (= begin-count 1)
               (opencode-fuzz--fail case "assistant begin %s count=%d"
                                    begin begin-count))
             (unless (= end-count 1)
               (opencode-fuzz--fail case "assistant end %s count=%d"
                                    end end-count))
             (unless (and pos (> pos last-pos))
               (opencode-fuzz--fail case "assistant tag %s is out of order"
                                    begin))
             (when (string-match-p (regexp-quote (concat end "> ")) text)
               (opencode-fuzz--fail case "prompt attached to %s" end))
             (dolist (title (plist-get entry :tool-titles))
               (when title
                 (let ((count (opencode-fuzz--count-literal title text)))
                   (unless (= count 1)
                     (opencode-fuzz--fail case "tool title %s count=%d"
                                          title count)))))
             (setq last-pos pos)))))
      (unless (equal opencode-session-status "idle")
        (opencode-fuzz--fail case "final status is %S" opencode-session-status))
      (when opencode-assistant-messages
        (opencode-fuzz--fail case "active messages remain: %S"
                             (mapcar #'car opencode-assistant-messages)))
      (unless (string-match-p "\n\n> " text)
        (opencode-fuzz--fail case "no separated prompt found"))
      (when (string-match-p "Issues: \(?!none\)" text)
        (opencode-fuzz--fail case "replay reported issues"))
      t)))

(cl-defun opencode-fuzz-run (&key seeds (start-seed 1) keep-failures)
  "Run OpenCode stateful fuzzing.
SEEDS controls the number of generated cases.  START-SEED selects the first
seed.  When KEEP-FAILURES is non-nil, leave failing JSONL files on disk."
  (let ((seeds (or seeds opencode-fuzz-default-seeds))
        (keep-failures (or keep-failures opencode-fuzz-keep-failures))
        (directory (make-temp-file "opencode-fuzz" t))
        failures)
    (unwind-protect
        (dotimes (offset seeds)
          (let* ((seed (+ start-seed offset))
                 (case (opencode-fuzz--make-case seed))
                 (file (opencode-fuzz--write-case case directory))
                 buffer)
            (condition-case err
                (progn
                  (setq buffer (opencode-record-replay file))
                  (opencode-fuzz--assert-replay-invariants case buffer))
              (error
               (push (list :seed seed :file file :error (error-message-string err))
                     failures)
               (when keep-failures
                 (copy-file file
                            (expand-file-name
                             (file-name-nondirectory file)
                             default-directory)
                            t))))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))
      (unless (or keep-failures failures)
        (delete-directory directory t)))
    (when failures
      (error "OpenCode fuzz failed: %S" (nreverse failures)))
    (list :passed seeds :start-seed start-seed)))

(defun opencode-fuzz-print (&optional seeds start-seed)
  "Run OpenCode fuzzing and print a compact result.
Optional SEEDS and START-SEED are used noninteractively."
  (interactive "P")
  (let* ((seed-count (if (numberp seeds) seeds opencode-fuzz-default-seeds))
         (start (or start-seed 1))
         (result (opencode-fuzz-run :seeds seed-count :start-seed start)))
    (princ (format "%S\n" result))))

(provide 'opencode-fuzz)
;;; opencode-fuzz.el ends here
