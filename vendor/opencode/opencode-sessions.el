;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'comint)
(require 'magit)
(require 'mailcap)
(require 'markdown-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-format-tool-calls)
(require 'opencode-record)
(require 'project)
(require 'vtable)
(require 'yank-media)

(declare-function opencode--queue-questions "opencode-question" (buffer question-id questions))
(declare-function opencode--handle-message "opencode" (data))
(declare-function opencode--download-slash-commands "opencode" (directory))
(declare-function opencode--slash-commands-for-directory "opencode" (directory))
(declare-function opencode-session--refresh-permission-for-tool "opencode-permission" (part))

(defvar opencode-session-control-mode-map
  (define-keymap
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "U" 'opencode-unshare-all-sessions
    "v" 'opencode-session-control-toggle-verbose))

(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")

(defun opencode--tab-dispatch ()
  "Cycle session agent, or move to next button if point is on one."
  (interactive)
  (if (button-at (point))
      (forward-button 1)
    (call-interactively #'opencode-cycle-session-agent)))

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block
    "C-c C-c" 'opencode-abort-session
    "C-c C-p" 'opencode-respond-permission
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "C-c r" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-add-buffer-dwim
    "C-c s" 'opencode-share-session
    "C-c u" 'opencode-unshare-session
    "C-c U" 'opencode-unshare-all-sessions
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
    "C-c D" 'opencode-delete-message
    "C-c R" 'opencode-revert-message
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-subagent))

(defun opencode--format-file-line-reference (file line session-directory)
  "Return a backticked FILE:LINE reference for SESSION-DIRECTORY."
  (let ((display-file (if (and session-directory
                               (file-in-directory-p file session-directory))
                          (file-relative-name file session-directory)
                        file)))
    (format "`%s:%d`" display-file line)))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-session-control-mode-map
		    "r" 'opencode-sessions-redisplay
		    "n" 'opencode-new-session
		    "gv" 'opencode-session-control-toggle-verbose)
  (evil-define-key* 'insert opencode-session-mode-map
		    "@" 'opencode-add-subagent
		    "/" 'opencode-insert-slash-command))

(defvar-local opencode-session-id nil
  "Session id for the current opencode session buffer.")

(defvar-local opencode-session-directory nil
  "Stable project directory for the current session buffer.")

(defvar-local opencode-session-tokens 0
  "Tokens consumed by the current session.")

(defvar-local opencode-session-status "idle"
  "Status of the current opencode session (busy or idle).")

(defvar-local opencode-session-agent nil
  "Currently active agent for this buffer's session.")

(defvar-local opencode-session-agents nil
  "List of agents for the current session.
Buffer local so we can configure models and variants per agent per session.")

(defvar-local opencode-session-pending-questions nil
  "Pending questions for this session buffer, awaiting user response.
When non-nil, contains a cons cell (QUESTION-ID . QUESTIONS-VECTOR).")

(defvar-local opencode-session-pending-permission nil
  "Pending permission requests for this session buffer, as a list of plists.
Each plist has keys :id, :session-id, :type, :title, and :marker.
Requests are processed FIFO.")

(defvar-local opencode--temp-files nil
  "Temporary files to delete after sending or killing a session buffer.")

(defvar-local opencode--files-edited-this-turn nil
  "Files edited by opencode during the current turn.")

(defvar opencode-session-buffers
  (make-hash-table :test 'equal)
  "A mapping of session ids to Emacs buffers.")

(defvar-local opencode--tool-calls-displayed nil
  "A hash table containing all callID that have already been displayed.")

(defvar-local opencode-part-text (make-hash-table :test 'equal)
  "Mapping of streaming part ids to accumulated text.")

(defvar-local opencode-part-sent (make-hash-table :test 'equal)
  "Mapping of streaming part ids to rendered text length.")

(defvar-local opencode-part-replay-text (make-hash-table :test 'equal)
  "Mapping of replayed part ids to text loaded from stored history.")

(defvar-local opencode-part-region-start (make-hash-table :test 'equal)
  "Mapping of provisionally rendered part ids to region start markers.")

(defvar-local opencode-part-region-end (make-hash-table :test 'equal)
  "Mapping of provisionally rendered part ids to region end markers.")

(defvar-local opencode-part-message (make-hash-table :test 'equal)
  "Mapping of streaming part ids to message ids.")

(defvar-local opencode-message-roles (make-hash-table :test 'equal)
  "Mapping of message ids to backend roles.")

(defvar-local opencode-assistant-messages
    nil
  "An alist mapping all currently updating assistant message ids, to start pos.")

(defvar-local opencode-rendered-message-ids (make-hash-table :test 'equal)
  "Assistant message ids already rendered in the current session buffer.")

(defvar-local opencode-session--history-validated-message-ids
    (make-hash-table :test 'equal)
  "Message ids whose rendered transcript visibility was already validated.")

(defvar-local opencode-session--interrupted-message-ids (make-hash-table :test 'equal)
  "Assistant message ids that already received an interrupted marker.")

(defvar-local opencode--reconcile-timer nil
  "Timer used to reconcile missed assistant output for this session buffer.")

(defvar-local opencode-session--history-reconcile-in-flight nil
  "Non-nil while a persisted-history reconcile request is in flight.")

(defvar-local opencode-session--history-reconcile-callbacks nil
  "Callbacks to run after the current persisted-history reconcile finishes.")

(defvar-local opencode-session--suspect-reconcile-timer nil
  "Timer used to reconcile only suspect active stream state.")

(defvar-local opencode-session--stream-quiet-timer nil
  "Timer used to check a busy stream after deltas go quiet.")

(defvar-local opencode-session--stream-message-states (make-hash-table :test 'equal)
  "Cheap stream-integrity metadata keyed by assistant message id.")

(defvar-local opencode--question-recover-timer nil
  "Timer used to recover missed pending questions for this session buffer.")

(defvar-local opencode-session--question-recover-last-finished nil
  "Float timestamp when pending-question recovery last finished.")

(defvar-local opencode--pending-question-tools (make-hash-table :test 'equal)
  "Mapping from question tool identity to pending question ids.")

(defvar-local opencode--completed-question-tools (make-hash-table :test 'equal)
  "Question tool identities that completed before recovery found a request.")

(defvar-local opencode--displayed-question-ids (make-hash-table :test 'equal)
  "Question request ids already rendered in this session buffer.")

(defvar-local opencode--pending-permission-tools (make-hash-table :test 'equal)
  "Mapping from permission tool identity to pending permission ids.")

(defvar-local opencode-shell-echo (make-hash-table :test 'equal)
  "Mapping of assistant message ids to bash output prefixes to strip.")

(defvar-local opencode-shell-calls (make-hash-table :test 'equal)
  "Mapping of shell call ids to the event source that claimed them.")

(defvar-local opencode-session--bootstrapping nil
  "Non-nil while replaying stored history into this session buffer.")

(defvar-local opencode-session--queued-events nil
  "Events received for this session while bootstrapping history.")

(defvar-local opencode-session--draining-queued-events nil
  "Non-nil while applying events queued during bootstrap.")

(defvar-local opencode-session--status-poll-timer nil
  "Timer used to poll session status when status events are missed.")

(defvar-local opencode-session--status-poll-in-flight nil
  "Non-nil while a fallback status poll request is outstanding.")

(defvar-local opencode-session--live-render-timer nil
  "Timer used to coalesce live markdown rendering for this session buffer.")

(defvar-local opencode-session--live-render-type nil
  "Pending live markdown render type for this session buffer.")

(defvar-local opencode-session--live-render-start nil
  "Pending live markdown render start for this session buffer.")

(defvar-local opencode-session--stream-batch-timer nil
  "Timer used to coalesce streaming output for this session buffer.")

(defvar-local opencode-session--stream-batch-type nil
  "Pending streaming output type for this session buffer.")

(defvar-local opencode-session--stream-batch-start nil
  "Pending streaming output start marker for this session buffer.")

(defvar-local opencode-session--stream-batch-strings nil
  "Pending streaming output chunks for this session buffer.")

(defvar-local opencode-session--idle-finalize-timer nil
  "Timer used to defer idle finalization while late deltas may arrive.")

(defvar-local opencode-session--idle-finalize-callback nil
  "Callback to run after deferred idle finalization.")

(defcustom opencode-session-status-poll-initial-delay 0.5
  "Seconds to wait before the first fallback status poll."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-status-poll-interval 5
  "Seconds between fallback status polls while a session remains busy."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-question-recover-attempts 5
  "Number of attempts to recover a missed pending question."
  :type 'integer
  :group 'opencode)

(defcustom opencode-session-question-recover-interval 1.0
  "Seconds between retries when recovering a missed pending question."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-question-recover-cooldown 15
  "Seconds to wait before starting another empty question recovery cycle."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-live-markdown-delay 0.2
  "Seconds to coalesce live markdown rendering while text streams."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-live-markdown-visible-only t
  "When non-nil, live markdown rendering only runs for visible session buffers.
Background session buffers still show streaming raw text and render markdown when
the active assistant block finishes."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-session-stream-batch-delay 0.05
  "Seconds to coalesce streaming output before buffer insertion."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-idle-finalize-delay 0.5
  "Seconds to wait before finalizing active output after idle.
Some OpenCode streams can deliver `session.idle' before the final text deltas
for the active message.  A short quiet period prevents splitting a live message
or showing the prompt in the middle of the remaining output."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-suspect-reconcile-delay 0.5
  "Seconds to wait before reconciling suspect active stream state."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-suspect-history-limit 50
  "Maximum persisted messages to inspect when reconciling suspect state."
  :type 'integer
  :group 'opencode)

(defcustom opencode-session-history-sync-limit 50
  "Maximum persisted messages to fetch for broad live history sync.
Session open and explicit history replay may still fetch complete history.  Live
reconciliation uses this bounded fetch so status/timer events cannot download a
large multi-megabyte session transcript during ordinary streaming."
  :type 'integer
  :group 'opencode)

(defcustom opencode-session-suspect-stream-max-age 30
  "Seconds to keep unfinished stream metadata eligible for suspect sync.
Active assistant messages are always eligible.  This bound prevents old streams
that never received a finish event from widening later targeted reconciles."
  :type 'number
  :group 'opencode)

(defcustom opencode-session-stream-quiet-reconcile-delay 2.0
  "Seconds without deltas before checking busy streamed output against history."
  :type 'number
  :group 'opencode)

(defun opencode-session--tool-key (message-id call-id)
  "Return a stable key for tool MESSAGE-ID and CALL-ID."
  (when (and (stringp message-id) (stringp call-id))
    (concat message-id "\0" call-id)))

(defun opencode-session--question-tool-key (message-id call-id)
  "Return a stable key for question tool MESSAGE-ID and CALL-ID."
  (opencode-session--tool-key message-id call-id))

(defun opencode-session--call-id (part)
  "Return PART's tool call id."
  (let-alist part
    .callID))

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((agent-name (alist-get 'name opencode-session-agent))
         (agent-cell (and agent-name
                          (cl-loop for cell on opencode-session-agents
                                   when (string= agent-name
                                                 (alist-get 'name (car cell)))
                                   return cell))))
    (when agent-cell
      (setcar agent-cell opencode-session-agent)))
  (let* ((pos (cl-position-if (lambda (agent)
                                (string= (alist-get 'name agent)
                                         (alist-get 'name opencode-session-agent)))
                              opencode-session-agents))
         (next (nth (1+ pos) opencode-session-agents)))
    (setf opencode-session-agent (or next (car opencode-session-agents)))
    (unless (string= "primary" (alist-get 'mode opencode-session-agent))
      (opencode-cycle-session-agent)))
  (force-mode-line-update))

(defun opencode--session-buffer-p (buf)
  "Return non-nil if BUF is a opencode session buffer.
Accepts (\"name\" . buffer) to work as a read-buffer predicate."
  (with-current-buffer (cl-etypecase buf
                         ((or string buffer) buf)
                         (list (cdr buf)))
    opencode-session-id))

(defun opencode-select-open-session ()
  "Select among open session buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Switch to: " nil t 'opencode--session-buffer-p)))

(defun opencode-consult-sessions ()
  "Run `consult-line-multi' across all open opencode sessions."
  (interactive)
  (if (require 'consult nil t)
      (progn
        (declare-function consult-line-multi "consult")
        (consult-line-multi '(:predicate opencode--session-buffer-p)))
    (user-error "This requires consult")))

(defun opencode-visit-last-idle ()
  "Open the most recent session to notify as idle."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-open-session (pop opencode-alerted-sessions))
    (message "No idle sessions")))

(defun opencode-select-session ()
  "Select and open a session from the current project using completion."
  (interactive)
  (opencode-api-sessions sessions
    (opencode--select-sessions "Session: "
                               (seq-remove (lambda (session)
                                             (alist-get 'parentID session))
                                           sessions)
                               (format "No sessions in %s" default-directory)
                               :pop-to-buffer nil)))

(cl-defun opencode--select-sessions (prompt sessions no-sessions-message &key (pop-to-buffer t))
  "Select a session from SESSIONS to open.
Use PROMPT and display NO-SESSIONS-MESSAGE if SESSIONS is empty.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer."
  (if sessions
      (opencode-open-session
       (opencode--annotated-completion
        prompt
        (cl-loop for session in sessions
                 collect (let-alist session
                           (list .title
                                 session
                                 (opencode--format-time-ago
                                  (opencode--time-ago
                                   session 'updated))
                                 (opencode--time-ago
                                  session 'updated)))))
       :pop-to-buffer pop-to-buffer)
    (message no-sessions-message)))

(defun opencode-select-idle ()
  "Select a session that hasn't been visited since it went idle."
  (interactive)
  (opencode--select-sessions "Session: " opencode-alerted-sessions "No idle sessions"))

(defun opencode--collect-all-models ()
  "Collect all models from `opencode-providers' as a list.
Each element is (display-name . (provider-id provider-name model-id))."
  (let (result)
    (dolist (provider opencode-providers)
      (let-alist provider
        (dolist (model-entry .models)
          (let ((model (cdr model-entry)))
            (push (list (alist-get 'name model)
                        `((providerID . ,.id)
                          (modelID . ,(alist-get 'id model)))
                        .name)
                  result)))))
    (nreverse result)))

(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let ((model (opencode--annotated-completion
                     "Model: "
                     (opencode--collect-all-models))))
    (setf (alist-get 'model opencode-session-agent) model
          opencode-last-model model)
    (when-let ((variant (alist-get 'variant opencode-session-agent)))
      (unless (alist-get variant
                         (alist-get 'variants (opencode--current-model)))
        (setq opencode-session-agent
              (assq-delete-all 'variant opencode-session-agent))))))

(defun opencode--current-model ()
  "Return the active model for this session."
  (let-alist opencode-session-agent
    (map-nested-elt
     (seq-find (lambda (provider)
                 (string= .model.providerID (alist-get 'id provider)))
               opencode-providers)
     `(models ,(intern .model.modelID)))))

(defun opencode-select-variant ()
  "Select a variant for the current model."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let ((variants (alist-get 'variants (opencode--current-model))))
    (if variants
        (setf (alist-get 'variant opencode-session-agent)
              (opencode--annotated-completion
               "Variant: "
               (cl-loop for (variant . options) in
                        variants
                        collect (list (symbol-name variant)
                                      variant
                                      (format "%s" options)))))
      (message "No variants"))))

(defun opencode-select-child-session ()
  "Open a child (subagent) session of the current session."
  (interactive)
  (opencode-api-session-children (opencode-session-id)
      children
    (opencode--select-sessions "Subagent: " children "No children")))

(defun opencode-open-parent ()
  "Open the parent of the current session."
  (interactive)
  (opencode-api-session (opencode-session-id)
      session
    (opencode-api-session ((alist-get 'parentID session))
        parent
      (opencode-open-session parent))))

(defun opencode-share-session (&optional session)
  "Share SESSION or the current session."
  (interactive)
  (opencode-api-share-session ((or (alist-get 'id session)
                                   opencode-session-id))
      session
    (let ((url (map-nested-elt session '(share url))))
      (gui-select-text url)
      (message "Copied to clipboard: %s" url))))

(defun opencode-unshare-session (&optional session)
  "Unshare SESSION or the current session."
  (interactive)
  (opencode-api-unshare-session ((or (alist-get 'id session)
                                     opencode-session-id))
      _session
    (message "Session no longer shared")))

(defun opencode-unshare-all-sessions ()
  "Unshare all sessions across all projects."
  (interactive)
  (opencode-api-projects projects
    (dolist (project projects)
      (let ((default-directory (alist-get 'worktree project)))
        (opencode-api-sessions sessions
          (dolist (session sessions)
            (when (alist-get 'share session)
              (opencode-unshare-session session))))))))

(defun opencode-insert-slash-command ()
  "Insert an opencode slash command."
  (interactive)
  (if (= (point) (cdr comint-last-prompt))
      (let* ((directory (opencode--normalize-directory default-directory))
             (builtins '(((name . "compact")
                          (description . "summarize the session to reduce context size"))
                         ((name . "summarize")
                          (description . "alias for compact"))))
             (commands (or (alist-get directory opencode--slash-commands-by-directory
                                      nil nil #'string=)
                           (and (fboundp 'opencode--slash-commands-for-directory)
                                (opencode--slash-commands-for-directory directory))))
             (command (opencode--annotated-completion
                       "Slash command: "
                       (cl-loop for command in (append builtins commands)
                                collect (let-alist command
                                          (list
                                           .name
                                           .name
                                           .description))))))
        (when command
          (insert (concat "/" command))))
    (call-interactively #'self-insert-command)))

(defun opencode--session-status-indicator ()
  "Return mode line indicator for session status."
  (let-alist opencode-session-agent
    (let* ((agent (pcase .name
                    ("Planner-Sisyphus" "Planner")
                    ((and (pred stringp) name) name)
                    (_ "OpenCode")))
           (model (ignore-errors (opencode--current-model)))
           (model-name (or (alist-get 'name model) "unknown"))
           (context-limit (or (map-nested-elt model '(limit context)) 1))
           (status (pcase opencode-session-status
                     ("busy" "⏳")
                     ("idle" "🚀")
                     (_ "")))
           (context-used (* 100 (/ (float opencode-session-tokens)
                                   context-limit))))
      (if (< (window-width) 115)
          (format "[🤖 %s] %.0f%%%% %s  " agent context-used status)
        (format "[🤖 %s - %s] %.0f%%%% %s  "
                agent
                (concat model-name
                        (when .variant
                          (propertize (format " %s" .variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (when-let (buffer (gethash session-id opencode-session-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((changed (not (equal opencode-session-status status))))
          (setq opencode-session-status status)
          (when (equal status "busy")
            (opencode-session--cancel-idle-finalize-timer))
          (when changed
            (force-mode-line-update)))
        (when (and (equal status "idle")
                   (timerp opencode-session--status-poll-timer))
          (cancel-timer opencode-session--status-poll-timer)
          (setq opencode-session--status-poll-timer nil))))))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (visual-line-mode)
  (font-lock-mode -1)
  (cursor-intangible-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image)
  (add-hook 'comint-input-filter-functions 'opencode--render-input-markdown nil t))

(defun opencode-session--directory ()
  "Return the stable project directory for the current session buffer."
  (let ((directory (and (stringp opencode-session-directory)
                        (file-directory-p opencode-session-directory)
                        (file-name-as-directory
                         (file-truename opencode-session-directory)))))
    (when (and directory
               (not (file-equal-p directory default-directory)))
      (setq default-directory directory))
    (or directory default-directory)))

(defun opencode-yank-code-block ()
  "Yank the markdown code block under point."
  (interactive)
  (save-excursion
    (markdown-backward-block)
    (copy-region-as-kill (point)
                         (progn (markdown-forward-block)
                                (point)))))

(defun opencode-kill-session (&optional session)
  "Kill SESSION."
  (interactive)
  (opencode-api-delete-session ((or (alist-get 'id session)
                                    opencode-session-id))
      result
    (unless result
      (error "Unable to delete session"))
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-this-buffer))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--mimetype (file)
  "Return guess of mimetype for FILE."
  (pcase (file-name-extension file)
    ((or "org" "ts" (pred null)) "text/plain")
    (ext (or (mailcap-extension-to-mime ext)
             "text/plain"))))

(defun opencode--remove-label (overlay after _beg _end &optional _len)
  "Called AFTER deleting OVERLAY, remove the associated part from context."
  (when (and after
             (not (eq this-command 'comint-send-input)))
    (let ((file-url (overlay-get overlay 'file-url))
          (buffer-name (overlay-get overlay 'buffer-name))
          (region-id (overlay-get overlay 'region-id))
          (agent-name (overlay-get overlay 'agent-name))
          (ov-start (overlay-start overlay))
          (ov-end (overlay-end overlay)))
      (setq opencode--extra-parts
            (seq-remove (lambda (part)
                          (let-alist part
                            (cond
                             (file-url (string= file-url .url))
                             (buffer-name (string= buffer-name .metadata.buffer-name))
                             (region-id (eq region-id .metadata.region-id))
                             (agent-name (string= agent-name .name)))))
                        opencode--extra-parts))
      (delete-region ov-start ov-end)
      (delete-overlay overlay))))

(defun opencode--in-label-p ()
  "Return non-nil if point is inside (or at the edge of) a label overlay."
  (cl-loop for ov in (overlays-at (point))
           thereis (overlay-get ov 'opencode-label)))

(defun opencode--insert-intangible (name extra-prop extra-value)
  "Insert an intangible label with NAME (buffer or filename).
Assign the overlay EXTRA-PROP with EXTRA-VALUE."
  (let* ((start (point))
         (end (progn (insert "`")
                     (insert (propertize name 'cursor-intangible t))
                     (insert (propertize "`" 'rear-nonsticky t))
                     (point)))
         (ov (make-overlay start end)))
    (overlay-put ov 'opencode-label t)
    (overlay-put ov extra-prop extra-value)
    (overlay-put ov 'display (propertize name 'face 'markdown-inline-code-face))
    (overlay-put ov 'modification-hooks '(opencode--remove-label))
    ov))

(defun opencode--add-file-to-context (file &optional mime display-name)
  "Add FILE to context with optional MIME and DISPLAY-NAME."
  (let* ((display-name (or display-name
                           (opencode--relative-path-for-display file)))
         (mime (or mime (opencode--mimetype file)))
         (url (concat "file://" file)))
    (push `((type . file)
            (filename . ,display-name)
            (mime . ,mime)
            (url . ,url))
          opencode--extra-parts)
    (opencode--insert-intangible display-name 'file-url url)
    (insert " ")))

(defun opencode--image-extension (mime)
  "Return a file extension for image MIME."
  (or (car (split-string (or (cadr (split-string mime "/")) "")
                         "[;+]" t))
      "img"))

(defun opencode--yank-image (type data)
  "Add pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted into the current prompt"))
  (let* ((mime (symbol-name type))
         (extension (opencode--image-extension mime))
         (file (make-temp-file "opencode-image-" nil
                               (concat "." extension))))
    (condition-case error
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil file nil 'silent))
          (push file opencode--temp-files)
          (opencode--add-file-to-context file mime (file-name-nondirectory file)))
      (error
       (ignore-errors (delete-file file))
       (signal (car error) (cdr error))))))

(defun opencode--delete-temp-files (files)
  "Delete temporary FILES created by opencode."
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file)))))

(defun opencode--session-cleanup ()
  "Clean up resources for the current session buffer."
  (opencode-session--cancel-live-render-timer)
  (opencode-session--cancel-stream-batch-timer)
  (opencode-session--cancel-idle-finalize-timer)
  (opencode-session--cancel-suspect-reconcile-timer)
  (opencode-session--cancel-stream-quiet-timer)
  (when-let ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (opencode--delete-temp-files opencode--temp-files)
  (setq opencode--temp-files nil))

(defmacro with-last-opencode-session (&rest body)
  "Run BODY with the last used opencode session buffer active."
  `(if opencode-last-session-buffer
       (if (buffer-live-p opencode-last-session-buffer)
           (with-current-buffer opencode-last-session-buffer
             (unless (comint-after-pmark-p)
               (goto-char (point-max)))
             ,@body)
         (user-error "Last selected OpenCode session buffer is no longer active"))
     (user-error "Open an OpenCode session first")))

(defun opencode-add-file (&optional file)
  "Add a FILE to context, or prompt for file in current project."
  (interactive)
  (with-last-opencode-session
   (let* ((project (project-current t))
          (file (or file
                    (project--read-file-name project
                                             "Add to context"
                                             (project-files project)
                                             nil
                                             'file-name-history))))
     (opencode--add-file-to-context file))))

(defun opencode-add-file-dwim ()
  "If in Dired, add all marked files, or file at point if none marked.
Otherwise add the current buffer's file.
Otherwise prompt for file in current project."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (if-let (files (dired-get-marked-files))
          (mapc #'opencode-add-file files)
        (if-let (file (dired-get-filename nil t))
            (opencode-add-file file)
          (user-error "No file at point, and no marked files")))
    (opencode-add-file (buffer-file-name))))

(defun opencode-add-buffer-dwim ()
  "Add current buffer or prompt for one to add."
  (interactive)
  (let ((buffer (if opencode-session-id
                    (read-buffer "Add to context: ")
                  (buffer-name))))
    (with-last-opencode-session
     (push `((type . text)
             (text . ,(concat
                       (format "<buffer name=\"%s\">" buffer)
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                       "</buffer>"))
             (synthetic . t)
             (metadata . ((buffer-name . ,buffer))))
           opencode--extra-parts)
     (opencode--insert-intangible buffer 'buffer-name buffer))))

(defun opencode-add-region ()
  "Add the active region to context."
  (interactive)
  (if (use-region-p)
      (let ((region (buffer-substring-no-properties (region-beginning)
                                                    (region-end)))
            (region-id (gensym)))
        (with-last-opencode-session
         (push `((type . text)
                 (text . ,(concat "<region>" region "</region>"))
                 (synthetic . t)
                 (metadata . ((region-id . ,region-id))))
               opencode--extra-parts)
         (opencode--insert-intangible
          (format "region: %s"
                  (truncate-string-to-width region 24 0 nil (truncate-string-ellipsis)))
          'region-id region-id)
         (insert " ")))
    (user-error "No active region")))

(defun opencode-insert-line-reference ()
  "Insert a backticked file:line reference into the last OpenCode session.
Uses the current buffer's file and line number at point.  This inserts the
same `file:line' form that OpenCode chat already recognizes and buttonizes."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((file buffer-file-name)
        (line (line-number-at-pos)))
    (with-last-opencode-session
     (insert (opencode--format-file-line-reference
              file line default-directory)
             " "))))

(defun opencode-add-subagent ()
  "Insert a subagent mention into the current input."
  (interactive)
  (if (comint-after-pmark-p)
      (let ((subagents (cl-loop for agent in opencode-agents
                                when (string= "subagent" (alist-get 'mode agent))
                                collect agent)))
        (unless subagents
          (user-error "No subagents available"))
        (let ((name (opencode--annotated-completion
                     "Subagent: "
                     (cl-loop for agent in subagents
                              collect (let-alist agent
                                        (list .name .name .description))))))
          (push `((type . "agent")
                  (name . ,name))
                opencode--extra-parts)
          (opencode--insert-intangible (concat "@" name) 'agent-name name)))
    (call-interactively #'self-insert-command)))

(defun opencode--primary-agent ()
  "Return the primary OpenCode agent, or the first available agent."
  (or (seq-find (lambda (agent)
                  (string= "primary" (alist-get 'mode agent)))
                opencode-agents)
      (car opencode-agents)))

(defun opencode--effective-session-agent ()
  "Return a valid agent for the current session."
  (unless opencode-session-agents
    (setq opencode-session-agents (copy-tree opencode-agents)))
  (let ((agent (or opencode-session-agent
                   (seq-find (lambda (agent)
                               (string= "primary" (alist-get 'mode agent)))
                             opencode-session-agents)
                   (car opencode-session-agents)
                   (opencode--primary-agent))))
    (unless agent
      (user-error "No OpenCode agent is available"))
    (unless (memq agent opencode-session-agents)
      (setq agent (copy-tree agent))
      (push agent opencode-session-agents))
    (unless (alist-get 'name agent)
      (user-error "Current OpenCode agent has no name"))
    (unless (alist-get 'model agent)
      (setf (alist-get 'model agent) opencode-last-model))
    (setq opencode-session-agent agent)
    agent))

(defun opencode-session--normalize-questions (questions)
  "Return QUESTIONS as a vector with option lists converted to vectors."
  (vconcat
   (mapcar (lambda (question)
             (let ((copy (copy-tree question)))
               (when-let ((options (alist-get 'options copy)))
                 (setf (alist-get 'options copy) (vconcat options)))
               copy))
           questions)))

(defun opencode-session--sync-pending-question ()
  "Fetch and queue a backend pending question for the current session.
Return non-nil if a pending question was found."
  (when (and opencode-api-url opencode-session-id)
    (when-let* ((questions
                 (condition-case nil
                     (plz 'get (concat opencode-api-url "/question")
                       :as (lambda ()
                             (unless (string-empty-p (buffer-string))
                               (json-parse-buffer :array-type 'list
                                                  :object-type 'alist)))
                       :headers `(("Content-Type" . "application/json")
                                  ,(cons "x-opencode-directory"
                                         (opencode-session--directory))
                                  ,(opencode--auth-header))
                       :then 'sync)
                   (error nil)))
                (question
                 (seq-find (lambda (question)
                             (string= opencode-session-id
                                      (alist-get 'sessionID question)))
                           questions))
                (question-id (alist-get 'id question))
                (question-list (alist-get 'questions question))
                (tool (alist-get 'tool question))
                (tool-key (and tool
                               (opencode-session--question-tool-key
                                (alist-get 'messageID tool)
                                (alist-get 'callID tool)))))
      (when (and (stringp question-id)
                 (string-prefix-p "que" question-id)
                 (not (and tool-key
                           (gethash tool-key opencode--completed-question-tools))))
        (let ((normalized (opencode-session--normalize-questions question-list)))
          (opencode-session--record-question-tool question-id tool)
          (unless (equal (car-safe opencode-session-pending-questions)
                         question-id)
            (opencode--queue-questions (current-buffer) question-id normalized))
          t)))))

(defun opencode-session--schedule-question-recover (session-id &optional delay attempts)
  "Schedule pending-question recovery for SESSION-ID.
Retry ATTEMPTS times after DELAY seconds while no question is queued."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp opencode--question-recover-timer)
          (cancel-timer opencode--question-recover-timer))
        (setq opencode--question-recover-timer
              (run-at-time
               (or delay 0.25) nil
               (lambda (session-id buffer attempts)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq opencode--question-recover-timer nil)
                     (let ((recovered
                            (unless opencode-session-pending-questions
                              (opencode-session--sync-pending-question))))
                       (if (or opencode-session-pending-questions
                               recovered
                               (<= attempts 1))
                           (setq opencode-session--question-recover-last-finished
                                 (float-time))
                         (opencode-session--schedule-question-recover
                          session-id opencode-session-question-recover-interval
                          (1- attempts)))))))
               session-id buffer
               (or attempts opencode-session-question-recover-attempts)))))))

(defun opencode-session--question-recover-cooling-down-p ()
  "Return non-nil when pending-question recovery is cooling down."
  (and (numberp opencode-session--question-recover-last-finished)
       (numberp opencode-session-question-recover-cooldown)
       (> opencode-session-question-recover-cooldown 0)
       (< (- (float-time) opencode-session--question-recover-last-finished)
          opencode-session-question-recover-cooldown)))

(defun opencode-session--maybe-schedule-question-recover (session-id &optional delay)
  "Schedule pending-question recovery for SESSION-ID if none is active."
  (when (and opencode-api-url session-id)
    (when-let ((buffer (gethash session-id opencode-session-buffers)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (or opencode-session-pending-questions
                      (timerp opencode--question-recover-timer)
                      (opencode-session--question-recover-cooling-down-p))
            (opencode-session--schedule-question-recover session-id delay)))))))

(defun opencode-session--clear-pending-question (question-id)
  "Clear pending QUESTION-ID from the current session buffer."
  (when (stringp question-id)
    (maphash (lambda (key value)
               (when (equal value question-id)
                 (remhash key opencode--pending-question-tools)))
             opencode--pending-question-tools))
  (when (and (stringp question-id)
             (equal (car-safe opencode-session-pending-questions) question-id))
    (setq opencode-session-pending-questions nil)))

(defun opencode-session--record-question-tool (question-id tool)
  "Associate QUESTION-ID with TOOL metadata in the current session buffer."
  (when (and (stringp question-id)
             (string-prefix-p "que" question-id)
             (listp tool))
    (let-alist tool
      (when-let ((key (opencode-session--question-tool-key .messageID .callID)))
        (when (and (hash-table-p opencode--tool-calls-displayed)
                   (stringp .callID))
          (puthash .callID t opencode--tool-calls-displayed))
        (unless (gethash key opencode--completed-question-tools)
          (puthash key question-id opencode--pending-question-tools))))))

(defun opencode-session--sequence-list (value)
  "Return VALUE as a list when VALUE is a list or vector."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)
        ((null value) nil)
        (t (list value))))

(defun opencode-session--format-question-answer (answer)
  "Format a single recorded question ANSWER."
  (let ((values (opencode-session--sequence-list answer)))
    (string-join
     (delq nil
           (mapcar (lambda (value)
                     (when value
                       (string-trim (format "%s" value))))
                   values))
     ", ")))

(defun opencode-session--format-completed-question-tool (part)
  "Return transcript text for a completed question tool PART."
  (let-alist part
    (when (and (string= .tool "question")
               (member .state.status '("completed" "error"))
               .state.input.questions)
      (let* ((questions (opencode-session--normalize-questions
                         .state.input.questions))
             (answers (opencode-session--sequence-list
                       .state.metadata.answers))
             (answer-lines
              (cl-loop for answer in answers
                       for text = (opencode-session--format-question-answer answer)
                       for index from 1
                       unless (string-empty-p text)
                       collect (format "Answer%s: %s"
                                       (if (= (length answers) 1)
                                           ""
                                         (format " %d" index))
                                       text)))
             (fallback (and (stringp .state.output)
                            (string-trim .state.output))))
        (string-join
         (delq nil
               (list (opencode--format-questions questions)
                     (and answer-lines
                          (string-join answer-lines "\n"))
                     (and (not answer-lines)
                          fallback
                          (not (string-empty-p fallback))
                          fallback)))
         "\n")))))

(defun opencode-session--insert-completed-question-tool (part)
  "Insert completed question tool PART into the transcript."
  (when-let ((text (opencode-session--format-completed-question-tool part)))
    (unless (string-empty-p (string-trim text))
      (let-alist part
        (when (and (hash-table-p opencode--tool-calls-displayed)
                   (stringp .callID))
          (puthash .callID t opencode--tool-calls-displayed)))
      (opencode--insert-block-with-margin text 'opencode-tool-margin-highlight)
      (opencode--maybe-insert-block-spacing))))

(defun opencode-session--clear-question-for-tool (part)
  "Clear pending question associated with tool PART, if any."
  (let-alist part
    (when (and (string= .tool "question")
               (member .state.status '("completed" "error")))
      (when-let ((key (opencode-session--question-tool-key .messageID .callID)))
        (puthash key t opencode--completed-question-tools)
        (when-let ((question-id (or (gethash key opencode--pending-question-tools)
                                    .requestID .questionID
                                    .state.requestID .state.questionID)))
          (opencode-session--clear-pending-question question-id))))))

(defun opencode-session--stash-shell-echo (part)
  "Record bash tool output from PART for echo stripping."
  (let-alist part
    (when (and (string= .tool "bash")
               (stringp .messageID)
               (string= .state.status "completed")
               (stringp .state.output))
      (let ((output (replace-regexp-in-string "\\`\n+" "" .state.output)))
        (unless (string-empty-p (string-trim output))
          (let ((values (list output))
                (trimmed (replace-regexp-in-string "\n+\\'" "" output)))
            (unless (string= trimmed output)
              (push trimmed values))
            (puthash .messageID values opencode-shell-echo)))))))

(defun opencode-session--strip-shell-echo (message-id chunk)
  "Strip stashed shell echo for MESSAGE-ID from CHUNK."
  (if-let ((values (and (stringp message-id)
                        (gethash message-id opencode-shell-echo))))
      (progn
        (remhash message-id opencode-shell-echo)
        (catch 'stripped
          (dolist (value (sort (copy-sequence values)
                               (lambda (a b) (> (length a) (length b)))))
            (when (and (stringp value)
                       (not (string-empty-p value))
                       (string-prefix-p value chunk))
              (throw 'stripped
                     (replace-regexp-in-string
                      "\\`\n+" "" (substring chunk (length value))))))
          chunk))
    chunk))

(defun opencode-session--shell-call-source (call-id)
  "Return the recorded source for shell CALL-ID."
  (when (stringp call-id)
    (plist-get (gethash call-id opencode-shell-calls) :source)))

(defun opencode-session--handle-shell-started (session-id call-id command)
  "Render first-class shell start event for SESSION-ID and CALL-ID."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (and (buffer-live-p buffer) (stringp call-id))
      (with-current-buffer buffer
        (unless (eq (opencode-session--shell-call-source call-id) 'legacy-tool)
          (puthash call-id (list :source 'shell-event
                                 :command command)
                   opencode-shell-calls)
          (unless (gethash call-id opencode--tool-calls-displayed)
            (puthash call-id t opencode--tool-calls-displayed)
            (opencode--insert-tool-block
             "bash" `((command . ,(or command ""))))))))))

(defun opencode-session--handle-shell-ended (session-id call-id output)
  "Render first-class shell end event for SESSION-ID and CALL-ID."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (and (buffer-live-p buffer) (stringp call-id))
      (with-current-buffer buffer
        (when (eq (opencode-session--shell-call-source call-id) 'shell-event)
          (let ((command (plist-get (gethash call-id opencode-shell-calls)
                                    :command)))
            (opencode--maybe-insert-tool-output
             `((tool . "bash")
               (callID . ,call-id)
               (state . ((status . "completed")
                         (input . ((command . ,command)))
                         (output . ,output)))))))))))

(defun opencode-session--status-type-for-session (statuses session-id)
  "Return the status type for SESSION-ID from STATUSES."
  (let ((status (cond
                 ((hash-table-p statuses) (gethash session-id statuses))
                 ((listp statuses) (alist-get session-id statuses nil nil #'string=)))))
    (cond
     ((hash-table-p status) (gethash "type" status))
     ((listp status) (alist-get 'type status)))))

(defun opencode-session--active-status-p (status)
  "Return non-nil when STATUS means a session is still active."
  (member status '("busy" "waiting" "starting")))

(defun opencode-session--active-output-quiet-p ()
  "Return non-nil when active output has stopped receiving deltas."
  (when (and opencode-assistant-messages
             (not opencode-session--stream-batch-strings)
             (not (timerp opencode-session--stream-quiet-timer))
             (not (timerp opencode-session--suspect-reconcile-timer))
             (not (timerp opencode--reconcile-timer)))
    (let ((now (float-time))
          last-delta)
      (dolist (entry opencode-assistant-messages)
        (when-let ((state (gethash (car entry)
                                   opencode-session--stream-message-states)))
          (when-let ((delta-time (plist-get state :last-delta)))
            (setq last-delta (if last-delta
                                 (max last-delta delta-time)
                               delta-time)))))
      (or (null last-delta)
          (>= (- now last-delta)
              opencode-session-stream-quiet-reconcile-delay)))))

(defun opencode-session--take-last (items limit)
  "Return the last LIMIT ITEMS."
  (if (and (integerp limit) (> limit 0))
      (let ((extra (- (length items) limit)))
        (if (> extra 0)
            (nthcdr extra items)
          items))
    items))

(defun opencode-session--history-reconcile-start (callback)
  "Return non-nil if a persisted-history reconcile may start.
If another request is already in flight, queue CALLBACK and return nil."
  (if opencode-session--history-reconcile-in-flight
      (progn
        (when callback
          (push callback opencode-session--history-reconcile-callbacks))
        nil)
    (setq opencode-session--history-reconcile-in-flight t)
    (when callback
      (push callback opencode-session--history-reconcile-callbacks))
    t))

(defun opencode-session--history-reconcile-finish ()
  "Clear persisted-history reconcile state and run queued callbacks."
  (setq opencode-session--history-reconcile-in-flight nil)
  (let ((callbacks (nreverse opencode-session--history-reconcile-callbacks)))
    (setq opencode-session--history-reconcile-callbacks nil)
    (dolist (callback callbacks)
      (funcall callback))))

(defun opencode-session--stream-state (message-id)
  "Return stream-integrity state for MESSAGE-ID, creating it if needed."
  (when (and (stringp message-id) (not (string-empty-p message-id)))
    (or (gethash message-id opencode-session--stream-message-states)
        (puthash message-id
                 (list :started (float-time)
                       :delta-count 0
                       :delta-chars 0
                       :finished nil)
                 opencode-session--stream-message-states))))

(defun opencode-session--note-message-stream-event (message-id &optional part-id delta type)
  "Record cheap stream metadata for MESSAGE-ID, PART-ID, DELTA, and TYPE."
  (when-let ((state (opencode-session--stream-state message-id)))
    (let ((now (float-time)))
      (setq state (plist-put state :last-event now))
      (when part-id
        (setq state (plist-put state :last-part-id part-id)))
      (when type
        (setq state (plist-put state :last-type type)))
      (when (and (stringp delta) (not (string-empty-p delta)))
        (setq state (plist-put state :last-delta now))
        (setq state (plist-put state :delta-count
                               (1+ (or (plist-get state :delta-count) 0))))
        (setq state (plist-put state :delta-chars
                               (+ (or (plist-get state :delta-chars) 0)
                                  (length delta))))
        (when (member type '("text" "reasoning"))
          (opencode-session--schedule-stream-quiet-reconcile
           opencode-session-id message-id)))
      (puthash message-id state opencode-session--stream-message-states))))

(defun opencode-session--note-message-stream-finished (message-id)
  "Mark MESSAGE-ID finished in cheap stream metadata."
  (opencode-session--cancel-stream-quiet-timer)
  (when-let ((state (opencode-session--stream-state message-id)))
    (setq state (plist-put state :finished t))
    (setq state (plist-put state :finished-time (float-time)))
    (puthash message-id state opencode-session--stream-message-states)))

(defun opencode-session--forget-message-stream-state (message-id)
  "Forget stream-integrity metadata for MESSAGE-ID."
  (when (stringp message-id)
    (remhash message-id opencode-session--stream-message-states)))

(defun opencode-session--recent-stream-state-p (state now)
  "Return non-nil when STATE is recent enough at NOW for suspect sync."
  (let ((time (or (plist-get state :suspect-time)
                  (plist-get state :last-delta)
                  (plist-get state :last-event))))
    (and (numberp time)
         (or (not (numberp opencode-session-suspect-stream-max-age))
             (<= (- now time) opencode-session-suspect-stream-max-age)))))

(defun opencode-session--suspect-message-ids ()
  "Return message ids whose stream state should be checked against history."
  (let ((now (float-time))
        active-ids
        ids)
    (dolist (entry opencode-assistant-messages)
      (when (stringp (car entry))
        (push (car entry) ids)
        (push (car entry) active-ids)))
    (maphash
     (lambda (message-id state)
       (when (and (not (plist-get state :finished))
                  (not (member message-id active-ids))
                  (or (plist-get state :last-delta)
                      (plist-get state :suspect))
                  (opencode-session--recent-stream-state-p state now))
         (push message-id ids)))
     opencode-session--stream-message-states)
    (delete-dups ids)))

(defun opencode-session--mark-stream-suspect (reason &optional message-id)
  "Mark current stream state suspicious for REASON and optional MESSAGE-ID."
  (if message-id
      (when-let ((state (opencode-session--stream-state message-id)))
        (setq state (plist-put state :suspect reason))
        (setq state (plist-put state :suspect-time (float-time)))
        (puthash message-id state opencode-session--stream-message-states))
    (dolist (entry opencode-assistant-messages)
      (opencode-session--mark-stream-suspect reason (car entry)))))

(defun opencode-session--message-id-in-list-p (message ids)
  "Return non-nil when MESSAGE's id is in IDS."
  (when-let ((message-id (map-nested-elt message '(info id))))
    (member message-id ids)))

(defun opencode-session--target-message-ids (target)
  "Return message ids from TARGET.
TARGET may be nil, a list of ids, or a function returning a list of ids."
  (delete-dups
   (seq-filter
    (lambda (id) (and (stringp id) (not (string-empty-p id))))
    (cond
     ((functionp target) (funcall target))
     ((listp target) target)))))

(defun opencode-session--message-detail-result (result)
  "Return a persisted message from message detail RESULT, if present."
  (cond
   ((and (listp result) (alist-get 'info result)) result)
   ((and (listp result) (listp (car result)) (alist-get 'info (car result)))
    (car result))))

(defun opencode-session--run-history-callback (callback)
  "Run CALLBACK in the current session buffer when non-nil."
  (when callback
    (funcall callback)))

(defun opencode-session--sync-history-message-details (session-id reason callback ids)
  "Fetch targeted persisted IDS for SESSION-ID and reconcile them.
REASON is diagnostic text.  CALLBACK runs after targeted reconciliation."
  (ignore reason)
  (if (null ids)
      (opencode-session--run-history-callback callback)
    (let ((remaining (length ids))
          messages)
      (dolist (message-id ids)
        (opencode-api-message-details (session-id message-id) message
          (with-current-buffer (current-buffer)
            (when-let ((stored (opencode-session--message-detail-result message)))
              (push stored messages))
            (setq remaining (1- remaining))
            (when (<= remaining 0)
              (unwind-protect
                  (progn
                    (opencode-session--apply-history-messages
                     (nreverse messages) ids)
                    (opencode-session--run-history-callback callback))
                (opencode-session--history-reconcile-finish)))))))))

(defun opencode-session--sync-history-broad (session-id buffer limit)
  "Fetch a bounded persisted history tail for SESSION-ID into BUFFER."
  (let ((effective-limit (or limit opencode-session-history-sync-limit)))
    (if (and (integerp effective-limit) (> effective-limit 0))
        (opencode-api-session-messages-limited (session-id effective-limit)
            messages
          (with-current-buffer buffer
            (unwind-protect
                (opencode-session--apply-history-messages messages nil)
              (opencode-session--history-reconcile-finish))))
      (opencode-api-session-messages (session-id) messages
        (with-current-buffer buffer
          (unwind-protect
              (let ((tail (opencode-session--take-last messages limit)))
                (opencode-session--apply-history-messages tail nil))
            (opencode-session--history-reconcile-finish)))))))

(defun opencode-session--sync-history-tail (session-id reason &optional callback
                                                       target limit)
  "Fetch persisted history for SESSION-ID and apply a bounded tail.
REASON describes the trigger for diagnostics.  CALLBACK, when non-nil, runs in
the session buffer after the sync.  TARGET may be nil for broad reconciliation,
a list of message ids, or a function returning message ids in the session
  buffer.  LIMIT bounds the persisted history tail inspected."
  (ignore reason)
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((ids (and target (opencode-session--target-message-ids target))))
          (cond
           ((and target (null ids))
            (opencode-session--run-history-callback callback))
           ((opencode-session--history-reconcile-start
             (unless target callback))
            (let ((default-directory (opencode-session--directory)))
              (if ids
                  (opencode-session--sync-history-message-details
                   session-id reason callback ids)
                (opencode-session--sync-history-broad
                 session-id buffer limit))))
           ;; A broad sync already in flight will usually cover nil TARGET.  For
           ;; targeted syncs, queue a follow-up so a one-message repair is not
           ;; dropped behind an unrelated in-flight request.
           (target
            (push (lambda ()
                    (opencode-session--sync-history-tail
                     session-id reason callback target limit))
                  opencode-session--history-reconcile-callbacks))))))))

(defun opencode-session--reconcile-suspect-history (session-id &optional callback)
  "Reconcile only suspect active stream state for SESSION-ID.
When CALLBACK is non-nil, call it in the session buffer after reconciliation."
  (opencode-session--sync-history-tail
   session-id "suspect" callback
   #'opencode-session--suspect-message-ids
   opencode-session-suspect-history-limit))

(defun opencode-session--run-suspect-reconcile (session-id buffer reason callback)
  "Run suspect reconcile for SESSION-ID in BUFFER for REASON, then CALLBACK."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode-session--suspect-reconcile-timer nil)
      (opencode-session--mark-stream-suspect reason))
    (opencode-session--reconcile-suspect-history session-id callback)))

(defun opencode-session--schedule-suspect-reconcile (session-id reason
								&optional delay
								callback)
  "Schedule targeted suspect reconciliation for SESSION-ID because of REASON."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (opencode-session--mark-stream-suspect reason)
        (when (timerp opencode-session--suspect-reconcile-timer)
          (cancel-timer opencode-session--suspect-reconcile-timer))
        (setq opencode-session--suspect-reconcile-timer
              (run-at-time (or delay opencode-session-suspect-reconcile-delay)
                           nil
                           #'opencode-session--run-suspect-reconcile
                           session-id buffer reason callback))))))

(defun opencode-session--apply-polled-status (session-id statuses)
  "Apply polled STATUSES for SESSION-ID and return the resulting status.
If SESSION-ID is absent from STATUSES, preserve an active local status because
the SSE status stream is authoritative and `/session/status' can transiently
omit sessions that are still streaming."
  (let* ((polled-status (opencode-session--status-type-for-session
                         statuses session-id))
         (current-status
          (when-let ((buffer (and session-id
                                  (gethash session-id
                                           opencode-session-buffers))))
            (when (buffer-live-p buffer)
              (buffer-local-value 'opencode-session-status buffer))))
         (status (or polled-status
                     (and (opencode-session--active-status-p current-status)
                          current-status)
                     "idle")))
    (when (or polled-status
              (not (opencode-session--active-status-p current-status)))
      (opencode-session--set-status session-id status))
    (opencode-session--maybe-schedule-question-recover session-id 0.25)
    (when (and (not polled-status)
               (opencode-session--active-status-p current-status))
      (when-let ((buffer (and session-id
                              (gethash session-id opencode-session-buffers))))
        (when (buffer-live-p buffer)
          (run-at-time 0.5 nil
                       #'opencode-session--finish-after-missing-polled-status-in-buffer
                       buffer)))
      (opencode-session--schedule-suspect-reconcile
       session-id "missing-polled-status" 0.1
       #'opencode-session--finish-after-missing-polled-status))
    (when (equal status "idle")
      (opencode-session--finalize-active-output session-id)
      (opencode-session--reconcile-pending session-id))
    (when (equal status "busy")
      (when-let ((buffer (and session-id
                              (gethash session-id opencode-session-buffers))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (or (and (not opencode-assistant-messages)
                           (not (timerp opencode--reconcile-timer)))
                      (opencode-session--active-output-quiet-p))
              (opencode-session--schedule-reconcile session-id 0.2))))))
    status))

(defun opencode-session--poll-status (session-id)
  "Poll backend status for SESSION-ID and update the session buffer."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (equal opencode-session-status "idle")
            (setq opencode-session--status-poll-timer nil
                  opencode-session--status-poll-in-flight nil)
          (unless opencode-session--status-poll-in-flight
            (setq opencode-session--status-poll-in-flight t)
            (let ((default-directory (opencode-session--directory)))
              (opencode-api-sessions-status statuses
                (opencode-session--apply-polled-status session-id statuses)
                (setq opencode-session--status-poll-in-flight nil
                      opencode-session--status-poll-timer nil)
                (when (and (buffer-live-p buffer)
                           (not (equal opencode-session-status "idle")))
                  (opencode-session--schedule-status-poll
                   session-id opencode-session-status-poll-interval))))))))))

(defun opencode-session--cancel-status-poll-timers (session-id)
  "Cancel queued fallback status poll timers for SESSION-ID."
  (dolist (timer timer-list)
    (when (and (eq (timer--function timer) #'opencode-session--poll-status)
               (equal (timer--args timer) (list session-id)))
      (cancel-timer timer))))

(defun opencode-session--schedule-status-poll (session-id &optional delay)
  "Schedule backend status polling for SESSION-ID after DELAY seconds."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (or opencode-session--status-poll-in-flight
                    (equal opencode-session-status "idle"))
          (opencode-session--cancel-status-poll-timers session-id)
          (setq opencode-session--status-poll-timer
                (run-at-time (or delay opencode-session-status-poll-initial-delay) nil
                             #'opencode-session--poll-status session-id)))))))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode-record--send-input string)
  (when (string-empty-p (string-trim string))
    (user-error "Cannot send an empty prompt"))
  (when (opencode-session--sync-pending-question)
    (user-error "Answer the pending OpenCode question first with C-c C-p"))
  (when opencode-session-pending-questions
    (user-error "Answer the pending OpenCode question first with C-c C-p"))
  (when (equal opencode-session-status "busy")
    (user-error "OpenCode session is busy"))
  (opencode--highlight-input)
  (opencode--output "\n")
  (let ((extra-parts opencode--extra-parts)
        (temp-files opencode--temp-files)
        sent-message)
    (setf opencode--extra-parts nil
          opencode--temp-files nil)
    (let ((default-directory (opencode-session--directory)))
      (let-alist (opencode--effective-session-agent)
        (opencode-session--set-status opencode-session-id "busy")
        (opencode-session--schedule-status-poll opencode-session-id)
        (cond
         ((string-prefix-p "/" string)
          (let* ((space-pos (seq-position string ?\s))
                 (command (substring string 1 space-pos))
                 (arguments (if space-pos
                                (substring string (1+ space-pos))
                              "")))
            (if (member command '("compact" "summarize"))
                (opencode-api-summarize-session (opencode-session-id)
                    `((providerID . ,.model.providerID)
                      (modelID . ,.model.modelID))
                    _response)
              (opencode-api-execute-command (opencode-session-id)
                  `((agent . ,.name)
                    (model . ,(concat .model.providerID "/" .model.modelID))
                    (command . ,command)
                    (arguments . ,arguments))
                  _response))))
         ((string-prefix-p "!" string)
          (opencode-api-execute-shell (opencode-session-id)
              `((agent . ,.name)
                (command . ,(substring string 1)))
              _response))
         (t
          (setq sent-message t)
          (opencode-api-send-message (opencode-session-id)
              `((agent . ,.name)
                ,(assoc 'model opencode-session-agent)
                ,@(when .variant
                    `((variant . ,.variant)))
                (parts . ,(nreverse
                           (cons `((type . text) (text . ,string))
                                 extra-parts))))
              _result
            (opencode--delete-temp-files temp-files))))))
    (unless sent-message
      (opencode--delete-temp-files temp-files))))

(defun opencode-session--send-synthetic-input (string)
  "Send STRING to the current opencode session.
Preserve any pending input context while sending STRING as a plain prompt."
  (let* ((process (get-buffer-process (current-buffer)))
         (pending-prompt (copy-marker (process-mark process) t))
         (original-point (copy-marker (point) t))
         (prompt-start (opencode--session-process-position))
         start
         end)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char prompt-start)
              (setq start (point))
              (insert string)
              (setq end (point))))
          (set-marker (process-mark process) end)
          (let ((comint-last-input-start start)
                (comint-last-input-end end)
                (opencode--extra-parts nil)
                (opencode--temp-files nil))
            (opencode--send-input process string)
            (opencode--maybe-insert-block-spacing)))
      (set-marker (process-mark process) pending-prompt)
      (goto-char original-point)
      (set-marker pending-prompt nil)
      (set-marker original-point nil))))

(defun opencode-session--message-updated (info)
  "Handle message.updated event with INFO."
  (let-alist info
    (pcase .role
      ("assistant"
       (let ((message-id .id)
             (session-id .sessionID)
             (finished (or .finish .error)))
         (when .time.completed
           (when-let (buffer (map-elt opencode-session-buffers session-id))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq opencode-session-tokens
                       (+ .tokens.input .tokens.output .tokens.reasoning
                          .tokens.cache.read .tokens.cache.write))
                 (force-mode-line-update)))))
         (when-let (buffer (map-elt opencode-session-buffers session-id))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (puthash message-id .role opencode-message-roles)
               (opencode-session--flush-buffered-message-parts message-id))))
         (if finished
             (progn
               (when-let (buffer (map-elt opencode-session-buffers session-id))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (opencode-session--note-message-stream-finished
                      message-id))))
               (opencode-session--reconcile-message session-id message-id)
               (opencode-session--schedule-reconcile session-id 2)
               (when-let (buffer (map-elt opencode-session-buffers session-id))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setf opencode-assistant-messages
                           (assoc-delete-all message-id
                                             opencode-assistant-messages))))))
           (when-let (buffer (map-elt opencode-session-buffers session-id))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (unless (assoc-string message-id opencode-assistant-messages)
                   (push (cons message-id nil) opencode-assistant-messages))
                 (opencode-session--schedule-reconcile session-id 2)))))))
      (_
       (let ((message-id .id)
             (session-id .sessionID))
         (when-let (buffer (map-elt opencode-session-buffers session-id))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (puthash message-id .role opencode-message-roles)
               (opencode-session--flush-buffered-message-parts message-id)
               (when (equal .role "user")
                 (opencode-session--schedule-reconcile session-id 0.2))))))))))

(defun opencode-session--render-complete-assistant-message (message)
  "Render complete assistant MESSAGE into the current session buffer."
  (let-alist (alist-get 'info message)
    (let ((message-id .id))
      (unless (gethash message-id opencode-rendered-message-ids)
        (puthash message-id "assistant" opencode-message-roles)
        (dolist (part (opencode-session--sequence-list
                       (alist-get 'parts message)))
          (let-alist part
            (cond
             ((string= .type "tool")
              (when (string= .state.status "running")
                (opencode-session--set-status opencode-session-id "busy")
                (opencode-session--schedule-status-poll opencode-session-id))
              (if (string= .tool "question")
                  (if (string= .state.status "running")
                      (opencode--replay-pending-question part)
                    (opencode-session--insert-completed-question-tool part))
                (unless (and (stringp .callID)
                             (gethash .callID opencode--tool-calls-displayed)
                             (opencode-session--tool-part-visible-p part))
                  (when (stringp .callID)
                    (puthash .callID t opencode--tool-calls-displayed))
                  (opencode--insert-tool-block .tool .state.input))
                (opencode--maybe-insert-tool-output part)))
             (.text
              (when .id
                (puthash .id .type opencode-part-type)
                (puthash .id message-id opencode-part-message)
                (puthash .id .text opencode-part-text)
                (puthash .id .text opencode-part-replay-text)
                (puthash .id (length .text) opencode-part-sent))
              (let ((text (opencode--render-markdown (string-trim .text))))
                (unless (string-empty-p text)
                  (let ((start-marker
                         (copy-marker (opencode--session-process-position)))
                        end-marker)
                    (pcase .type
                      ("text"
                       (opencode--output text)
                       (setq end-marker
                             (copy-marker
                              (opencode--session-process-position)))
                       (opencode--output "\n\n"))
                      ("reasoning"
                       (when opencode-show-reasoning
                         (opencode--insert-reasoning-block text)
                         (setq end-marker
                               (copy-marker
                                (opencode--session-process-position)))
                         (opencode--output "\n\n"))))
                    (when (and .id end-marker)
                      (puthash .id start-marker opencode-part-region-start)
                      (puthash .id end-marker opencode-part-region-end)))))))))
        (puthash message-id t opencode-rendered-message-ids)))))

(defun opencode-session--reconcile-rendered-message-tail (message)
  "Extend already-rendered MESSAGE when persisted text has a missing tail."
  (let-alist (alist-get 'info message)
    (when (and .time.completed
               (gethash .id opencode-rendered-message-ids))
      (let (anchor)
        (seq-do
         (lambda (part)
           (let-alist part
             (when (and .id
                        (member .type '("text" "reasoning"))
                        (stringp .text))
               (let ((current (gethash .id opencode-part-text))
                     (start (gethash .id opencode-part-region-start))
                     (end (gethash .id opencode-part-region-end)))
                 (if (and (stringp current)
                          (string-prefix-p current .text)
                          (markerp start)
                          (markerp end)
                          (marker-position start)
                          (marker-position end))
                     (let* ((start-pos (marker-position start))
                            (old-end (marker-position end))
                            (proc (get-buffer-process (current-buffer)))
                            (proc-at-end
                             (and proc
                                  (= (marker-position (process-mark proc))
                                     old-end)))
                            (rendered (opencode--render-markdown
                                       (string-trim .text)))
                            (current-rendered
                             (opencode--render-markdown
                              (string-trim current)))
                            ;; Some old/test buffers have insertion-type start
                            ;; markers that advanced to the end of the partial
                            ;; text.  If so, repair the actual visible prefix.
                            (repair-start
                             (if (and (= start-pos old-end)
                                      (not (string-empty-p current-rendered))
                                      (>= start-pos
                                          (+ (point-min)
                                             (length current-rendered)))
                                      (equal (buffer-substring-no-properties
                                              (- start-pos
                                                 (length current-rendered))
                                              start-pos)
                                             current-rendered))
                                 (- start-pos (length current-rendered))
                               start-pos))
                            (visible (buffer-substring-no-properties
                                      repair-start old-end))
                            (visible-matches-part
                             (or (equal visible current-rendered)
                                 (equal visible rendered)
                                 (and (not (string-empty-p visible))
                                      (string-prefix-p visible rendered)
                                      (>= (length visible)
                                          (min (length rendered) 80))))))
                       (when (and visible-matches-part
                                  (<= (length current) (length .text))
                                  (not (equal visible rendered)))
                         (save-excursion
                           (let ((inhibit-read-only t))
                             (delete-region repair-start old-end)
                             (goto-char repair-start)
                             (insert rendered)
                             (set-marker-insertion-type start nil)
                             (set-marker start repair-start)
                             (set-marker end (point))
                             (when proc-at-end
                               (set-marker (process-mark proc) (point))))))
                       (when visible-matches-part
                         (setq anchor end)
                         (puthash .id .type opencode-part-type)
                         (puthash .id .text opencode-part-text)
                         (puthash .id .text opencode-part-replay-text)
                         (puthash .id (length .text) opencode-part-sent)))
                   (when (and anchor
                              (not current)
                              (not (string-empty-p (string-trim .text))))
                     (let* ((proc (get-buffer-process (current-buffer)))
                            (proc-at-anchor
                             (and proc
                                  (= (marker-position (process-mark proc))
                                     (marker-position anchor))))
                            (start-marker (copy-marker anchor t))
                            (end-marker (copy-marker anchor t))
                            (rendered (opencode--render-markdown
                                       (string-trim .text))))
                       (save-excursion
                         (let ((inhibit-read-only t))
                           (goto-char anchor)
                           (if (equal .type "reasoning")
                               (when opencode-show-reasoning
                                 (opencode--insert-reasoning-block rendered)
                                 (opencode--output "\n\n"))
                             (insert rendered)
                             (insert "\n\n"))
                           (set-marker end-marker (point))
                           (when proc-at-anchor
                             (set-marker (process-mark proc) (point)))))
                       (setq anchor end-marker)
                       (puthash .id .type opencode-part-type)
                       (puthash .id .text opencode-part-text)
                       (puthash .id .text opencode-part-replay-text)
                       (puthash .id (length .text) opencode-part-sent)
                       (puthash .id start-marker opencode-part-region-start)
                       (puthash .id end-marker opencode-part-region-end))))))))
         (alist-get 'parts message))))))

(defun opencode-session--user-message-p (message)
  "Return non-nil when MESSAGE is a persisted user message."
  (equal "user" (map-nested-elt message '(info role))))

(defun opencode-session--message-text-parts (message)
  "Return non-empty text strings from MESSAGE."
  (delq nil
        (mapcar (lambda (part)
                  (let-alist part
                    (when (and (equal .type "text")
                               (stringp .text)
                               (not (string-empty-p (string-trim .text))))
                      .text)))
                (opencode-session--sequence-list
                 (alist-get 'parts message)))))

(defun opencode-session--message-tool-parts (message)
  "Return transcript-visible tool parts from MESSAGE."
  (delq nil
        (mapcar (lambda (part)
                  (let-alist part
                    (when (and (equal .type "tool")
                               (member .state.status
                                       '("running" "completed" "error")))
                      part)))
                (opencode-session--sequence-list
                 (alist-get 'parts message)))))

(defun opencode-session--history-validated-table ()
  "Return the current buffer's history validation table."
  (unless (hash-table-p opencode-session--history-validated-message-ids)
    (setq opencode-session--history-validated-message-ids
          (make-hash-table :test 'equal)))
  opencode-session--history-validated-message-ids)

(defun opencode-session--history-validated-p (message-id)
  "Return non-nil when MESSAGE-ID visibility has already been validated."
  (and (stringp message-id)
       (gethash message-id (opencode-session--history-validated-table))))

(defun opencode-session--mark-history-validated (message-id)
  "Remember that MESSAGE-ID is visible in the current transcript."
  (when (stringp message-id)
    (puthash message-id t (opencode-session--history-validated-table))))

(defun opencode-session--unmark-history-validated (message-id)
  "Forget visibility validation for MESSAGE-ID."
  (when (stringp message-id)
    (remhash message-id (opencode-session--history-validated-table))))

(defun opencode-session--text-visible-p (text)
  "Return non-nil when TEXT appears to be visible in the current buffer."
  (let* ((rendered (substring-no-properties
                    (opencode--render-markdown (string-trim text))))
         (len (length rendered)))
    (when (> len 0)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (<= len 240)
              (search-forward rendered nil t)
            (let ((prefix (substring rendered 0 120))
                  (suffix (substring rendered (- len 120))))
              (and (search-forward prefix nil t)
                   (progn
                     (goto-char (point-min))
                     (search-forward suffix nil t))))))))))

(defun opencode-session--tool-part-visible-p (part)
  "Return non-nil when tool PART appears to be visible in the buffer."
  (let-alist part
    (let ((text (cond
                 ((equal .tool "question")
                  (opencode-session--format-completed-question-tool part))
                 (t
                  (opencode--format-tool-call .tool .state.input)))))
      (when (and (stringp text)
                 (not (string-empty-p (string-trim text))))
        (let ((needle (substring-no-properties text)))
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (search-forward needle nil t))))))))

(defun opencode-session--message-visible-p (message)
  "Return non-nil when MESSAGE's persisted transcript is visible."
  (let ((text-parts (opencode-session--message-text-parts message))
        (tool-parts (opencode-session--message-tool-parts message)))
    (and (or (null text-parts)
             (cl-every #'opencode-session--text-visible-p text-parts))
         (or (null tool-parts)
             (cl-every #'opencode-session--tool-part-visible-p tool-parts)))))

(defun opencode-session--history-messages-by-id (messages)
  "Return a hash table mapping ids to persisted MESSAGES."
  (let ((messages-by-id (make-hash-table :test 'equal)))
    (dolist (message messages)
      (when-let ((message-id (map-nested-elt message '(info id))))
        (puthash message-id message messages-by-id)))
    messages-by-id))

(defun opencode-session--reconcile-completed-assistant-history (message)
  "Reconcile completed assistant MESSAGE against the current buffer."
  (let-alist (alist-get 'info message)
    (when (and (equal .role "assistant") .time.completed)
      (cond
       ((and (gethash .id opencode-rendered-message-ids)
             (opencode-session--history-validated-p .id))
        nil)
       ((gethash .id opencode-rendered-message-ids)
        (opencode-session--reconcile-rendered-message-tail message)
        (if (opencode-session--message-visible-p message)
            (opencode-session--mark-history-validated .id)
          (remhash .id opencode-rendered-message-ids)
          (opencode-session--unmark-history-validated .id)
          (opencode-session--render-complete-assistant-message message)
          (opencode-session--mark-history-validated .id)))
       (t
        (opencode-session--render-complete-assistant-message message)
        (opencode-session--mark-history-validated .id)))
      (setf opencode-assistant-messages
            (assoc-delete-all .id opencode-assistant-messages))
      (opencode-session--forget-message-stream-state .id))))

(defun opencode-session--apply-history-messages (messages &optional target-ids)
  "Apply persisted MESSAGES to the current session buffer.
When TARGET-IDS is nil, reconcile the whole bounded history set.  When
TARGET-IDS is non-nil, reconcile only those message ids plus any current active
messages found in the bounded message table."
  (let ((messages-by-id (opencode-session--history-messages-by-id messages)))
    (if target-ids
        (progn
          (dolist (message messages)
            (when (opencode-session--message-id-in-list-p message target-ids)
              (opencode-session--reconcile-active-provisional-message message)
              (opencode-session--reconcile-running-assistant-message message)))
          (opencode-session--reconcile-pending-messages messages-by-id)
          (dolist (message messages)
            (when (opencode-session--message-id-in-list-p message target-ids)
              (opencode-session--reconcile-completed-assistant-history message))))
      (opencode-session--reconcile-active-provisional-messages messages)
      (opencode-session--reconcile-running-assistant-messages messages)
      (opencode-session--reconcile-pending-messages messages-by-id)
      (opencode-session--reconcile-missed-history-messages messages))))

(defun opencode-session--visible-user-request-count ()
  "Return the number of visible user requests in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^> \\(.*\\)$" nil t)
        ;; Do not count the empty current input prompt as a persisted user
        ;; request.  That false positive can mark one backend user message
        ;; rendered even though it is absent from the buffer.
        (unless (string-empty-p (string-trim (match-string-no-properties 1)))
          (setq count (1+ count))))
      count)))

(defun opencode-session--reconcile-missed-history-messages (messages)
  "Render missed persisted user and completed assistant MESSAGES in order."
  (let ((visible-count (opencode-session--visible-user-request-count))
        (assistant-reconcile-enabled
         (> (hash-table-count opencode-rendered-message-ids) 0))
        (user-index 0))
    (seq-do
     (lambda (message)
       (let-alist (alist-get 'info message)
         (pcase .role
           ("user"
            (cond
             ((and (gethash .id opencode-rendered-message-ids)
                   (opencode-session--history-validated-p .id))
              (puthash .id "user" opencode-message-roles))
             ((opencode-session--message-visible-p message)
              (puthash .id t opencode-rendered-message-ids)
              (puthash .id "user" opencode-message-roles)
              (opencode-session--mark-history-validated .id))
             ((gethash .id opencode-rendered-message-ids)
              (remhash .id opencode-rendered-message-ids)
              (opencode-session--unmark-history-validated .id)
              (opencode--replay-user-request message)
              (opencode-session--mark-history-validated .id))
             ((< user-index visible-count)
              ;; Existing buffers opened before user-message tracking already
              ;; have these prompts in the comint transcript; mark them to avoid
              ;; replaying old history when reconciliation catches up.
              (puthash .id t opencode-rendered-message-ids)
              (puthash .id "user" opencode-message-roles)
              (opencode-session--mark-history-validated .id))
             (t
              (opencode--replay-user-request message)
              (opencode-session--mark-history-validated .id)))
            (setq user-index (1+ user-index)))
           ("assistant"
            (when (and assistant-reconcile-enabled .time.completed)
              (opencode-session--reconcile-completed-assistant-history
               message))))))
     messages)))

(defun opencode-session--reconcile-pending (session-id &optional callback)
  "Render stored assistant messages still pending for SESSION-ID.
When CALLBACK is non-nil, call it in the session buffer after reconciliation."
  (opencode-session--sync-history-tail session-id "pending" callback))

(defun opencode-session--finish-after-missing-polled-status ()
  "Finish the current session if reconciliation cleared active output."
  (when (and (not opencode-assistant-messages)
             (not opencode-session--stream-batch-strings))
    (unless (equal opencode-session-status "idle")
      (opencode-session--set-status opencode-session-id "idle"))
    (opencode-session--finalize-active-output opencode-session-id)
    (opencode--show-prompt)))

(defun opencode-session--finish-after-missing-polled-status-in-buffer (buffer)
  "Run missing-status finish check in BUFFER if it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (opencode-session--finish-after-missing-polled-status))))

(defun opencode-session--running-tool-message-p (message)
  "Return non-nil when MESSAGE contains a running tool part."
  (seq-some
   (lambda (part)
     (let-alist part
       (and (equal .type "tool")
            (equal .state.status "running"))))
   (alist-get 'parts message)))

(defun opencode-session--reconcile-running-assistant-message (message)
  "Render active incomplete assistant MESSAGE from stored history."
  (let-alist (alist-get 'info message)
    (when (and (equal .role "assistant")
               (not .time.completed)
               (not (gethash .id opencode-rendered-message-ids))
               (not (assoc-string .id opencode-assistant-messages))
               (opencode-session--running-tool-message-p message))
      (puthash .id "assistant" opencode-message-roles)
      (seq-do
       (lambda (part)
         (let-alist part
           (pcase .type
             ((or "text" "reasoning")
              (opencode-session--update-part part nil .type))
             ("tool"
              (opencode-session--update-part part nil "tool")))))
       (alist-get 'parts message))
      (opencode-session--set-status opencode-session-id "busy")
      (opencode-session--schedule-status-poll opencode-session-id)
      t)))

(defun opencode-session--reconcile-running-assistant-messages (messages)
  "Render running assistant MESSAGES missed by live events."
  (seq-do #'opencode-session--reconcile-running-assistant-message messages))

(defun opencode-session--part-text-from-history (part)
  "Return the best known text for PART, preserving unpersisted live tail."
  (let-alist part
    (let* ((stored (or .text ""))
           (live (and .id (gethash .id opencode-part-text))))
      (if (and (stringp live)
               (>= (length live) (length stored))
               (string-prefix-p stored live))
          live
        stored))))

(defun opencode-session--active-message-start (message fallback)
  "Return earliest rendered text/reasoning start for active MESSAGE.
Use FALLBACK when no rendered part-start marker is known."
  (let ((start fallback))
    (seq-do
     (lambda (part)
       (let-alist part
         (when (member .type '("text" "reasoning"))
           (when-let ((part-start (and .id
                                       (gethash .id
                                                opencode-part-region-start))))
             (when (or (not start)
                       (< (if (markerp part-start)
                              (marker-position part-start)
                            part-start)
                          (if (markerp start)
                              (marker-position start)
                            start)))
               (setq start part-start))))))
     (alist-get 'parts message))
    start))

(defun opencode-session--reconcile-active-provisional-message (message)
  "Hydrate active text/reasoning output for MESSAGE from stored metadata."
  (let-alist (alist-get 'info message)
    (let* ((message-id .id)
           (entry (assoc-string message-id opencode-assistant-messages))
           (active-type (and entry (cadr entry)))
           (fallback-start (and entry (cddr entry)))
           (start (and (memq active-type '(provisional text reasoning))
                       (opencode-session--active-message-start
                        message fallback-start))))
      (when (and (equal .role "assistant") start)
        (puthash message-id "assistant" opencode-message-roles)
        (let ((segments nil))
          (seq-do
           (lambda (part)
             (let-alist part
               (when (member .type '("text" "reasoning"))
                 (let ((text (opencode-session--part-text-from-history part)))
                   (when .id
                     (puthash .id .type opencode-part-type)
                     (puthash .id message-id opencode-part-message)
                     (puthash .id text opencode-part-text)
                     (puthash .id (length text) opencode-part-sent))
                   (when (and (stringp text)
                              (not (string-empty-p text))
                              (or (string= .type "text")
                                  opencode-show-reasoning))
                     (push (list .type text .id) segments))))))
           (alist-get 'parts message))
          (let* ((ordered-segments (nreverse segments))
                 (rendered-segments
                  (mapcar (lambda (segment)
                            (opencode--render-markdown (nth 1 segment)))
                          ordered-segments))
                 (visible (buffer-substring-no-properties start (point-max)))
                 (already-complete
                  (and .time.completed
                       (or (equal visible
                                  (mapconcat #'identity rendered-segments ""))
                           (equal visible
                                  (mapconcat #'identity rendered-segments
                                             "\n\n"))))))
            (if already-complete
                (progn
                  (puthash message-id t opencode-rendered-message-ids)
                  (setf opencode-assistant-messages
                        (assoc-delete-all message-id
                                          opencode-assistant-messages)))
              (let ((inhibit-read-only t))
                (delete-region start (point-max)))
              (setf (cdr entry) nil)
              (let ((index 0)
                    last-type last-start)
                (dolist (segment ordered-segments)
                  (pcase-let ((`(,type ,text ,part-id) segment))
                    (when (> index 0)
                      (opencode--maybe-insert-block-spacing))
                    (let ((segment-start (copy-marker (point-max))))
                      (pcase type
                        ("text"
                         (opencode--output (opencode--render-markdown text))
                         (setq last-type 'text
                               last-start segment-start))
                        ("reasoning"
                         (opencode--insert-reasoning-block
                          (opencode--render-markdown text))
                         (setq last-type 'reasoning
                               last-start segment-start)))
                      (when part-id
                        (puthash part-id segment-start
                                 opencode-part-region-start)
                        (puthash part-id (copy-marker (point-max) t)
                                 opencode-part-region-end)))
                    (setq index (1+ index))))
                (when last-type
                  (setf (cdr entry) (cons last-type last-start))))
              (when .time.completed
                (opencode--maybe-insert-block-spacing)
                (puthash message-id t opencode-rendered-message-ids)
                (setf opencode-assistant-messages
                      (assoc-delete-all message-id
                                        opencode-assistant-messages)))))
          t)))))

(defun opencode-session--reconcile-active-provisional-messages (messages)
  "Hydrate active assistant output from stored MESSAGES."
  (seq-do #'opencode-session--reconcile-active-provisional-message messages))

(defun opencode-session--reconcile-pending-messages (messages-by-id)
  "Render pending assistant messages from MESSAGES-BY-ID in current buffer."
  (dolist (entry (copy-sequence opencode-assistant-messages))
    (let* ((message-id (car entry))
           (message (gethash message-id messages-by-id)))
      (when (and message
                 (not (gethash message-id opencode-rendered-message-ids))
                 (or (null (cdr entry))
                     (map-nested-elt message '(info time completed))))
        (opencode-session--render-complete-assistant-message message)
        (setf opencode-assistant-messages
              (assoc-delete-all message-id opencode-assistant-messages))))))

(defun opencode-session--reconcile-missed-completed-assistants (messages)
  "Render completed assistant MESSAGES that were missed by live events.
This is intentionally gated on existing rendered state to avoid duplicating
legacy buffers that predate `opencode-rendered-message-ids'."
  (when (> (hash-table-count opencode-rendered-message-ids) 0)
    (seq-do
     (lambda (message)
       (let-alist (alist-get 'info message)
         (when (and (string= .role "assistant")
                    .time.completed
                    (not (gethash .id opencode-rendered-message-ids)))
           (opencode-session--render-complete-assistant-message message)
           (setf opencode-assistant-messages
                 (assoc-delete-all .id opencode-assistant-messages)))))
     messages)))

(defun opencode-session--schedule-reconcile (session-id &optional delay)
  "Schedule reconciliation for SESSION-ID after DELAY seconds."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp opencode--reconcile-timer)
          (cancel-timer opencode--reconcile-timer))
        (setq opencode--reconcile-timer
              (run-at-time (or delay 2) nil
                           (lambda (session-id buffer)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (setq opencode--reconcile-timer nil))
                               (opencode-session--reconcile-pending session-id)))
                           session-id buffer))))))

(defun opencode-session--reconcile-message (session-id message-id)
  "Render stored assistant MESSAGE-ID for SESSION-ID if live streaming missed it."
  (when message-id
    (opencode-session--sync-history-tail
     session-id "message-updated" nil (list message-id))))

(defun opencode-session--part-id (part)
  "Return the id for streaming PART."
  (let-alist part
    (or .id .partID)))

(defun opencode-session--role-ready-p (part)
  "Return non-nil if text PART can be rendered in this buffer."
  (let-alist part
    (let* ((part-id (opencode-session--part-id part))
           (message-id (or .messageID
                           (and part-id
                                (gethash part-id opencode-part-message))))
           (role (and message-id
                      (gethash message-id opencode-message-roles))))
      (or (not message-id) (equal role "assistant")))))

(defun opencode-session--drop-part (part-id)
  "Forget buffered state for PART-ID."
  (remhash part-id opencode-part-type)
  (remhash part-id opencode-part-text)
  (remhash part-id opencode-part-sent)
  (remhash part-id opencode-part-replay-text)
  (remhash part-id opencode-part-region-start)
  (remhash part-id opencode-part-region-end)
  (remhash part-id opencode-part-message))

(defun opencode-session--flush-untyped-delta-parts-as-text ()
  "Render buffered delta-only parts that never received type metadata."
  (maphash
   (lambda (part-id message-id)
     (let ((text (gethash part-id opencode-part-text))
           (sent (gethash part-id opencode-part-sent 0))
           (entry (assoc-string message-id opencode-assistant-messages)))
       (when (and (not (gethash part-id opencode-part-type))
                  (stringp text)
                  (or (< sent (length text))
                      (eq (cadr entry) 'provisional)))
         (puthash part-id "text" opencode-part-type)
         (unless (gethash message-id opencode-message-roles)
           (puthash message-id "assistant" opencode-message-roles))
         (opencode-session--update-part
          `((sessionID . ,opencode-session-id)
            (id . ,part-id)
            (messageID . ,message-id)
            (type . "text"))
          nil "text"))))
   opencode-part-message))

(defun opencode-session--part-delta (part delta)
  "Return only the not-yet-rendered text for PART and DELTA.
The server may send either incremental `message.part.delta' events or full
part snapshots on `message.part.updated'.  Track accumulated and rendered text
separately so out-of-order deltas are buffered until their type is known."
  (let-alist part
    (let* ((part-id (opencode-session--part-id part))
           (previous (and part-id (gethash part-id opencode-part-text "")))
           (replay-text (and part-id
                             opencode-session--draining-queued-events
                             (gethash part-id opencode-part-replay-text)))
           (delta-covered-by-replay
            (and replay-text
                 (stringp delta)
                 (not (string-empty-p delta))
                 (string-match-p (regexp-quote delta) replay-text)))
           (next (cond
                  ((stringp .text)
                   (remhash part-id opencode-part-replay-text)
                   (if (and previous (< (length .text) (length previous)))
                       previous
                     .text))
                  (delta-covered-by-replay previous)
                  ((and previous (stringp delta)) (concat previous delta))
                  ((stringp delta) delta)
                  (previous)))
           (sent (and part-id (gethash part-id opencode-part-sent 0))))
      (when (and part-id (stringp delta) (not delta-covered-by-replay))
        (remhash part-id opencode-part-replay-text))
      (when (and part-id next)
        (puthash part-id next opencode-part-text))
      (cond
       (delta-covered-by-replay nil)
       ((and part-id next sent (> (length next) sent))
        (substring next sent))
       ((and (not part-id) (stringp delta)) delta)))))

(defun opencode-session--mark-part-rendered (part)
  "Mark all accumulated text for PART as rendered."
  (when-let* ((part-id (opencode-session--part-id part))
              (text (gethash part-id opencode-part-text)))
    (puthash part-id (length text) opencode-part-sent)))

(defun opencode-session--flush-buffered-message-parts (message-id)
  "Flush buffered parts after learning the role for MESSAGE-ID."
  (let ((role (gethash message-id opencode-message-roles)))
    (maphash
     (lambda (part-id part-message-id)
       (when (equal part-message-id message-id)
         (if (equal role "assistant")
             (when-let ((type (gethash part-id opencode-part-type)))
               (opencode-session--update-part
                `((sessionID . ,opencode-session-id)
                  (id . ,part-id)
                  (messageID . ,message-id)
                  (type . ,type))
                nil type))
           (when-let ((entry (assoc-string message-id
                                           opencode-assistant-messages)))
             (when (and (eq (cadr entry) 'provisional)
                        (cddr entry))
               (let ((inhibit-read-only t))
                 (delete-region (cddr entry)
                                (opencode--session-process-position))))
             (setf opencode-assistant-messages
                   (assoc-delete-all message-id
                                     opencode-assistant-messages)))
           (opencode-session--drop-part part-id))))
     opencode-part-message)))

(defun opencode-session--finalize-active-output (session-id)
  "Finalize active streamed output for SESSION-ID when no finish part arrived."
  (when-let ((buffer (and session-id
                          (gethash session-id opencode-session-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (opencode-session--flush-untyped-delta-parts-as-text)
        (dolist (entry (copy-sequence opencode-assistant-messages))
          (let ((message-id (car entry))
                (last-type (cadr entry))
                (last-start (cddr entry)))
            (when (and last-type last-start)
              (opencode-session--render-final-region last-type last-start)
              (opencode--maybe-insert-block-spacing)
              (puthash message-id t opencode-rendered-message-ids)
              (setf opencode-assistant-messages
                    (assoc-delete-all message-id
                                      opencode-assistant-messages)))))))))

(defun opencode--render-region (type start &optional end)
  "Render TYPE markdown from START to END.
END defaults to the process mark."
  (setq end (or end (opencode--session-process-position)))
  (when (and (seq-contains-p '(reasoning text) type)
             (< start end))
    (let ((inhibit-read-only t)
          (text (buffer-substring-no-properties start end)))
      (ignore-errors
        (setf text (opencode--render-markdown text)))
      (delete-region start end)
      (cl-case type
        (reasoning (opencode--insert-reasoning-block text))
        (text (opencode--output text))))))

(defun opencode-session--cancel-live-render-timer ()
  "Cancel any pending live markdown render timer for this buffer."
  (when (timerp opencode-session--live-render-timer)
    (cancel-timer opencode-session--live-render-timer))
  (setq opencode-session--live-render-timer nil
        opencode-session--live-render-type nil
        opencode-session--live-render-start nil))

(defun opencode-session--cancel-suspect-reconcile-timer ()
  "Cancel any pending suspect reconciliation timer for this buffer."
  (when (timerp opencode-session--suspect-reconcile-timer)
    (cancel-timer opencode-session--suspect-reconcile-timer))
  (setq opencode-session--suspect-reconcile-timer nil))

(defun opencode-session--cancel-stream-quiet-timer ()
  "Cancel any pending quiet-stream reconciliation timer for this buffer."
  (when (timerp opencode-session--stream-quiet-timer)
    (cancel-timer opencode-session--stream-quiet-timer))
  (setq opencode-session--stream-quiet-timer nil))

(defun opencode-session--run-stream-quiet-reconcile (session-id buffer message-id)
  "Reconcile SESSION-ID in BUFFER after MESSAGE-ID's stream goes quiet."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode-session--stream-quiet-timer nil)
      (when (and (or (opencode-session--active-status-p opencode-session-status)
                     opencode-assistant-messages)
                 (or opencode-assistant-messages
                     (gethash message-id opencode-session--stream-message-states)))
        (opencode-session--mark-stream-suspect "stream-quiet" message-id)
        (opencode-session--reconcile-suspect-history
         session-id #'opencode-session--finish-after-missing-polled-status)))))

(defun opencode-session--schedule-stream-quiet-reconcile (session-id message-id)
  "Schedule quiet-stream reconciliation for SESSION-ID and MESSAGE-ID."
  (when (and session-id message-id
             (not (bound-and-true-p opencode-record-replay-buffer))
             (numberp opencode-session-stream-quiet-reconcile-delay)
             (> opencode-session-stream-quiet-reconcile-delay 0))
    (when-let ((buffer (gethash session-id opencode-session-buffers)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (timerp opencode-session--stream-quiet-timer)
            (cancel-timer opencode-session--stream-quiet-timer))
          (setq opencode-session--stream-quiet-timer
                (run-at-time opencode-session-stream-quiet-reconcile-delay
                             nil
                             #'opencode-session--run-stream-quiet-reconcile
                             session-id buffer message-id)))))))

(defun opencode-session--buffer-visible-p ()
  "Return non-nil when the current session buffer is visible."
  (get-buffer-window (current-buffer) t))

(defun opencode-session--live-render-enabled-p ()
  "Return non-nil when this buffer should render markdown while streaming."
  (and (numberp opencode-session-live-markdown-delay)
       (>= opencode-session-live-markdown-delay 0)
       (or (not opencode-session-live-markdown-visible-only)
           (opencode-session--buffer-visible-p))))

(defun opencode-session--run-live-render (buffer)
  "Render pending live markdown for BUFFER if it is still visible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (opencode-session--flush-stream-batch)
      (let ((type opencode-session--live-render-type)
            (start opencode-session--live-render-start))
        (setq opencode-session--live-render-timer nil
              opencode-session--live-render-type nil
              opencode-session--live-render-start nil)
        (when (and type start
                   (opencode-session--live-render-enabled-p))
          (opencode--render-region type start))))))

(defun opencode-session--schedule-live-render (type start)
  "Schedule a coalesced live markdown render of TYPE starting at START."
  (when (and (memq type '(reasoning text))
             start
             (opencode-session--live-render-enabled-p))
    (setq opencode-session--live-render-type type
          opencode-session--live-render-start start)
    (unless (timerp opencode-session--live-render-timer)
      (setq opencode-session--live-render-timer
            (run-at-time opencode-session-live-markdown-delay nil
                         #'opencode-session--run-live-render
                         (current-buffer))))))

(defun opencode-session--cancel-stream-batch-timer ()
  "Cancel any pending streaming output batch timer for this buffer."
  (when (timerp opencode-session--stream-batch-timer)
    (cancel-timer opencode-session--stream-batch-timer))
  (setq opencode-session--stream-batch-timer nil))

(defun opencode-session--cancel-idle-finalize-timer ()
  "Cancel deferred idle finalization for this buffer."
  (when (timerp opencode-session--idle-finalize-timer)
    (cancel-timer opencode-session--idle-finalize-timer))
  (setq opencode-session--idle-finalize-timer nil
        opencode-session--idle-finalize-callback nil))

(defun opencode-session--active-output-p ()
  "Return non-nil when this session has unfinalized output."
  (or opencode-assistant-messages
      opencode-session--stream-batch-strings))

(defun opencode-session--run-idle-finalize (buffer)
  "Finalize deferred idle output for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode-session--idle-finalize-timer nil)
      (let ((callback opencode-session--idle-finalize-callback))
        (setq opencode-session--idle-finalize-callback nil)
        (opencode-session--finalize-active-output opencode-session-id)
        (when callback
          (funcall callback))))))

(defun opencode-session--finish-idle (session-id &optional callback)
  "Finalize idle SESSION-ID output, then run CALLBACK.
When active output remains, defer finalization briefly so late deltas from the
same stream can arrive before the prompt is shown."
  (if-let ((buffer (and session-id
                        (gethash session-id opencode-session-buffers))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and (opencode-session--active-output-p)
                   (numberp opencode-session-idle-finalize-delay)
                   (> opencode-session-idle-finalize-delay 0))
              (progn
                (opencode-session--schedule-suspect-reconcile
                 session-id "idle-with-active-output"
                 opencode-session-idle-finalize-delay)
                (setq opencode-session--idle-finalize-callback callback)
                (unless (timerp opencode-session--idle-finalize-timer)
                  (setq opencode-session--idle-finalize-timer
                        (run-at-time opencode-session-idle-finalize-delay nil
                                     #'opencode-session--run-idle-finalize
                                     buffer))))
            (opencode-session--finalize-active-output session-id)
            (when callback
              (funcall callback)))))
    (when callback
      (funcall callback))))

(defun opencode-session--note-stream-event ()
  "Cancel deferred idle finalization because stream output is still arriving."
  (when (timerp opencode-session--idle-finalize-timer)
    (opencode-session--cancel-idle-finalize-timer))
  (when (and opencode-session-id
             (equal opencode-session-status "idle"))
    (opencode-session--set-status opencode-session-id "busy")
    (opencode-session--schedule-status-poll opencode-session-id)))

(defun opencode-session--stream-batch-enabled-p ()
  "Return non-nil when this buffer should batch streaming output."
  (and (derived-mode-p 'opencode-session-mode)
       (numberp opencode-session-stream-batch-delay)
       (> opencode-session-stream-batch-delay 0)))

(defun opencode-session--flush-stream-batch (&optional buffer)
  "Flush pending streaming output for BUFFER or the current buffer."
  (if buffer
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--flush-stream-batch)))
    (let ((strings opencode-session--stream-batch-strings)
          (type opencode-session--stream-batch-type)
          (start opencode-session--stream-batch-start))
      (opencode-session--cancel-stream-batch-timer)
      (setq opencode-session--stream-batch-type nil
            opencode-session--stream-batch-start nil
            opencode-session--stream-batch-strings nil)
      (when strings
        (opencode--output (mapconcat #'identity (nreverse strings) ""))
        (when (eq type 'reasoning)
          (opencode-session--add-live-reasoning-margin start))))))

(defun opencode-session--queue-stream-output (type start string)
  "Queue STRING as streaming TYPE output beginning at START."
  (when (and (stringp string) (> (length string) 0))
    (if (opencode-session--stream-batch-enabled-p)
        (progn
          (when (and opencode-session--stream-batch-strings
                     (not (and (eq type opencode-session--stream-batch-type)
                               (eq start opencode-session--stream-batch-start))))
            (opencode-session--flush-stream-batch))
          (setq opencode-session--stream-batch-type type
                opencode-session--stream-batch-start start)
          (push string opencode-session--stream-batch-strings)
          (unless (timerp opencode-session--stream-batch-timer)
            (setq opencode-session--stream-batch-timer
                  (run-at-time opencode-session-stream-batch-delay nil
                               #'opencode-session--flush-stream-batch
                               (current-buffer)))))
      (opencode--output string)
      (when (eq type 'reasoning)
        (opencode-session--add-live-reasoning-margin start)))))

(defun opencode-session--add-live-reasoning-margin (start)
  "Add a reasoning margin from START to the current process position."
  (let ((end (opencode--session-process-position)))
    (when (and start (< start end))
      (opencode--add-margin start end 'opencode-reasoning-margin-highlight))))

(defun opencode-session--render-final-region (type start &optional end)
  "Cancel live rendering and render final markdown for TYPE from START to END."
  (opencode-session--flush-stream-batch)
  (opencode-session--cancel-live-render-timer)
  (opencode--render-region type start end))

(defun opencode--stream-line-start ()
  "Return the current line start at process mark."
  (save-excursion
    (goto-char (opencode--session-process-position))
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (opencode-session--flush-stream-batch)
  (let ((pos (opencode--session-process-position)))
    (opencode--output
     (cond
      ((not (eq ?\n (char-before pos))) "\n\n")
      ((and (eq ?\n (char-before pos))
            (not (eq ?\n (char-before (1- pos)))))
       "\n")
      (t "")))))

(defun opencode--maybe-insert-prompt-spacing ()
  "Ensure a prompt starts on its own block when output already exists."
  (opencode-session--flush-stream-batch)
  (let ((pos (opencode--session-process-position)))
    (when (> pos (point-min))
      (opencode--output
       (cond
        ((not (eq ?\n (char-before pos))) "\n\n")
        ((and (eq ?\n (char-before pos))
              (not (eq ?\n (char-before (1- pos)))))
         "\n")
        (t ""))))))

(defun opencode--output (string)
  "Output STRING as comint output."
  (let ((process (get-buffer-process (current-buffer)))
        (string (opencode--buttonize-file-references string)))
    (if (and process (markerp (process-mark process)))
        (condition-case nil
            (comint-output-filter process string)
          (error
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert string))))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert string)))))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█░░█ █░░█ █▀▀▀ █░░█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█░░░ █░░█ █░░█ █▀▀▀" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for line in logo-left
             for index from 0
             do
             (opencode--output (propertize line 'face 'shadow))
             (opencode--output " ")
             (opencode--output (propertize (nth index logo-right) 'face 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Highlight the prompt after displaying output."
  (unless (let ((pos (opencode--session-process-position)))
            (and (>= pos (+ (point-min) 2))
                 (string= (buffer-substring-no-properties (- pos 2) pos)
                          "> ")))
    (opencode--maybe-insert-prompt-spacing)
    (opencode--output (propertize "> " 'invisible t))
    (when (and (consp comint-last-prompt)
               (number-or-marker-p (car comint-last-prompt))
               (number-or-marker-p (cdr comint-last-prompt))
               (< (car comint-last-prompt) (cdr comint-last-prompt)))
      (opencode--add-margin (car comint-last-prompt)
                            (cdr comint-last-prompt)
                            'opencode-request-margin-highlight)))
  (goto-char (point-max)))

(defun opencode-session--update-part (part delta type)
  "Display PART, partial message output. DELTA is new text since last update.
TYPE is text|reasoning|tool|step-finish"
  (let-alist part
    (when-let ((buffer (gethash .sessionID opencode-session-buffers))
               (_ (buffer-live-p buffer)))
      (with-current-buffer buffer
        (setq type (or type (gethash (opencode-session--part-id part)
                                     opencode-part-type)))
        (unless (and .messageID
                     (gethash .messageID
                              opencode-session--interrupted-message-ids))
          (when .messageID
            (opencode-session--note-stream-event))
          (when-let* ((part-id (opencode-session--part-id part))
                      (_ type))
            (puthash part-id type opencode-part-type))
          (when-let* ((part-id (opencode-session--part-id part))
                      (message-id .messageID))
            (puthash part-id message-id opencode-part-message))
          (when-let* ((message-id .messageID)
                      (_ (equal type "reasoning")))
            ;; Reasoning parts only come from assistant messages.  Some server
            ;; versions send reasoning deltas before the part seed, and may never
            ;; send a separate message role event.
            (unless (gethash message-id opencode-message-roles)
              (puthash message-id "assistant" opencode-message-roles)))
          (when-let* ((message-id .messageID)
                      (_ (member type '("text" "reasoning"))))
            (unless (gethash message-id opencode-message-roles)
              (opencode-session--schedule-reconcile .sessionID 1)))
          (setq delta (opencode-session--part-delta part delta))
          (when .messageID
            (opencode-session--note-message-stream-event
             .messageID (opencode-session--part-id part) delta type))
          (if .time.end
              (opencode-session--drop-part (opencode-session--part-id part))
            ;; only will follow up with message.part.delta updates when it has "" as text
            (when (and (stringp .text) (string-empty-p .text))
              (puthash .id type opencode-part-type)))
          (when-let* ((part-id (opencode-session--part-id part))
                      (message-id (gethash part-id opencode-part-message))
                      (role (gethash message-id opencode-message-roles)))
            (when (and (member type '("text" "reasoning"))
                       (not (equal role "assistant")))
              (opencode-session--drop-part part-id)
              (setq delta nil
                    type nil)))
          (let* ((message-parts
                  (or (assoc-string .messageID opencode-assistant-messages)
                      (when (and .messageID (not (equal type "step-finish")))
			(let ((entry (cons .messageID nil)))
                          (push entry opencode-assistant-messages)
                          entry))))
		 (last-type (cadr message-parts))
		 (last-start (cddr message-parts)))
            (cl-flet ((maybe-render-last-and-update-message-parts
			(new-type)
			(unless (eq new-type last-type)
			  (when last-start
                            (opencode-session--render-final-region last-type last-start)
                            (opencode--maybe-insert-block-spacing))
			  (setf (cdr message-parts)
				(cons new-type
                                      (opencode--session-process-position)))))
                      (reclassify-provisional-region
			(new-type)
			(when (and (eq last-type 'provisional) last-start)
                          (let* ((part-id (opencode-session--part-id part))
				 (part-start (and part-id
                                                  (gethash part-id
                                                           opencode-part-region-start)))
				 (part-end (and part-id
						(gethash part-id
							 opencode-part-region-end)))
				 (start (or part-start last-start))
				 (end (or part-end (point-max)))
				 (end-at-point-max
                                  (= (if (markerp end) (marker-position end) end)
                                     (point-max))))
                            (pcase new-type
                              ('reasoning
                               (if opencode-show-reasoning
                                   (if end-at-point-max
                                       (progn
                                         (opencode-session--render-final-region
                                          'reasoning start end)
					 (setf (cdr message-parts)
                                               (cons 'reasoning start)))
                                     (opencode--add-margin
                                      start end
                                      'opencode-reasoning-margin-highlight)
                                     (setf (cdr message-parts)
                                           (cons 'reasoning start)))
				 (let ((inhibit-read-only t))
                                   (delete-region start end))
				 (setf (cdr message-parts) nil)))
                              ('text
                               (when end-at-point-max
                                 (opencode-session--render-final-region
                                  'text start end))
                               (setf (cdr message-parts)
                                     (cons 'text start)))))
                          (opencode-session--mark-part-rendered part)
                          (setq last-type (cadr message-parts)
				last-start (cddr message-parts)))))
              (when (and (eq last-type 'provisional)
			 (member type '("text" "reasoning"))
			 (opencode-session--role-ready-p part))
		(reclassify-provisional-region (intern type)))
              ;; don't display margins on extra whitespace
              (when (and delta
			 (member type '("text" "reasoning"))
			 (= 0 (gethash (opencode-session--part-id part)
                                       opencode-part-sent 0))
			 (string-match-p "\\`[[:space:]]*\\'" delta))
		(setq delta nil))
              (pcase type
		(`nil
                 (when (and delta
                            (equal .field "text")
                            .messageID)
                   (unless (gethash .messageID opencode-message-roles)
                     (puthash .messageID "assistant" opencode-message-roles))
                   (unless (and (= 0 (gethash (opencode-session--part-id part)
                                              opencode-part-sent 0))
				(string-match-p "\\`[[:space:]]*\\'" delta))
                     (maybe-render-last-and-update-message-parts 'provisional)
                     (when-let ((part-id (opencode-session--part-id part)))
                       (unless (gethash part-id opencode-part-region-start)
                         (puthash part-id (copy-marker (point-max))
                                  opencode-part-region-start)))
                     (opencode--output delta)
                     (when-let ((part-id (opencode-session--part-id part)))
                       (puthash part-id (copy-marker (point-max) t)
				opencode-part-region-end))
                     (unless (timerp opencode--reconcile-timer)
                       (opencode-session--schedule-reconcile .sessionID 1))
                     (opencode-session--mark-part-rendered part))))
		((and "reasoning" (guard delta)
                      (guard (not opencode-show-reasoning))
                      (guard (opencode-session--role-ready-p part)))
		 (opencode-session--mark-part-rendered part))
		((and "reasoning" (guard delta)
                      (guard opencode-show-reasoning)
                      (guard (opencode-session--role-ready-p part)))
                 (maybe-render-last-and-update-message-parts 'reasoning)
                 (setq last-type (cadr message-parts)
                       last-start (cddr message-parts))
                 (when-let ((part-id (opencode-session--part-id part)))
                   (unless (gethash part-id opencode-part-region-start)
                     (puthash part-id last-start opencode-part-region-start)))
                 (opencode-session--queue-stream-output 'reasoning last-start delta)
                 (opencode-session--schedule-live-render 'reasoning last-start)
                 (opencode-session--mark-part-rendered part))
		((and "text" (guard delta)
                      (guard (opencode-session--role-ready-p part)))
		 (when (= 0 (gethash (opencode-session--part-id part)
                                     opencode-part-sent 0))
                   (setq delta (opencode-session--strip-shell-echo
				.messageID delta)))
                 (maybe-render-last-and-update-message-parts 'text)
                 (setq last-type (cadr message-parts)
                       last-start (cddr message-parts))
                 (when-let ((part-id (opencode-session--part-id part)))
                   (unless (gethash part-id opencode-part-region-start)
                     (puthash part-id last-start opencode-part-region-start)))
                 (opencode-session--queue-stream-output 'text last-start delta)
                 (opencode-session--schedule-live-render 'text last-start)
                 (opencode-session--mark-part-rendered part))
		("tool"
		 (maybe-render-last-and-update-message-parts 'tool)
		 (when (and (string= .tool "bash")
                            (stringp .callID)
                            (not (gethash .callID opencode-shell-calls)))
                   (puthash .callID '(:source legacy-tool) opencode-shell-calls))
		 (when (and (string= .tool "question")
                            (string= .state.status "running")
                            (not opencode-session-pending-questions))
                   (opencode-session--schedule-question-recover .sessionID))
		 (opencode-session--clear-question-for-tool part)
		 (when (fboundp 'opencode-session--refresh-permission-for-tool)
                   (opencode-session--refresh-permission-for-tool part))
		 (unless (and (string= .tool "bash")
                              (eq (opencode-session--shell-call-source .callID)
                                  'shell-event))
                   (opencode-session--stash-shell-echo part)
                   (when (and
                          ;; render once even if the running update was missed
                          (member .state.status '("running" "completed" "error"))
                          ;; avoid duplicate display
                          (not (and (gethash .callID opencode--tool-calls-displayed)
                                    (opencode-session--tool-part-visible-p part)))
                          ;; skip live questions, handled by question.asked event
                          (not (string= .tool "question")))
                     (puthash .callID t opencode--tool-calls-displayed)
                     (opencode--insert-tool-block .tool .state.input))
                   (when (and (string= .tool "question")
                              (member .state.status '("completed" "error"))
                              (stringp .callID)
                              (not (gethash .callID opencode--tool-calls-displayed)))
                     (puthash .callID t opencode--tool-calls-displayed)
                     (opencode-session--insert-completed-question-tool part))
                   (opencode--maybe-insert-tool-output part)))
		("step-finish"
		 (when (string= "stop" .reason)
                   (opencode-session--note-message-stream-finished .messageID)
                   (opencode-session--render-final-region last-type last-start)
                   (opencode--maybe-insert-block-spacing)
                   (puthash .messageID t opencode-rendered-message-ids)
                   (setf opencode-assistant-messages
                         (assoc-delete-all .messageID
                                           opencode-assistant-messages))))))))))))

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)

(defun opencode--margin (face)
  "Return margin string for FACE."
  (propertize ">" 'display
              `((margin left-margin)
                ,(propertize "▎" 'face
                             face))))

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  (let ((ov (make-overlay start (1- end)))
        (margin (opencode--margin face)))
    (overlay-put ov 'line-prefix margin)
    (overlay-put ov 'wrap-prefix margin)))

(defun opencode--render-input-markdown (input)
  "Rerender comint INPUT as markdown."
  (let ((inhibit-read-only t))
    (delete-region (opencode--session-process-position)
                   (point))
    (insert (opencode--render-markdown input))))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--show-prompt)
  (let-alist message
    (when .info.id
      (puthash .info.id "user" opencode-message-roles)
      (puthash .info.id t opencode-rendered-message-ids))
    (let ((request-start (opencode--session-process-position)))
      (seq-do (lambda (part)
                (let-alist part
                  (unless .synthetic
                    (pcase .type
                      ("text" (insert .text))))))
              .parts)
      (insert "\n")
      (if (get-buffer-process (current-buffer))
          (let ((comint-input-sender #'opencode--highlight-input))
            (comint-send-input))
        (opencode--add-margin request-start (point)
                              'opencode-request-margin-highlight)))
    (let ((agent (seq-find (lambda (agent)
                             (string= (alist-get 'name agent)
                                      .info.agent))
                           opencode-session-agents)))
      (setq opencode-session-agent agent)
      (setf (alist-get 'model opencode-session-agent) .info.model)
      (setf (alist-get 'variant opencode-session-agent) .info.model.variant))))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (save-excursion
                                        (goto-char (opencode--session-process-position))
                                        (skip-chars-backward "\r\n[:blank:]")
                                        (point))
                            face))))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--refine-diff-hunks (start)
  "Refine all diff hunks between START and process marker."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (opencode--session-process-position))
      (condition-case nil
          (while (>= (point) start)
            (diff-refine-hunk)
            (diff-hunk-prev)
            ;; Hide the diff hunk headers
            (add-text-properties (line-beginning-position)
                                 (min (point-max)
                                      (1+ (line-end-position)))
                                 '(invisible t)))
        (error nil)))))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (let ((process (get-buffer-process (current-buffer))))
    (if (and process (markerp (process-mark process)))
        (marker-position (process-mark process))
      (point-max))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    (opencode--maybe-insert-block-spacing)
    ;; For diff-like tools, apply diff hunk refinement after insertion.
    (when (member tool '("edit" "apply_patch"))
      (opencode--refine-diff-hunks start))))

(defun opencode--replay-pending-question (part)
  "Replay pending question tool PART after reconnecting to a session."
  (let-alist part
    (when (and (string= .tool "question")
               (string= .state.status "running")
               .state.input.questions)
      (let* ((questions (opencode-session--normalize-questions
                         .state.input.questions))
             (question-id (or .requestID .questionID
                              .state.requestID .state.questionID)))
        (when (and (stringp question-id)
                   (string-prefix-p "que" question-id))
          (opencode--queue-questions (current-buffer) question-id questions))))))

(defun opencode--maybe-insert-tool-output (message)
  "Maybe insert the output from tool call in MESSAGE."
  (let-alist message
    (when (and (string= .state.status "completed")
               (or opencode-show-tool-output
                   ;; show output for user run shell commands
                   (and (string= .tool "bash")
                        (not .state.input.description)))
               .state.output)
      (opencode--output .state.output)
      (opencode--output "\n"))))

(defun opencode-session--flush-interrupted ()
  "Finalize active assistant output as interrupted once."
  (when opencode-assistant-messages
    (let ((did-mark nil))
      (dolist (entry opencode-assistant-messages)
        (let ((message-id (car entry))
              (last-type (cadr entry))
              (last-start (cddr entry)))
          (unless (and (stringp message-id)
                       (gethash message-id opencode-session--interrupted-message-ids))
            (when last-start
              (opencode-session--render-final-region last-type last-start))
            (when (stringp message-id)
              (puthash message-id t opencode-session--interrupted-message-ids))
            (setq did-mark t))))
      (when did-mark
        (opencode--maybe-insert-block-spacing)
        (opencode--output (propertize "[interrupted]" 'face 'warning))
        (opencode--output "\n\n"))
      (setq opencode-assistant-messages nil))))

(defun opencode-session--directory-matches-p (directory)
  "Return non-nil if current session buffer belongs to DIRECTORY.
Nil DIRECTORY matches all session buffers."
  (or (not directory)
      (equal (opencode--normalize-directory default-directory)
             (opencode--normalize-directory directory))))

(defun opencode-session--mark-stream-failed (&optional message directory)
  "Mark active session buffers as failed with MESSAGE.
When DIRECTORY is non-nil, only buffers in that project directory are affected."
  (maphash
   (lambda (session-id buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (and (opencode-session--directory-matches-p directory)
                    (or opencode-assistant-messages
                        (equal opencode-session-status "busy")))
           (opencode-session--flush-interrupted)
           (opencode-session--set-status session-id "idle")
           (when (and (stringp message)
                      (not (string-empty-p message)))
             (opencode--output (propertize message 'face 'error))
             (opencode--output "\n\n"))
           (opencode--show-prompt)))))
   opencode-session-buffers))

(defun opencode-open-session-same-window (session)
  "Open SESSION using the current window."
  (opencode-open-session session :pop-to-buffer nil))

(cl-defun opencode-open-session (session &key (pop-to-buffer t) callback)
  "Open comint based shell for SESSION.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer.
Returns the buffer.
If CALLBACK is given, it will be called with the session after it is initialized."
  (let-alist session
    (let ((old-buffer (gethash .id opencode-session-buffers)))
      (if (buffer-live-p old-buffer)
          (if pop-to-buffer
              (pop-to-buffer old-buffer)
            (switch-to-buffer old-buffer))
        (let ((buffer (generate-new-buffer (format "*OpenCode: %s*" .title)))
              (agent (copy-tree opencode-session-agent))
              (agents (copy-tree opencode-session-agents)))
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq opencode-session-id .id
                  opencode-session-directory (file-name-as-directory .directory)
                  opencode-last-session-buffer buffer
                  default-directory (file-name-as-directory .directory)
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
                  opencode-session--bootstrapping t
                  opencode-session--queued-events nil
                  opencode-session--draining-queued-events nil
                  opencode-session--status-poll-timer nil
                  opencode-session--suspect-reconcile-timer nil
                  opencode-session--stream-quiet-timer nil
                  opencode-session--stream-message-states (make-hash-table :test 'equal)
                  opencode-assistant-messages nil
                  opencode-rendered-message-ids (make-hash-table :test 'equal)
                  opencode-session--interrupted-message-ids (make-hash-table :test 'equal)
                  opencode--pending-question-tools (make-hash-table :test 'equal)
                  opencode--completed-question-tools (make-hash-table :test 'equal)
                  opencode--displayed-question-ids (make-hash-table :test 'equal)
                  opencode--pending-permission-tools (make-hash-table :test 'equal)
                  opencode-shell-calls (make-hash-table :test 'equal)
                  opencode-session-agents (mapcar (lambda (agent)
                                                    (unless (alist-get 'model agent)
                                                      (setf (alist-get 'model agent)
                                                            opencode-last-model))
                                                    agent)
                                                  (or agents
                                                      (copy-tree opencode-agents)))
                  opencode-session-agent (or agent (car opencode-session-agents))
                  mode-line-process '(:eval (opencode--session-status-indicator)))
            (hack-dir-local-variables-non-file-buffer)
            (add-hook 'fill-nobreak-predicate #'opencode--in-label-p nil t)
            (puthash .id buffer opencode-session-buffers)
            (when (fboundp 'opencode--download-slash-commands)
              (opencode--download-slash-commands default-directory))
            (let ((proc (start-process "dummy" buffer nil)))
              (set-process-query-on-exit-flag proc nil)
              (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
              (opencode-insert-logo)
              (opencode-api-session-messages (.id)
                  messages
                (dolist (message messages)
                  (let-alist (alist-get 'info message)
                    (pcase .role
                      ("user" (opencode--replay-user-request message))
                      ("assistant"
                       (opencode-session--render-complete-assistant-message
                        message)
                       (setq opencode-session-tokens
                             (+ .tokens.input .tokens.output .tokens.reasoning
                                .tokens.cache.read .tokens.cache.write))))))
                (opencode--show-prompt)
                (opencode-session--sync-pending-question)
                (setq opencode-session--bootstrapping nil)
                (let ((opencode-session--draining-queued-events t))
                  (dolist (event (nreverse opencode-session--queued-events))
                    (opencode--handle-message event)))
                (setq opencode-session--queued-events nil)
                (when callback
                  (funcall callback session))))
            (if pop-to-buffer
                (pop-to-buffer buffer)
              (switch-to-buffer buffer))))))))

(defun opencode--current-message-number ()
  "Return the 0-indexed message number at point.
Counts prompts from the beginning of the buffer to the current position.
Returns nil if point is before the first prompt."
  (save-excursion
    (end-of-line)
    (comint-previous-prompt 1)
    (let ((target-point (point)))
      (goto-char (point-min))
      (cl-loop do (comint-next-prompt 1)
               while (< (point) target-point)
               count t))))

(defmacro opencode--current-message-id (result &rest body)
  "Run BODY with RESULT as the message id of the user message at point."
  (declare (indent defun))
  `(opencode--current-message-exchange (,result _assistant-id)
     ,@body))

(defmacro opencode--current-message-exchange (bindings &rest body)
  "Run BODY with BINDINGS (user-id assistant-id)
bound to the exchange ids at point."
  (declare (indent defun))
  (let ((user-id (car bindings))
        (assistant-id (cadr bindings)))
    `(when-let (message-number (opencode--current-message-number))
       (let ((default-directory (opencode-session--directory)))
         (opencode-api-session-messages (opencode-session-id)
             messages
           (let* ((user-message-index
                   (cl-loop for message in messages
                            for index from 0
                            when (string= "user" (map-nested-elt message '(info role)))
                            count t into count
                            when (= (1- count) message-number)
                            return index))
                  (user-message (and user-message-index
                                     (nth user-message-index messages)))
                  (assistant-message (and user-message-index
                                          (let ((next-message (nth (1+ user-message-index)
                                                                   messages)))
                                            (when (string= "assistant"
                                                           (map-nested-elt next-message
                                                                           '(info role)))
                                              next-message))))
                  (,user-id (map-nested-elt user-message '(info id)))
                  (,assistant-id (map-nested-elt assistant-message '(info id))))
             ,@body))))))

(defun opencode--delete-message-at-point ()
  "Delete the prompt at point and its output from the session buffer."
  (let ((start (save-excursion
                 (end-of-line)
                 (comint-previous-prompt 1)
                 (line-beginning-position)))
        (end (save-excursion
               (end-of-line)
               (comint-previous-prompt 1)
               (goto-char (line-beginning-position 2))
               (if (ignore-errors (comint-next-prompt 1) t)
                   (line-beginning-position)
                 (point-max)))))
    (let ((inhibit-read-only t))
      (remove-overlays start end)
      (delete-region start end))))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let ((title (read-string "Title: ")))
    (let ((default-directory (opencode-session--directory)))
      (opencode-api-rename-session ((or (alist-get 'id session)
                                        opencode-session-id))
          `((title . ,title))
          _res
        (unless session
          (rename-buffer
           (generate-new-buffer-name (format "*OpenCode: %s*" title))))))))

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID and then new prompt."
  (when-let (buffer (gethash session-id opencode-session-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (opencode-session--flush-interrupted)
        (opencode--output (propertize message 'face 'error))
        (opencode--output "\n\n")
        (opencode--show-prompt)))))

(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (let ((default-directory (opencode-session--directory)))
    (opencode-api-abort-session (opencode-session-id)
        success-p
      (if success-p
          (progn
            (opencode-session--flush-interrupted)
            (opencode-session--set-status opencode-session-id "idle")
            (opencode--show-prompt))
        (message "Failed to abort session.")))))

(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))

(defun opencode-sessions-redisplay ()
  "Refresh the session display table for DIRECTORY."
  (interactive)
  (opencode-api-sessions sessions
    (let ((inhibit-read-only t)
          (point (point))
          (sessions (if opencode-session-control-verbose
                        sessions
                      (seq-remove (lambda (session)
                                    (alist-get 'parentID session))
                                  sessions)))
          cache)
      (erase-buffer)
      (if sessions
          (make-vtable
           :columns '("Title"
                      (:name "Branch" :min-width 6)
                      (:name "Last Updated" :width 12
			     :formatter opencode--format-time-ago
			     :primary ascend)
                      (:name "Files changed" :width 13 :align right)
                      (:name "Created at" :width 10
			     :formatter opencode--format-time-ago))
           :objects sessions
           :actions '("x" opencode-kill-session
                      "R" opencode-rename-session
                      "s" opencode-share-session
                      "u" opencode-unshare-session
                      "RET" opencode-open-session-same-window
                      "o" opencode-open-session-same-window)
           :getter (lambda (object column vtable)
                     (let-alist object
                       (pcase (vtable-column vtable column)
                         ("Title" (if .share
                                      (concat (propertize "shared " 'face
                                                          '(bold opencode-request-margin-highlight))
                                              .title)
                                    .title))
                         ("Branch" (if (and .directory (file-exists-p .directory))
                                       (let ((default-directory .directory))
                                         (with-memoization
                                             (map-elt cache .directory)
                                           (magit-get-current-branch)))
                                     "-"))
                         ("Last Updated" (opencode--time-ago object 'updated))
                         ("Files changed" (let-alist .summary
                                            (if (opencode--json-falsy .files)
                                                "none"
                                              (format "%d  +%d-%d"
                                                      .files
                                                      (if (opencode--json-falsy .additions)
                                                          0 .additions)
                                                      (if (opencode--json-falsy .deletions)
                                                          0 .deletions)))))
                         ("Created at" (opencode--time-ago object 'created)))))
           :separator-width 3
           :keymap opencode-session-control-mode-map)
        (insert "No sessions in " (or default-directory "unknown directory")))
      (goto-char point))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here
