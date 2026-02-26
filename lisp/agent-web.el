;;; agent-web.el --- HTTP frontend for agent-bridge and agent-board -*- lexical-binding: t; -*-

;;; Code:

(require 'web-server)
(require 'url-util)
(require 'agent-bridge)
(require 'agent-board)

(defcustom agent-web-port 7780
  "Port for the agent-web HTTP server."
  :type 'integer
  :group 'agent-bridge)

(defvar agent-web--server nil
  "The running ws-server instance.")

;;; HTML helpers

(defun agent-web--html-escape (text)
  "Escape TEXT for safe HTML display."
  (setq text (replace-regexp-in-string "&" "&amp;" text))
  (setq text (replace-regexp-in-string "<" "&lt;" text))
  (setq text (replace-regexp-in-string ">" "&gt;" text))
  (setq text (replace-regexp-in-string "\"" "&quot;" text))
  text)

(defun agent-web--render-sidebar (active-dir)
  "Render compact workspace sidebar HTML.
ACTIVE-DIR, when non-nil, highlights the matching row."
  (let* ((workspaces (agent-board--build-workspaces))
         (groups (make-hash-table :test 'equal)))
    ;; Group workspaces by project
    (dolist (ws workspaces)
      (let ((proj (agent-board-workspace-project ws)))
        (puthash proj (append (gethash proj groups) (list ws)) groups)))
    (concat
     (if workspaces
         (let ((sections nil))
           (maphash
            (lambda (proj wss)
              (push
               (concat
                (format "<h3>%s</h3>\n" (agent-web--html-escape proj))
                "<table class=\"sidebar-table\">\n"
                (mapconcat
                 (lambda (ws)
                   (let* ((status (agent-board--status ws))
                          (dir (agent-board-workspace-worktree ws))
                          (enc (agent-web--encode-dir dir))
                          (task (agent-board-workspace-task ws))
                          (label (or task (agent-board-workspace-branch ws)))
                          (active (and active-dir
                                       (file-equal-p dir active-dir))))
                     (format (concat
                              "<tr class=\"clickable%s\""
                              " onclick=\"parent.location='/session/%s'\">"
                              "<td class=\"%s\">&bull;</td>"
                              "<td>%s</td></tr>")
                             (if active " active" "")
                             enc
                             (agent-web--html-escape status)
                             (agent-web--html-escape label))))
                 wss "\n")
                "\n</table>\n"
                (format "<a href=\"/create/%s\"><button>+ new</button></a>\n"
                        (agent-web--encode-dir
                         (agent-board-workspace-toplevel (car wss)))))
               sections))
            groups)
           (apply #'concat (nreverse sections)))
       "<p>No workspaces.</p>"))))

(defun agent-web--html-page (title body &optional refresh active-dir)
  "Wrap BODY in HTML page with TITLE.
REFRESH is optional auto-refresh interval in seconds.
ACTIVE-DIR highlights the matching workspace in the sidebar."
  (concat
   "<!DOCTYPE html>\n<html>\n<head>\n"
   "<meta charset=\"utf-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   "<script src=\"https://cdn.jsdelivr.net/npm/marked/marked.min.js\"></script>\n"
   "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16/dist/katex.min.css\">\n"
   "<script src=\"https://cdn.jsdelivr.net/npm/katex@0.16/dist/katex.min.js\"></script>\n"
   "<title>" (agent-web--html-escape title) "</title>\n"
   "<style>\n"
   "body { font-family: monospace; margin: 0; display: flex;"
   " background: #1a1a2e; color: #e0e0e0; }\n"
   ".sidebar-col { width: 280px; flex-shrink: 0; height: 100vh;"
   " display: flex; flex-direction: column; border-right: 1px solid #333;"
   " background: #16213e; }\n"
   ".sidebar-col .dash-link { display: block; padding: 0.7em 1em 0.5em;"
   " font-size: 0.85em; }\n"
   ".sidebar-col iframe { flex: 1; border: none; width: 100%; }\n"
   ".content { flex: 1; overflow-y: auto; height: 100vh;"
   " padding: 1em 2em; box-sizing: border-box;"
   " display: flex; flex-direction: column-reverse; }\n"
   ".content-inner { }\n"
   "table { border-collapse: collapse; width: 100%; }\n"
   "th, td { border: 1px solid #333; padding: 6px 10px; text-align: left; }\n"
   "th { background: #16213e; }\n"
   "a { color: #4fc3f7; }\n"
   "pre { background: #0f0f23; padding: 1em; white-space: pre-wrap;"
   " word-wrap: break-word; max-height: 70vh; overflow-y: auto; }\n"
   "textarea { width: 100%; height: 5em; background: #0f0f23; color: #e0e0e0;"
   " border: 1px solid #333; font-family: monospace; padding: 0.5em;"
   " box-sizing: border-box; }\n"
   "button { background: #16213e; color: #e0e0e0; border: 1px solid #4fc3f7;"
   " padding: 6px 16px; cursor: pointer; font-family: monospace; margin: 2px; }\n"
   "button:hover { background: #1a3a5c; }\n"
   ".idle { color: #66bb6a; }\n"
   ".busy, .running, .waiting { color: #ffa726; }\n"
   ".exited, .error, .completed { color: #ef5350; }\n"
   ".no-agent, .no-session { color: #888; }\n"
   "tr.clickable { cursor: pointer; }\n"
   "tr.clickable:hover { background: #1a3a5c; }\n"
   ".actions form { display: inline-block; }\n"
   ".output { background: #0f0f23; padding: 1em; max-height: 70vh;"
   " overflow-y: auto; white-space: pre-wrap; word-wrap: break-word;"
   " font-size: 0.9em; }\n"
   ".output pre { padding: 0.5em; margin: 0; max-height: none; overflow: visible; }\n"
   "details { margin: 0.2em 0; }\n"
   "summary { cursor: pointer; color: #888; }\n"
   "details pre { color: #999; }\n"
   ".user-prompt { margin: 1em 0 0.5em; }\n"
   ".assistant-text { white-space: pre-wrap; word-wrap: break-word; }\n"
   ".tool-use summary, .tool-result summary { font-size: 0.9em; }\n"
   ".tool-result.error summary { color: #ef5350; }\n"
   ".result { margin-top: 1em; color: #a5d6a7; }\n"
   "h1 { color: #4fc3f7; }\n"
   ".assistant-text code { background: #0f0f23; padding: 2px 4px; }\n"
   ".assistant-text pre { background: #0f0f23; padding: 1em; }\n"
   ".assistant-text pre code { padding: 0; }\n"
   ".tool-use.in-progress summary { color: #ffa726; }\n"
   ".tool-use.in-progress { border-left: 2px solid #ffa726;"
   " padding-left: 0.5em; }\n"
   "@keyframes pulse { 0%,100% { opacity: 1; } 50% { opacity: 0.4; } }\n"
   ".status-indicator { display: inline-block; margin-right: 0.3em; }\n"
   ".status-indicator.running { animation: pulse 1.5s infinite; color: #ffa726; }\n"
   ".status-indicator.completed { color: #66bb6a; }\n"
   ".status-indicator.failed { color: #ef5350; }\n"
   ".diff-block { margin: 0.5em 0; }\n"
   ".diff-block pre { margin: 0; font-size: 0.85em; }\n"
   ".diff-old pre { background: #2a0a0a; color: #ef9a9a; }\n"
   ".diff-new pre { background: #0a2a0a; color: #a5d6a7; }\n"
   ".diff-file { color: #4fc3f7; font-size: 0.85em; margin-bottom: 0.3em; }\n"
   ".plan-list { list-style: none; padding: 0; margin: 0.3em 0; }\n"
   ".plan-list li { padding: 2px 0; }\n"
   ".plan-status { display: inline-block; width: 1.2em; text-align: center; }\n"
   ".usage-bar { display: inline-flex; align-items: center; gap: 0.5em;"
   " font-size: 0.8em; margin-left: 1em; color: #aaa; }\n"
   ".usage-bar-track { width: 80px; height: 8px; background: #333;"
   " border-radius: 4px; overflow: hidden; display: inline-block; }\n"
   ".usage-bar-fill { height: 100%; border-radius: 4px; }\n"
   "@media (max-width: 768px) {\n"
   " body { flex-direction: column; }\n"
   " .sidebar-col { width: 100%; height: auto; flex-direction: row;"
   " align-items: center; border-right: none;"
   " border-bottom: 1px solid #333; }\n"
   " .sidebar-col iframe { display: none; }\n"
   " .sidebar-col .dash-link { padding: 0.6em 1em; }\n"
   " .content { height: auto; min-height: 80vh; padding: 0.5em 0.8em; }\n"
   " .output { max-height: none; }\n"
   " pre { max-height: none; }\n"
   " textarea { font-size: 16px; }\n"
   " button { padding: 10px 18px; }\n"
   " h1 { font-size: 1.1em; margin: 0.4em 0; }\n"
   "}\n"
   "</style>\n"
   (if refresh (format "<meta http-equiv=\"refresh\" content=\"%d\">\n" refresh) "")
   "<script>\n"
   "addEventListener('DOMContentLoaded',function(){\n"
   " if(typeof marked!=='undefined'&&typeof katex!=='undefined'){\n"
   "  marked.use({extensions:[{name:'math',level:'inline',\n"
   "   start:function(src){return src.indexOf('$');},\n"
   "   tokenizer:function(src){\n"
   "    var b=src.match(/^\\$\\$([\\s\\S]+?)\\$\\$/);\n"
   "    if(b)return{type:'math',raw:b[0],text:b[1],display:true};\n"
   "    var i=src.match(/^\\$([^$\\n]+?)\\$/);\n"
   "    if(i)return{type:'math',raw:i[0],text:i[1],display:false};\n"
   "   },\n"
   "   renderer:function(token){\n"
   "    return katex.renderToString(token.text,\n"
   "     {displayMode:token.display,throwOnError:false});\n"
   "   }}]});\n"
   " }\n"
   " document.querySelectorAll('.assistant-text').forEach(function(el){\n"
   "  if(typeof marked!=='undefined')\n"
   "   el.innerHTML=marked.parse(el.textContent)})});\n"
   "</script>\n"
   "</head>\n<body>\n"
   "<div class=\"sidebar-col\">\n"
   "<a href=\"/\" class=\"dash-link\">&larr; Dashboard</a>\n"
   (format "<iframe src=\"/sidebar%s\"></iframe>\n"
           (if active-dir (concat "/" (agent-web--encode-dir active-dir)) ""))
   "</div>\n"
   "<main class=\"content\">\n"
   "<div class=\"content-inner\">\n"
   "<h1>" (agent-web--html-escape title) "</h1>\n"
   body
   "\n</div>\n</main>\n</body>\n</html>"))

;;; URL helpers

(defun agent-web--encode-dir (dir)
  "Encode DIR for use in URL paths."
  (url-hexify-string (directory-file-name dir)))

(defun agent-web--decode-dir (encoded)
  "Decode ENCODED directory from URL path."
  (file-name-as-directory (url-unhex-string encoded)))

(defun agent-web--path-segments (path)
  "Split URL PATH into segments."
  (split-string path "/" t))

;;; Output rendering

(defun agent-web--render-segments-html (segments)
  "Render display SEGMENTS as HTML."
  (let ((parts nil)
        (last-tool-label nil))
    (dolist (seg segments)
      (pcase (alist-get 'type seg)
        ('prompt
         (push (format "<div class=\"user-prompt\"><strong>&gt; %s</strong></div>\n"
                       (agent-web--html-escape (alist-get 'text seg)))
               parts))
        ('text
         (push (format "<div class=\"assistant-text\">%s</div>\n"
                       (agent-web--html-escape (alist-get 'text seg)))
               parts))
        ('thought
         (push (format
                (concat "<details class=\"tool-use\">"
                        "<summary>&#128161; Thinking</summary>"
                        "<pre>%s</pre></details>\n")
                (agent-web--html-escape (alist-get 'text seg)))
               parts))
        ('tool-use
         (let* ((name (alist-get 'name seg))
                (input (alist-get 'input seg))
                (status (or (alist-get 'status seg) "completed"))
                (label (agent-bridge-tool-label name input))
                (in-progress (member status '("pending" "running")))
                (status-icon
                 (cond
                  ((member status '("pending" "running")) "&#9696;")
                  ((equal status "failed") "&#10007;")
                  (t "&#9654;")))
                (status-class
                 (cond
                  ((member status '("pending" "running")) "running")
                  ((equal status "failed") "failed")
                  (t "completed"))))
           (setq last-tool-label label)
           (push (format
                  (concat "<details class=\"tool-use%s\">"
                          "<summary>"
                          "<span class=\"status-indicator %s\">%s</span>"
                          " %s</summary>"
                          "<pre>%s</pre></details>\n")
                  (if in-progress " in-progress" "")
                  (agent-web--html-escape status-class)
                  status-icon
                  (agent-web--html-escape label)
                  (agent-web--html-escape
                   (truncate-string-to-width input 2000)))
                 parts)))
        ('tool-result
         (let* ((is-error (alist-get 'error seg))
                (diff-old (alist-get 'diff-old seg))
                (diff-new (alist-get 'diff-new seg))
                (diff-file (alist-get 'diff-file seg))
                (label (concat (if is-error "error" "result")
                               (when last-tool-label
                                 (concat ": " last-tool-label)))))
           (setq last-tool-label nil)
           (push (format
                  (concat "<details class=\"tool-result%s\">"
                          "<summary>%s</summary>"
                          "%s"
                          "<pre>%s</pre></details>\n")
                  (if is-error " error" "")
                  (agent-web--html-escape label)
                  (if (or diff-old diff-new)
                      (concat
                       "<div class=\"diff-block\">"
                       (when diff-file
                         (format "<div class=\"diff-file\">%s</div>"
                                 (agent-web--html-escape diff-file)))
                       (when diff-old
                         (format "<div class=\"diff-old\"><pre>%s</pre></div>"
                                 (agent-web--html-escape
                                  (truncate-string-to-width diff-old 3000))))
                       (when diff-new
                         (format "<div class=\"diff-new\"><pre>%s</pre></div>"
                                 (agent-web--html-escape
                                  (truncate-string-to-width diff-new 3000))))
                       "</div>")
                    "")
                  (agent-web--html-escape
                   (truncate-string-to-width (alist-get 'text seg) 2000)))
                 parts)))
        ('plan
         (let* ((entries (alist-get 'entries seg))
                (items
                 (mapconcat
                  (lambda (entry)
                    (let* ((st (or (map-elt entry 'status) "pending"))
                           (content (or (map-elt entry 'content) ""))
                           (icon (cond
                                  ((equal st "completed") "&#9989;")
                                  ((equal st "in_progress") "&#128992;")
                                  (t "&#9898;"))))
                      (format "<li><span class=\"plan-status\">%s</span> %s</li>"
                              icon (agent-web--html-escape content))))
                  entries "")))
           (push (format
                  (concat "<details open class=\"tool-use\">"
                          "<summary>Plan</summary>"
                          "<ul class=\"plan-list\">%s</ul>"
                          "</details>\n")
                  items)
                 parts)))
        ('result
         (push (format "<div class=\"result\"><hr>%s</div>\n"
                       (agent-web--html-escape (alist-get 'text seg)))
               parts))))
    (apply #'concat (nreverse parts))))

;;; Response helpers

(defun agent-web--respond (process html)
  "Send 200 HTML response to PROCESS."
  (ws-response-header process 200
    '("Content-type" . "text/html; charset=utf-8"))
  (process-send-string process html))

(defun agent-web--redirect (process url)
  "Send 303 redirect to URL via PROCESS."
  (ws-response-header process 303
    (cons "Location" url)
    '("Content-type" . "text/plain"))
  (process-send-string process "Redirecting...\n"))

;;; Route: GET /sidebar (iframe)

(defun agent-web--handle-sidebar (request)
  "Render the sidebar as a standalone page for iframe embedding."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :GET headers)))
           (segs (agent-web--path-segments path))
           (active-dir (when (nth 1 segs)
                         (agent-web--decode-dir (nth 1 segs)))))
      (ws-response-header process 200
        '("Content-type" . "text/html; charset=utf-8"))
      (process-send-string
       process
       (concat
        "<!DOCTYPE html>\n<html>\n<head>\n"
        "<meta charset=\"utf-8\">\n"
        "<meta http-equiv=\"refresh\" content=\"5\">\n"
        "<base target=\"_parent\">\n"
        "<style>\n"
        "body { font-family: monospace; margin: 0; padding: 1em;"
        " background: #16213e; color: #e0e0e0; }\n"
        "h3 { margin: 0.8em 0 0.3em; font-size: 0.9em; color: #888; }\n"
        ".sidebar-table { width: 100%; font-size: 0.85em;"
        " border-collapse: collapse; }\n"
        ".sidebar-table td { border: none; padding: 4px 6px; }\n"
        ".sidebar-table tr.clickable { cursor: pointer; }\n"
        ".sidebar-table tr.clickable:hover { background: #1a3a5c; }\n"
        ".sidebar-table tr.active { background: #1a3a5c; }\n"
        "a { color: #4fc3f7; }\n"
        "button { background: #16213e; color: #e0e0e0;"
        " border: 1px solid #4fc3f7; padding: 4px 10px;"
        " cursor: pointer; font-family: monospace;"
        " font-size: 0.85em; margin: 2px; }\n"
        "button:hover { background: #1a3a5c; }\n"
        ".idle { color: #66bb6a; }\n"
        ".busy, .running, .waiting { color: #ffa726; }\n"
        ".exited, .error, .completed { color: #ef5350; }\n"
        ".no-agent, .no-session { color: #888; }\n"
        "</style>\n"
        "</head>\n<body>\n"
        (agent-web--render-sidebar active-dir)
        "\n</body>\n</html>")))))

;;; Route: GET / (Dashboard)

(defun agent-web--handle-dashboard (request)
  "Render the workspace dashboard."
  (with-slots (process) request
    (let* ((workspaces (agent-board--build-workspaces))
           (rows
            (mapconcat
             (lambda (ws)
               (let* ((status (agent-board--status ws))
                      (dir (agent-board-workspace-worktree ws))
                      (enc (agent-web--encode-dir dir))
                      (url (format "/session/%s" enc)))
                 (format (concat
                          "<tr class=\"clickable\" onclick=\"location='%s'\">"
                          "<td class=\"%s\">%s</td>"
                          "<td>%s</td><td>%s</td>"
                          "<td>%s</td><td>%s</td></tr>")
                         url
                         (agent-web--html-escape status)
                         (agent-web--html-escape status)
                         (agent-web--html-escape
                          (agent-board-workspace-project ws))
                         (agent-web--html-escape
                          (or (agent-board-workspace-task ws) "-"))
                         (agent-web--html-escape
                          (agent-board-workspace-branch ws))
                         (agent-web--html-escape
                          (abbreviate-file-name dir)))))
             workspaces "\n"))
           (toplevels
            (delete-dups
             (mapcar #'agent-board-workspace-toplevel workspaces))))
      (agent-web--respond
       process
       (agent-web--html-page
        "Agent Board"
        (if workspaces
            (concat
             "<table>\n"
             "<tr><th>Status</th><th>Project</th><th>Task</th>"
             "<th>Branch</th><th>Worktree</th></tr>\n"
             rows "\n</table>\n"
             "<div class=\"actions\" style=\"margin-top:1em\">"
             (mapconcat
              (lambda (tl)
                (format "<a href=\"/create/%s\"><button>New worktree in %s</button></a>"
                        (agent-web--encode-dir tl)
                        (agent-web--html-escape
                         (file-name-nondirectory
                          (directory-file-name tl)))))
              toplevels " ")
             "</div>")
          "<p>No workspaces found. Open agent-board in Emacs first.</p>")
        5)))))

;;; Route: GET /session/DIR

(defun agent-web--render-usage-bar (buf)
  "Render HTML usage bar for agent-shell BUF, or empty string."
  (let ((usage (agent-bridge-usage buf)))
    (if (not usage) ""
      (let* ((used (or (map-elt usage :context-used) 0))
             (size (or (map-elt usage :context-size) 1))
             (pct (min 100.0 (* 100.0 (/ (float used) size))))
             (cost (or (map-elt usage :cost-amount) 0))
             (currency (or (map-elt usage :cost-currency) "$"))
             (bar-color (cond
                         ((>= pct 85) "#ef5350")
                         ((>= pct 60) "#ffa726")
                         (t "#66bb6a"))))
        (format (concat
                 "<span class=\"usage-bar\">"
                 "<span class=\"usage-bar-track\">"
                 "<span class=\"usage-bar-fill\""
                 " style=\"width:%.0f%%;background:%s\"></span>"
                 "</span>"
                 " %.0f%% ctx"
                 " &middot; %s%.2f"
                 "</span>")
                pct bar-color pct currency cost)))))

(defun agent-web--handle-session (request)
  "Render a session view."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :GET headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (buf (agent-bridge-find-buffer dir))
           (enc (agent-web--encode-dir dir))
           (status (agent-bridge-status buf))
           (busy-p (member status '("busy" "waiting"))))
      (agent-web--respond
       process
       (agent-web--html-page
        (format "Session: %s" (abbreviate-file-name dir))
        (concat
         "<div class=\"actions\" style=\"margin-bottom:1em\">"
         (format "<span class=\"%s\">%s</span>"
                 (agent-web--html-escape status)
                 (agent-web--html-escape status))
         (agent-web--render-usage-bar buf)
         " "
         (format "<form method=\"POST\" action=\"/session/%s/start\">" enc)
         "<button>New Session</button></form>"
         (format "<form method=\"POST\" action=\"/session/%s/interrupt\">" enc)
         "<button>Interrupt</button></form>"
         "</div>\n"
         "<div class=\"output\">"
         (let ((segments (and buf (agent-bridge-segments buf))))
           (if segments
               (concat
                (agent-web--render-segments-html segments)
                (when busy-p
                  "<p class=\"busy\">Agent is working...</p>"))
             (if buf
                 (let ((output (agent-bridge-buffer-text buf)))
                   (if (or (null output) (string-empty-p output))
                       "<pre>(no output yet)</pre>"
                     (concat
                      (format "<pre>%s</pre>"
                              (agent-web--html-escape output))
                      (when busy-p
                        "<p class=\"busy\">Agent is working...</p>"))))
               "<pre>(no session)</pre>")))
         "</div>\n"
         ;; Pending permission requests
         (let ((perms (and buf (agent-bridge-pending-permissions buf))))
           (if perms
               (mapconcat
                (lambda (perm)
                  (let ((tc-id (alist-get 'tool-call-id perm))
                        (title (alist-get 'title perm)))
                    (concat
                     "<div class=\"denials\" style=\"margin:1em 0;"
                     " padding:1em; border:1px solid #ffa726;"
                     " background:#2a1a00\">\n"
                     "<p style=\"color:#ffa726; margin:0 0 0.5em\">"
                     "Permission requested:</p>\n"
                     (format "<div>%s</div>\n"
                             (agent-web--html-escape title))
                     (format (concat
                              "<form method=\"POST\""
                              " action=\"/session/%s/approve/%s\""
                              " style=\"display:inline\">"
                              "<button style=\"border-color:#66bb6a;"
                              " margin-top:0.5em\">Allow</button>"
                              "</form>\n"
                              "<form method=\"POST\""
                              " action=\"/session/%s/deny/%s\""
                              " style=\"display:inline\">"
                              "<button style=\"border-color:#ef5350;"
                              " margin-top:0.5em\">Deny</button>"
                              "</form>\n")
                             enc (url-hexify-string tc-id)
                             enc (url-hexify-string tc-id))
                     "</div>\n")))
                perms "")
             ""))
         (if busy-p ""
           (concat
            (format "<form method=\"POST\" action=\"/session/%s/send\">\n" enc)
            "<textarea name=\"prompt\" placeholder=\"Enter prompt...\""
            " autofocus></textarea><br>\n"
            "<button type=\"submit\">Send</button>\n"
            "</form>\n"))
         (let ((cmds (append (agent-bridge-commands buf) nil)))
           (if cmds
               (concat
                "<details style=\"margin-top:1.5em\">\n"
                "<summary style=\"color:#888;cursor:pointer\">Slash commands</summary>\n"
                "<table style=\"margin-top:0.5em;font-size:0.85em\">\n"
                (mapconcat
                 (lambda (cmd)
                   (format "<tr><td style=\"color:#4fc3f7;padding-right:1em\">/%s</td><td>%s</td></tr>"
                           (agent-web--html-escape (or (map-elt cmd 'name) ""))
                           (agent-web--html-escape (or (map-elt cmd 'description) ""))))
                 cmds "\n")
                "\n</table>\n</details>\n")
             "")))
        (when busy-p 3) dir)))))

;;; Route: POST /session/DIR/send

(defun agent-web--handle-send (request)
  "Handle prompt submission."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (prompt (or (cdr (assoc "prompt" headers)) ""))
           (enc (agent-web--encode-dir dir))
           (buf (agent-bridge-ensure dir)))
      (when (not (string-empty-p prompt))
        (agent-bridge-send prompt buf))
      (agent-web--redirect process (format "/session/%s" enc)))))

;;; Route: POST /session/DIR/approve

(defun agent-web--handle-approve (request)
  "Approve a pending permission request."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (tc-id (url-unhex-string (nth 3 segs)))
           (enc (agent-web--encode-dir dir))
           (buf (agent-bridge-find-buffer dir)))
      (when (and buf tc-id)
        (agent-bridge-approve buf tc-id))
      (agent-web--redirect process (format "/session/%s" enc)))))

;;; Route: POST /session/DIR/deny

(defun agent-web--handle-deny (request)
  "Deny a pending permission request."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (tc-id (url-unhex-string (nth 3 segs)))
           (enc (agent-web--encode-dir dir))
           (buf (agent-bridge-find-buffer dir)))
      (when (and buf tc-id)
        (agent-bridge-deny buf tc-id))
      (agent-web--redirect process (format "/session/%s" enc)))))

;;; Route: POST /session/DIR/start

(defun agent-web--handle-start (request)
  "Start a new agent session."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (enc (agent-web--encode-dir dir)))
      (agent-bridge-ensure dir)
      (agent-web--redirect process (format "/session/%s" enc)))))

;;; Route: POST /session/DIR/interrupt

(defun agent-web--handle-interrupt (request)
  "Interrupt a running session."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (enc (agent-web--encode-dir dir))
           (buf (agent-bridge-find-buffer dir)))
      (when buf
        (agent-bridge-interrupt buf))
      (agent-web--redirect process (format "/session/%s" enc)))))

;;; Route: GET /history/DIR

(defun agent-web--handle-history (request)
  "Show past sessions for a directory."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :GET headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (enc (agent-web--encode-dir dir))
           (sessions (agent-bridge--scan-sessions dir))
           (rows
            (mapconcat
             (lambda (s)
               (let* ((sid (alist-get 'id s))
                      (mtime (alist-get 'mtime s))
                      (branch (or (alist-get 'git-branch s) "-"))
                      (summary (or (alist-get 'summary s)
                                   (alist-get 'first-prompt s)
                                   "-"))
                      (date-str (if mtime
                                    (agent-bridge--format-session-date mtime)
                                  "-")))
                 (concat
                  "<tr>"
                  (format "<td>%s</td>" (agent-web--html-escape date-str))
                  (format "<td>%s</td>" (agent-web--html-escape branch))
                  (format "<td>%s</td>"
                          (agent-web--html-escape
                           (truncate-string-to-width summary 80)))
                  "<td>"
                  (format (concat
                           "<form method=\"POST\""
                           " action=\"/session/%s/resume/%s\">"
                           "<button>Resume</button></form>")
                          enc (url-hexify-string sid))
                  "</td>"
                  "</tr>")))
             sessions "\n")))
      (agent-web--respond
       process
       (agent-web--html-page
        (format "Sessions: %s" (abbreviate-file-name dir))
        (if sessions
            (concat
             "<table>\n"
             "<tr><th>Date</th><th>Branch</th><th>Summary</th><th></th></tr>\n"
             rows "\n</table>")
          "<p>No past sessions found.</p>")
        nil dir)))))

;;; Route: POST /session/DIR/resume/SID

(defun agent-web--handle-resume (request)
  "Resume a past session by ID."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (sid (url-unhex-string (nth 3 segs)))
           (enc (agent-web--encode-dir dir)))
      (agent-bridge-resume dir sid)
      (agent-web--redirect process (format "/session/%s" enc)))))

;;; Route: GET /session/DIR/view/SID

(defun agent-web--handle-view-session (request)
  "View a past session as read-only conversation."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :GET headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (sid (url-unhex-string (nth 3 segs)))
           (sessions-dir (agent-bridge--claude-sessions-dir dir))
           (file (expand-file-name (concat sid ".jsonl") sessions-dir)))
      (agent-web--respond
       process
       (agent-web--html-page
        (format "Session: %s" (truncate-string-to-width sid 8))
        (if (file-readable-p file)
            (let ((segments (agent-bridge-parse-session-history file)))
              (concat
               (format "<p><a href=\"/history/%s\">&larr; Back to history</a></p>\n"
                       (agent-web--encode-dir dir))
               "<div class=\"output\">"
               (agent-web--render-segments-html segments)
               "</div>"))
          "<p>Session file not found.</p>")
        nil dir)))))

;;; Route: GET /create/REPO

(defun agent-web--handle-create-form (request)
  "Render the create-worktree form."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :GET headers)))
           (segs (agent-web--path-segments path))
           (toplevel (agent-web--decode-dir (nth 1 segs)))
           (enc (agent-web--encode-dir toplevel))
           (default-directory toplevel)
           (branches (magit-list-local-branch-names))
           (datalist (mapconcat
                      (lambda (b)
                        (format "<option value=\"%s\">"
                                (agent-web--html-escape b)))
                      branches "\n")))
      (agent-web--respond
       process
       (agent-web--html-page
        (format "New worktree in %s"
                (file-name-nondirectory (directory-file-name toplevel)))
        (concat
         (format "<form method=\"POST\" action=\"/create/%s\">\n" enc)
         "<datalist id=\"branches\">\n" datalist "\n</datalist>\n"
         "<label>Branch<br>"
         "<input name=\"branch\" list=\"branches\" required"
         " style=\"width:100%;background:#0f0f23;color:#e0e0e0;"
         "border:1px solid #333;padding:0.4em;font-family:monospace\">"
         "</label><br><br>\n"
         "<label>Start point (leave empty to checkout existing branch)<br>"
         "<input name=\"start-point\" list=\"branches\""
         " style=\"width:100%;background:#0f0f23;color:#e0e0e0;"
         "border:1px solid #333;padding:0.4em;font-family:monospace\">"
         "</label><br><br>\n"
         "<label>Task description<br>"
         "<input name=\"task\""
         " style=\"width:100%;background:#0f0f23;color:#e0e0e0;"
         "border:1px solid #333;padding:0.4em;font-family:monospace\">"
         "</label><br><br>\n"
         "<button type=\"submit\">Create</button>\n"
         "</form>"))))))

;;; Route: POST /create/REPO

(defun agent-web--handle-create (request)
  "Handle worktree creation."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (toplevel (agent-web--decode-dir (nth 1 segs)))
           (branch (or (cdr (assoc "branch" headers)) ""))
           (start-point (or (cdr (assoc "start-point" headers)) ""))
           (task (or (cdr (assoc "task" headers)) ""))
           (default-directory toplevel)
           (worktree-dir (expand-file-name
                          branch
                          (file-name-directory
                           (directory-file-name toplevel)))))
      (when (string-empty-p branch)
        (error "Branch name is required"))
      (if (or (string-empty-p start-point)
              (magit-local-branch-p branch))
          ;; Existing branch -- checkout
          (magit-call-git "worktree" "add" worktree-dir branch)
        ;; New branch -- create from start-point
        (magit-call-git "worktree" "add" "-b" branch
                        worktree-dir start-point))
      (when (and (not (string-empty-p task))
                 (not (string= branch "(detached)")))
        (magit-call-git "config"
                        (format "branch.%s.description" branch) task))
      (agent-web--redirect process "/"))))

;;; Route: POST /delete/DIR

(defun agent-web--handle-delete (request)
  "Handle worktree deletion."
  (with-slots (process headers) request
    (let* ((path (cdr (assoc :POST headers)))
           (segs (agent-web--path-segments path))
           (dir (agent-web--decode-dir (nth 1 segs)))
           (toplevel (or (cdr (assoc "toplevel" headers)) ""))
           (branch (or (cdr (assoc "branch" headers)) ""))
           (buf (agent-web--find-agent-buffer dir)))
      ;; Kill agent buffer if present
      (when buf (kill-buffer buf))
      ;; Remove worktree
      (let ((default-directory toplevel))
        (magit-call-git "worktree" "remove" "--force" dir))
      ;; Delete branch
      (when (and (not (string-empty-p branch))
                 (not (string= branch "(detached)"))
                 (let ((default-directory toplevel))
                   (magit-local-branch-p branch)))
        (let ((default-directory toplevel))
          (magit-call-git "branch" "-D" branch)))
      (agent-web--redirect process "/"))))

(defun agent-web--find-agent-buffer (dir)
  "Find a live agent-shell buffer for DIR."
  (agent-bridge-find-buffer dir))

;;; Server lifecycle

;;;###autoload
(defun agent-web-start (&optional port)
  "Start the agent-web HTTP server on PORT (default `agent-web-port')."
  (interactive)
  (when agent-web--server
    (user-error "Server already running; use agent-web-stop first"))
  (let ((p (or port agent-web-port)))
    (setq agent-web--server
          (ws-start
           '(((:GET  . "^/sidebar\\(/[^/]+\\)?$")
              . agent-web--handle-sidebar)
             ((:GET  . "^/$")
              . agent-web--handle-dashboard)
             ((:GET  . "^/session/[^/]+$")
              . agent-web--handle-session)
             ((:POST . "^/session/[^/]+/send$")
              . agent-web--handle-send)
             ((:POST . "^/session/[^/]+/approve/[^/]+$")
              . agent-web--handle-approve)
             ((:POST . "^/session/[^/]+/deny/[^/]+$")
              . agent-web--handle-deny)
             ((:POST . "^/session/[^/]+/start$")
              . agent-web--handle-start)
             ((:POST . "^/session/[^/]+/interrupt$")
              . agent-web--handle-interrupt)
             ((:GET  . "^/history/[^/]+$")
              . agent-web--handle-history)
             ((:GET  . "^/session/[^/]+/view/.+$")
              . agent-web--handle-view-session)
             ((:GET  . "^/create/[^/]+$")
              . agent-web--handle-create-form)
             ((:POST . "^/create/[^/]+$")
              . agent-web--handle-create)
             ((:POST . "^/session/[^/]+/resume/.+$")
              . agent-web--handle-resume)
             ((:POST . "^/delete/[^/]+$")
              . agent-web--handle-delete))
           p))
    (message "agent-web: listening on port %d" p)))

;;;###autoload
(defun agent-web-stop ()
  "Stop the agent-web HTTP server."
  (interactive)
  (if agent-web--server
      (progn
        (ws-stop agent-web--server)
        (setq agent-web--server nil)
        (message "agent-web: stopped"))
    (message "agent-web: no server running")))

(provide 'agent-web)
;;; agent-web.el ends here
