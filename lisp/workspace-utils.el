;; Workspace-aware Emacs configuration
;;
;; Dependencies:
;; - prodigy, projectile, hydra, code-cells (Emacs packages)
;; - ipython (Python package, should be in your vision environment)
;; - jupytext (optional, only needed for .ipynb conversion)
;;
;; TODO: IPython Prompt Detection Warning
;; =====================================
;; There's a persistent "Python shell prompts cannot be detected" warning
;; when starting IPython shells. This is a known compatibility issue between
;; IPython 5+ (which uses prompt_toolkit) and Emacs' inferior shell mode.
;; 
;; References:
;; - https://github.com/ipython/ipython/issues/9816 (IPython prompt_toolkit issues)
;; - https://emacs.stackexchange.com/questions/24453/python-shell-prompts-cannot-be-detected
;; - https://github.com/jorgenschaefer/elpy/issues/1550 (Elpy similar issues)
;; - Emacs python.el source: python-shell-prompt-detect function
;; 
;; Current workaround:
;; - Warning is disabled via python-shell-prompt-detect-failure-warning nil
;; - IPython is configured with --simple-prompt and proper regex patterns
;; - Nix shell wrapper is used which may interfere with prompt detection
;; 
;; The warning doesn't affect functionality but indicates suboptimal integration.
;; Future improvements could include:
;; - Switching to jupyter-console instead of IPython
;; - Using a different Python REPL integration (like python-mode alternatives)
;; - Finding a better way to launch IPython that doesn't trigger the warning
;; 
;; For now, everything works but the warning persists despite our configurations.

(require 'prodigy)
(require 'projectile nil t) ; Optional dependency
(require 'hydra)
(require 'cl-lib)

;; Buffer name format constants
(defconst workspace-arduino-shell-buffer-format "*Arduino Shell - %s*"
  "Format string for Arduino shell buffer names.")
(defconst workspace-mock-robots-buffer-format "*Mock Robots - %s*"
  "Format string for mock robots buffer names.")

;; Variables for remembering last inputs
(defvar workspace-integration-test-history nil
  "History list for integration test names.
Add this to your init.el to persist across sessions: (savehist-mode 1)")

;;;###autoload
(defun workspace-create-nix-ipython-interpreter (project-root)
  "Create a proper IPython interpreter configuration for nix shell."
  (let ((nix-shell-cmd (format "nix --quiet --no-warn-dirty develop --impure %s.#vision --command" project-root)))
    ;; Set up environment variables for better IPython detection
    (setenv "IPYTHONDIR" (expand-file-name ".ipython" project-root))
    (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
    
    ;; Return configuration for local use
    (list
     :interpreter "ipython"
     :args "-i --simple-prompt --InteractiveShell.display_page=True"
     :nix-wrapper nix-shell-cmd
     :prompt-regexp "In \\[[0-9]+\\]: "
     :prompt-output-regexp "Out\\[[0-9]+\\]: "
     :prompt-block-regexp "[.][.][.]+: ")))

;; Projectile integration
(defun workspace-projectile-setup ()
  "Setup workspace based on current projectile project."
  (interactive)
  (if (and (featurep 'projectile) (projectile-project-p))
      (let* ((project-root (projectile-project-root))
             (project-name (projectile-project-name))
             (workspace-path project-root)
             (workspace-name project-name))
        (message "Setting up projectile workspace: %s at %s" workspace-name workspace-path)
        (setenv "WORKSPACE_PATH" workspace-path)
        (setenv "WORKSPACE_NAME" workspace-name)
        (workspace-setup-prodigy))
    (message "Not in a projectile project or projectile not available")))

;; Hook into projectile if available
(when (featurep 'projectile)
  (add-hook 'projectile-switch-project-hook 'workspace-projectile-setup))

;;;###autoload
(defun workspace-setup-prodigy ()
  "Setup prodigy services for the current workspace.
Uses project-scoped tags so services from different projects can coexist."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((workspace-path (projectile-project-root))
           (workspace-name (projectile-project-name))
           ;; Create a unique tag for this project
           (project-tag (intern (format "project/%s" workspace-name))))

      (message "Setting up workspace: %s at %s" workspace-name workspace-path)

      ;; Remove ONLY services tagged with this project (not all services)
      (setq prodigy-services
            (seq-remove (lambda (service)
                          (memq project-tag (plist-get service :tags)))
                        prodigy-services))

      ;; Define services with project-specific names and tags
      (prodigy-define-service
        :name (format "%s/calibration_service" workspace-name)
        :command "nix"
        :args '("run" ".#calibration-service")
        :cwd workspace-path
        :tags (list project-tag 'workspace 'vision))

      (prodigy-define-service
        :name (format "%s/proxy" workspace-name)
        :command "/home/francisco/grpcwebproxy/grpcwebproxy-v0.15.0-linux-x86_64"
        :args '("--backend_addr" "localhost:6001"
                "--run_tls_server=false"
                "--server_http_debug_port" "9900"
                "--server_http_max_read_timeout=300s"
                "--server_http_max_write_timeout=300s")
        :tags (list project-tag 'workspace 'vision 'proxy))

      (prodigy-define-service
        :name (format "%s/portico" workspace-name)
        :command "yarn"
        :args '("run" "dev")
        :cwd (concat workspace-path "/portico")
        :tags (list project-tag 'workspace 'vision))

      (prodigy-define-service
        :name (format "%s/atrium" workspace-name)
        :command "electron"
        :args '("atrium" "--allow-multiple-instances")
        :cwd "~/sources/atrium-linux"
        :tags (list project-tag 'workspace 'vision))

      ;; Open prodigy from the workspace root
      (let ((default-directory workspace-path))
        (prodigy))

      ;; Filter to current project (use 'f' to clear filters in prodigy if needed)
      (message "Workspace services configured for %s (filter by tag: %s)"
               workspace-name project-tag))))

;;;###autoload
(defun workspace-open-notebook ()
  "Open a notebook and start dedicated IPython for it."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (notebook-dir (concat project-root "vision/experimental/notebooks")))
      
      ;; Select notebook file using helm
      (let ((notebook-file 
             (if (file-directory-p notebook-dir)
                 (let ((default-directory notebook-dir))
                   (if (fboundp 'helm-find-files)
                       (helm :sources
                             (helm-build-sync-source "Notebooks"
                               :candidates (lambda ()
                                           (directory-files-recursively notebook-dir "\\.\\(py\\|ipynb\\)$"))
                               :candidate-transformer (lambda (candidates)
                                                      (mapcar (lambda (c)
                                                               (cons (file-relative-name c notebook-dir) c))
                                                             candidates)))
                             :buffer "*helm notebooks*")
                     (read-file-name "Open notebook: " notebook-dir nil t nil
                                     (lambda (name) (or (string-suffix-p ".py" name)
                                                       (string-suffix-p ".ipynb" name))))))
               (read-file-name "Open notebook: " project-root nil t nil
                               (lambda (name) (or (string-suffix-p ".py" name)
                                                 (string-suffix-p ".ipynb" name)))))))
        
        (when notebook-file
          ;; Open the notebook file
          (find-file notebook-file)
          
          ;; Enable code-cells mode for notebook editing
          (when (string-suffix-p ".py" notebook-file)
            ;; Detect Jupytext format and set appropriate cell boundaries
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward "format_name: light" nil t)
                  (setq-local code-cells-boundary-regexp "^# \\+")
                (setq-local code-cells-boundary-regexp "^# %%")))
            (code-cells-mode 1)
            ;; Enable auto-revert for notebook files
            (auto-revert-mode 1)
            ;; Setup notebook-specific keybindings for this buffer
            (workspace-setup-notebook-keybindings)
            ;; Debug: show what cells are detected
            (message "code-cells-mode enabled, boundary regexp: %s" code-cells-boundary-regexp)
            (save-excursion
              (goto-char (point-min))
              (let ((cell-count 0))
                (while (re-search-forward code-cells-boundary-regexp nil t)
                  (setq cell-count (1+ cell-count))
                  (message "Found cell boundary at line %d: %s" (line-number-at-pos) (thing-at-point 'line t)))
                (message "Total cells detected: %d" (max 1 cell-count)))))
          
          ;; Create IPython process for this notebook
          (let* ((notebook-name (file-name-base notebook-file))
                 (python-buffer-name (format "*IPython[%s]*" notebook-name))
                 (default-directory project-root)
                 ;; Use nix shell wrapper but call ipython directly for better prompt detection
                 (python-shell-interpreter "nix")
                 (python-shell-interpreter-args "--quiet --no-warn-dirty develop --impure .#vision --command bash -c \"export PATH=/nix/store/30mhlcmz2p60rjgih2c3760zjc42g5m7-rerun-0.22.1/bin:$PATH && ipython -i --simple-prompt\"")
                 (python-shell-buffer-name python-buffer-name))
            
            ;; Start IPython in vision environment
            (message "Starting IPython for notebook: %s" notebook-name)
            (run-python)
            ;; Associate this buffer with the IPython process
            (setq-local python-shell-buffer-name python-buffer-name)
            
            (message "Notebook %s ready with IPython: %s" notebook-name python-buffer-name)))))))

;;;###autoload
(defun workspace-switch-to-notebook-ipython ()
  "Switch between notebook buffer and its associated IPython buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (current-name (buffer-name current-buffer)))
    (cond
     ;; If we're in an IPython buffer, find the associated notebook
     ((string-match "\\*IPython\\[\\(.*\\)\\]\\*" current-name)
      (let* ((notebook-name (match-string 1 current-name))
             (notebook-buffers (seq-filter 
                               (lambda (buf)
                                 (let ((buf-file (buffer-file-name buf)))
                                   (and buf-file 
                                        (string-equal (file-name-base buf-file) notebook-name)
                                        (or (string-suffix-p ".py" buf-file)
                                            (string-suffix-p ".ipynb" buf-file)))))
                               (buffer-list))))
        (if notebook-buffers
            (switch-to-buffer (car notebook-buffers))
          (message "No notebook buffer found for %s" notebook-name))))
     
     ;; If we're in a notebook buffer, find the associated IPython
     ((and (buffer-file-name current-buffer)
           (or (string-suffix-p ".py" (buffer-file-name current-buffer))
               (string-suffix-p ".ipynb" (buffer-file-name current-buffer))))
      (let* ((notebook-name (file-name-base (buffer-file-name current-buffer)))
             (ipython-buffer-name (format "*IPython[%s]*" notebook-name))
             ;; Also look for buffers that contain the notebook name in IPython format
             (python-buffers (cl-remove-if-not 
                             (lambda (buf) 
                               (string-match (format "IPython\\[%s\\]" (regexp-quote notebook-name)) 
                                           (buffer-name buf))) 
                             (buffer-list))))
        (cond 
         ;; First try exact match
         ((get-buffer ipython-buffer-name)
          (switch-to-buffer ipython-buffer-name))
         ;; Then try fuzzy match
         ((> (length python-buffers) 0)
          (switch-to-buffer (car python-buffers)))
         ;; No match found
         (t (message "No IPython buffer found for %s. Use C-c m n to start one." notebook-name)))))
     
     ;; Otherwise, not in a notebook-related buffer
     (t (message "Not in a notebook or IPython buffer")))))

(defun workspace-restart-ipython ()
  "Completely restart the IPython process."
  (interactive)
  (let* ((python-buffer (python-shell-get-buffer))
         (python-process (when python-buffer (get-buffer-process python-buffer))))
    (when python-process
      (message "Restarting IPython process...")
      (delete-process python-process)
      ;; Wait a moment for cleanup then restart
      (run-with-timer 1.0 nil 'run-python))))

(defun workspace-send-buffer-with-restart ()
  "Completely restart IPython and send the entire buffer."
  (interactive)
  (let* ((python-buffer (python-shell-get-buffer))
         (python-process (when python-buffer (get-buffer-process python-buffer))))
    (when python-process
      (message "Restarting IPython process...")
      (delete-process python-process)
      ;; Wait for restart then send buffer
      (run-with-timer 2.0 nil 'python-shell-send-buffer))))

;; Setup keybindings for notebook operations
(defun workspace-setup-notebook-keybindings ()
  "Setup keybindings for notebook operations."
  (local-set-key (kbd "C-c n s") 'workspace-switch-to-notebook-ipython)
  (local-set-key (kbd "C-c n b") 'workspace-send-buffer-with-restart)
  (local-set-key (kbd "C-c n r") 'workspace-restart-ipython))

(defun workspace-reload-vision-environment ()
  "Reload the vision environment without restarting Emacs."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let ((project-root (projectile-project-root)))
      (message "Reloading vision environment...")
      (let ((default-directory project-root))
        ;; Run the nix shell command to rebuild the environment
        (shell-command "nix develop --impure .#vision --command true")
        (message "Vision environment reloaded! You may need to restart Python processes.")))))

;;;###autoload
(defun workspace-generate-proto ()
  "Run nix run .#generate-proto in the project root."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (default-directory project-root))
      (message "Generating proto files...")
      (async-shell-command "nix run .#generate-proto" "*Generate Proto*"))))

;;;###autoload
(defun workspace-decode-proto ()
  "Decode a protobuf message using protoc."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (default-directory project-root)
           (command (read-string "Command to get proto data: "))
           (message-type (read-string "Message type: "))
           (proto-file (read-file-name "Proto definition file: " 
                                      (concat project-root "proto/") 
                                      nil t nil
                                      (lambda (name) (string-suffix-p ".proto" name))))
           (proto-path (file-name-directory proto-file))
           (proto-filename (file-name-nondirectory proto-file)))
      (when (and (not (string-empty-p command))
                 (not (string-empty-p message-type))
                 (file-exists-p proto-file))
        (let ((decode-command (format "%s | protoc --decode=%s --proto_path=%s %s"
                                     command
                                     message-type
                                     (shell-quote-argument proto-path)
                                     (shell-quote-argument proto-filename))))
          (message "Decoding proto with: %s" decode-command)
          (async-shell-command decode-command "*Proto Decode*"))))))

;;;###autoload
(defun workspace-pyright-check ()
  "Run pyright in the vision directory within the vision shell."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (vision-path (concat project-root "vision"))
           (default-directory vision-path))
      (if (file-directory-p vision-path)
          (progn
            (message "Running pyright in vision directory...")
            (async-shell-command "nix develop --impure ../.#vision --command pyright" "*Pyright Check*"))
        (message "Vision directory %s not found" vision-path)))))

;;;###autoload
(defun workspace-format-project ()
  "Format the entire project by running formatters on all directories."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (default-directory project-root)
           (portico-path (concat project-root "portico"))
           (arcade-path (concat project-root "arcade"))
           (vision-path (concat project-root "vision"))
           (formatted-count 0))

      ;; Format portico
      (when (file-directory-p portico-path)
        (message "Running yarn format in portico...")
        (let ((default-directory portico-path))
          (async-shell-command "yarn format" "*Format Portico*"))
        (setq formatted-count (1+ formatted-count)))

      ;; Format arcade
      (when (file-directory-p arcade-path)
        (message "Running cargo fmt in arcade...")
        (let ((default-directory arcade-path))
          (async-shell-command "cargo fmt --all" "*Format Arcade*"))
        (setq formatted-count (1+ formatted-count)))

      ;; Format vision
      (when (file-directory-p vision-path)
        (message "Running pyright in vision...")
        (let ((default-directory vision-path))
          (async-shell-command "nix shell --impure ../.#vision-all --command pyright" "*Format Vision*"))
        (setq formatted-count (1+ formatted-count)))

      (message "Started %d format tasks" formatted-count))))

;;;###autoload
(defun workspace-run-integration-test ()
  "Run integration tests with specified test name and optional --inspect-brk flag."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (default-value (car workspace-integration-test-history))
           (test-name (read-string "Test name: " 
                                  default-value
                                  'workspace-integration-test-history
                                  default-value))
           (use-inspect-brk (y-or-n-p "Add --inspect-brk? "))
           (default-directory project-root))
      (when (not (string-empty-p test-name))
        (let ((command (concat "scripts/integration --silent=false --no-coverage --no-file-parallelism --reporter=basic -t " test-name
                              (if use-inspect-brk " --inspect-brk" ""))))
          (message "Running integration test: %s" command)
          (async-shell-command command "*Integration Test*"))))))

;; Workspace hydra - organized using :column feature
(defhydra hydra-workspace (:color blue :columns 4)
  "Workspace Commands"
  ("s" workspace-setup-prodigy "setup prodigy" :column "Setup")
  ("r" workspace-quick-start "quick start")
  ("m" workspace-start-mock-robots "mock robots")
  ("n" workspace-open-notebook "notebook" :column "Development")
  ("a" workspace-arduino-shell "arduino cli")
  ("i" workspace-run-integration-test "integration test")
  ("y" workspace-yarn-install "yarn install")
  ("g" workspace-generate-proto "generate proto" :column "Code Tools")
  ("d" workspace-decode-proto "decode proto")
  ("p" workspace-pyright-check "pyright check")
  ("f" workspace-format-project "format project")
  ("l" workspace-list-machines-for-system "list machines" :column "Utilities")
  ("k" workspace-shutdown-all "shutdown all")
  ("q" nil "quit" :exit t))

;; Define main keybinding for hydra
(global-set-key (kbd "C-c m") 'hydra-workspace/body)

;;;###autoload
(defun workspace-yarn-install ()
  "Run yarn install in the portico directory."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (portico-path (concat project-root "portico")))
      (if (file-directory-p portico-path)
          (let ((default-directory portico-path))
            (message "Running yarn install in %s" portico-path)
            (async-shell-command "yarn install" "*yarn install*"))
        (message "Portico directory %s not found" portico-path)))))

;;;###autoload
(defun workspace-arduino-shell ()
  "Start a shell in the Arduino CLI environment at ctrl/only directory."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (workspace-name (projectile-project-name))
           (arduino-path (concat project-root "ctrl/only")))
      (if (file-directory-p arduino-path)
          (let ((default-directory arduino-path)
                (shell-buffer-name (format workspace-arduino-shell-buffer-format workspace-name)))
            (message "Starting Arduino CLI shell at %s" arduino-path)
            ;; Start bash shell and send nix-shell command
            (let ((explicit-shell-file-name "bash"))
              (shell shell-buffer-name))
            (run-with-timer 1 nil
                           `(lambda ()
                              (with-current-buffer ,shell-buffer-name
                                (comint-send-string (current-buffer) "nix shell .#arduino-cli\n")
                                (comint-send-string (current-buffer) "cat ../README.md\n"))))
            (switch-to-buffer shell-buffer-name)
            (message "Arduino CLI environment ready. Use arduino-cli commands."))
        (message "Arduino directory %s not found" arduino-path)))))


;; Arcade configuration parsing
(defun workspace-parse-arcades-json ()
  "Parse the arcades.json file and return the JSON data."
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (progn (message "Not in a projectile project") nil)
    (let* ((project-root (projectile-project-root))
           (arcades-json-path (concat project-root "arcade/config/arcades.json")))
      (when (file-exists-p arcades-json-path)
        (with-temp-buffer
          (insert-file-contents arcades-json-path)
          (goto-char (point-min))
          (json-parse-buffer :object-type 'alist :array-type 'list))))))

(defun workspace-get-available-systems ()
  "Get list of unique systems from arcades.json."
  (let ((arcades-data (workspace-parse-arcades-json))
        (systems '()))
    (when arcades-data
      (dolist (arcade arcades-data)
        (let ((system (alist-get 'system arcade)))
          (when (and system 
                     (stringp system)            ; Must be a string
                     (not (string-empty-p system)) ; Not empty string
                     (not (member system systems)))
            (push system systems))))
      ;; Filter out any remaining null-like values and sort
      (sort (seq-filter (lambda (s) (and (stringp s) (not (string-empty-p s)))) systems) 'string<))))

(defun workspace-get-machines-for-system (system-name)
  "Get all machines and their roles for a given system."
  (let ((arcades-data (workspace-parse-arcades-json))
        (machines '()))
    (when arcades-data
      (dolist (arcade arcades-data)
        (let ((system (alist-get 'system arcade))
              (machine (alist-get 'machine arcade))
              (role (alist-get 'role arcade))
              (hostname (alist-get 'hostname arcade)))
          (when (and system 
                     (stringp system)
                     (string-equal system system-name))
            (push (list :machine machine :role role :hostname hostname) machines))))
      machines)))

;;;###autoload
(defun workspace-list-machines-for-system ()
  "List all machines and roles for a selected system in a dedicated buffer."
  (interactive)
  (let* ((available-systems (workspace-get-available-systems))
         (selected-system (if available-systems
                             (completing-read "Select system: " available-systems nil t)
                           (progn (message "No systems found in arcades.json") nil))))
    (when selected-system
      (let ((machines (workspace-get-machines-for-system selected-system))
            (buffer-name (format "*Machines - %s*" selected-system)))
        (if machines
            (with-current-buffer (get-buffer-create buffer-name)
              (erase-buffer)
              (insert (format "Machines for system: %s\n" selected-system))
              (insert (make-string (+ 20 (length selected-system)) ?=))
              (insert "\n\n")
              (dolist (machine machines)
                (insert (format "%-20s %-25s %s\n" 
                               (plist-get machine :machine)
                               (plist-get machine :role)
                               (plist-get machine :hostname))))
              (insert "\n")
              (goto-char (point-min))
              (read-only-mode 1)
              (switch-to-buffer-other-window buffer-name))
          (message "No machines found for system '%s'" selected-system))))))

;;;###autoload
(defun workspace-start-mock-robots ()
  "Start mock robots in an eat buffer."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (available-systems (workspace-get-available-systems))
           (default-robots '("Pisa 11" "Petra 3" "Panama 2")) ; Fixed default order
           (default-robots-string (mapconcat 'identity default-robots ", "))
           (robot-systems (if available-systems
                              ;; Use helm for better multi-selection UX
                              (let ((selected (helm-comp-read (format "Select robot systems (C-SPC to mark, C-RET for defaults [%s]): "
                                                                      (mapconcat 'identity default-robots ", "))
                                                             available-systems
                                                             :marked-candidates t
                                                             :name "Robot Systems"
                                                             :buffer "*helm robot systems*"
                                                             :preselect (car default-robots)
                                                             :default default-robots)))
                                (cond
                                 ;; Empty string - use defaults (this is what C-RET returns)
                                 ((and (stringp selected) (string-empty-p selected))
                                  default-robots)
                                 ;; Empty or nil selection - use defaults
                                 ((or (null selected) (and (listp selected) (= (length selected) 0)))
                                  default-robots)
                                 ;; Single empty string in list - use defaults  
                                 ((and (listp selected) (= (length selected) 1) (string-empty-p (car selected)))
                                  default-robots)
                                 ;; Valid selection
                                 (t selected)))
                            ;; Fallback when no arcades.json available
                            (let ((selected (completing-read-multiple "Enter robot systems (comma-separated): "
                                                                     '("Pisa 11" "Panama 2" "Petra 3" "Petra 4" "Panama 3")
                                                                     nil nil nil nil default-robots-string)))
                              (if (and selected (> (length selected) 0))
                                  selected
                                default-robots))))
           (speedup-input (read-string "Enter speedup (default: 1): "))
           (speedup (if (string-empty-p speedup-input) "1" speedup-input)))
      (when (and robot-systems (> (length robot-systems) 0))
        (let* ((arcade-path (concat project-root "arcade"))
               (robot-args (mapconcat 'shell-quote-argument robot-systems " "))
               (command (format "nix run .#start-arcade -- --mock-standalone --speedup %s %s" speedup robot-args))
               (buffer-name (format workspace-mock-robots-buffer-format (mapconcat 'identity robot-systems ", "))))
          (if (file-exists-p arcade-path)
              (progn
                (require 'eat)
                (let ((default-directory arcade-path))
                  (let ((eat-buffer (eat)))
                    (with-current-buffer eat-buffer
                      (process-send-string (get-buffer-process eat-buffer) (concat command "; exit\n"))
                      (rename-buffer buffer-name))
                    (switch-to-buffer buffer-name)))
                (message "Mock robots started: %s" (mapconcat 'identity robot-systems ", ")))
            (message "Arcade directory %s not found" arcade-path)))))))

;;;###autoload
(defun workspace-shutdown-all ()
  "Shutdown all workspace processes including prodigy services and eat buffers.
Only stops services belonging to the current project."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (workspace-name (if (and (featurep 'projectile) (projectile-project-p))
                              (projectile-project-name)
                            (file-name-nondirectory (directory-file-name project-root))))
           (project-tag (intern (format "project/%s" workspace-name)))
           (stopped-services 0)
           (killed-buffers 0))

      ;; Stop only THIS project's prodigy services
      (when (get-buffer "*prodigy*")
        (dolist (service prodigy-services)
          (when (and (memq project-tag (plist-get service :tags))
                     (prodigy-service-started-p service))
            (prodigy-stop-service service)
            (setq stopped-services (1+ stopped-services)))))
      
      ;; Kill buffers related to THIS workspace only
      ;; Note: *prodigy* buffer is NOT killed since it's shared across projects
      (dolist (buffer (buffer-list))
        (let ((buffer-name (buffer-name buffer)))
          (when (and buffer-name  ; Check buffer-name is not nil
                     (or
                      ;; Mock Robots buffers (contain project-specific system names)
                      (string-match "\\*Mock Robots" buffer-name)
                      ;; Arduino Shell buffer for this workspace
                      (string-equal buffer-name (format workspace-arduino-shell-buffer-format workspace-name))
                      ;; IPython buffers from this workspace's notebooks
                      (and (string-match "\\*IPython\\[.*\\]\\*" buffer-name)
                           (with-current-buffer buffer
                             (and (boundp 'default-directory)
                                  (string-prefix-p project-root default-directory))))))
            (when (buffer-live-p buffer)
              ;; For shell buffers, try to kill the process first
              (when (string-match "\\*Arduino Shell\\*\\|\\*Mock Robots" buffer-name)
                (with-current-buffer buffer
                  (when (and (boundp 'comint-process-echoes)
                            (get-buffer-process buffer))
                    (delete-process (get-buffer-process buffer)))))
              (kill-buffer buffer)
              (setq killed-buffers (1+ killed-buffers))))))
      
      (message "Workspace shutdown complete: stopped %d services, killed %d buffers" 
               stopped-services killed-buffers))))

;;;###autoload
(defun workspace-quick-start ()
  "Quick start: setup prodigy with all services and start mock robots with default settings."
  (interactive)
  (if (not (and (featurep 'projectile) (projectile-project-p)))
      (message "Not in a projectile project")
    (let* ((project-root (projectile-project-root))
           (workspace-name (projectile-project-name))
           (project-tag (intern (format "project/%s" workspace-name))))
      (message "Quick starting workspace: %s" workspace-name)

      ;; Setup prodigy services
      (workspace-setup-prodigy)

      ;; Start only THIS project's prodigy services after a brief delay
      (let ((tag project-tag)
            (name workspace-name))
        (run-with-timer 1 nil
                        (lambda ()
                          (dolist (service prodigy-services)
                            (when (and (memq tag (plist-get service :tags))
                                       (not (prodigy-service-started-p service)))
                              (prodigy-start-service service)))
                          (message "All %s prodigy services started" name))))
      
      ;; Start mock robots with default settings (Pisa 11, Petra 3, Panama 2, speedup 1)
      (let* ((default-robots '("Pisa 11" "Petra 3" "Panama 2"))
             (speedup "1")
             (arcade-path (concat project-root "arcade"))
             (robot-args (mapconcat 'shell-quote-argument default-robots " "))
             (command (format "nix run .#start-arcade -- --mock-standalone --speedup %s %s" speedup robot-args))
             (buffer-name (format workspace-mock-robots-buffer-format (mapconcat 'identity default-robots ", "))))
        (if (file-exists-p arcade-path)
            (progn
              (require 'eat)
              (let ((default-directory arcade-path))
                (let ((eat-buffer (eat)))
                  (with-current-buffer eat-buffer
                    (process-send-string (get-buffer-process eat-buffer) (concat command "; exit\n"))
                    (rename-buffer buffer-name))))
              (message "Mock robots started: %s" (mapconcat 'identity default-robots ", ")))
          (message "Arcade directory %s not found" arcade-path)))
      
      (message "Quick start complete: prodigy services and mock robots running"))))

;;;###autoload
(defun workspace-copy-image-to-clipboard ()
  "Copy the image at point to clipboard using xclip or wl-copy."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq (car image) 'image)
        (let ((data (plist-get (cdr image) ':data))
              (file (plist-get (cdr image) ':file)))
          (cond
           (data
            ;; Image data embedded in buffer
            (with-temp-buffer
              (insert data)
              (cond 
               ((executable-find "wl-copy")
                (call-shell-region (point-min) (point-max)
                                 "wl-copy --type image/png"))
               ((executable-find "xclip")
                (call-shell-region (point-min) (point-max)
                                 "xclip -i -selection clipboard -t image/png"))
               (t (message "No clipboard utility found (need xclip or wl-copy)")))))
           (file
            ;; Image file on disk
            (if (file-exists-p file)
                (cond
                 ((executable-find "wl-copy")
                  (start-process "wl-copy-proc" nil "wl-copy" "--type" "image/png"
                               (file-truename file)))
                 ((executable-find "xclip") 
                  (start-process "xclip-proc" nil "xclip"
                               "-i" "-selection" "clipboard"
                               "-t" "image/png" "-quiet"
                               (file-truename file)))
                 (t (message "No clipboard utility found (need xclip or wl-copy)")))
              (message "Image file %s not found" file)))
           (t (message "The image seems to be malformed."))))
      (message "Point is not at an image."))))

;; Add keybinding for copying images
(defun workspace-setup-image-keybindings ()
  "Setup keybindings for image operations."
  (local-set-key (kbd "C-c i c") 'workspace-copy-image-to-clipboard)
  (local-set-key (kbd "C-c i s") 'image-save))

;; Add to inferior-python-mode-hook
(add-hook 'inferior-python-mode-hook 'workspace-setup-image-keybindings)

(provide 'workspace-utils)
