;;; workspace-utils-test.el --- Workspace command tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; Load definitions without requiring the interactive package dependencies.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../lisp/workspace-utils.el"
                     (file-name-directory (or load-file-name buffer-file-name))))
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (memq (car-safe form) '(defun defconst defvar))
            (eval form t))))
    (end-of-file nil)))

(ert-deftest workspace-mech-rust-command-pins-toolchain ()
  (should
   (equal (workspace-mech-rust-command "cargo fmt --all")
          "nix shell .#nixpkgs.rust-toolchain --command env RUSTC=rustc cargo fmt --all")))

(ert-deftest workspace-mock-robots-command-quotes-systems ()
  (let* ((systems '("Pisa 11" "robot; echo unsafe" "robot's name"))
         (command (workspace-mock-robots-command systems)))
    (should-not (string-match-p "--speedup" command))
    ;; Replace nix with an argument printer. No Nix or robot process runs.
    (should
     (equal (shell-command-to-string
             (concat "nix() { printf '%s\\n' \"$@\"; }; " command))
            (concat "shell\n.#nixpkgs.rust-toolchain\n--command\nenv\nRUSTC=rustc\n"
                    "nix\nrun\n.#start-arcade\n--\n"
                    (mapconcat #'identity systems "\n") "\n")))))

(ert-deftest workspace-mock-robots-entry-points-use-pinned-command ()
  (provide 'projectile)
  (let ((native-comp-enable-subr-trampolines nil)
        (systems '("Pisa 11" "Petra 3" "Panama 2"))
        commands)
    (with-temp-buffer
      (let ((buffer (current-buffer)))
        (cl-letf (((symbol-function 'projectile-project-p) (lambda () t))
                  ((symbol-function 'projectile-project-root) (lambda () "/tmp/mech/"))
                  ((symbol-function 'projectile-project-name) (lambda () "mech"))
                  ((symbol-function 'workspace-get-available-systems) (lambda () nil))
                  ((symbol-function 'completing-read-multiple) (lambda (&rest _) nil))
                  ((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Unexpected speedup prompt")))
                  ((symbol-function 'file-exists-p) (lambda (_) t))
                  ((symbol-function 'workspace-setup-prodigy) #'ignore)
                  ((symbol-function 'run-with-timer) #'ignore)
                  ((symbol-function 'shell) (lambda (_) buffer))
                  ((symbol-function 'switch-to-buffer) #'ignore)
                  ((symbol-function 'comint-send-string)
                   (lambda (_ command) (push command commands))))
          (workspace-start-mock-robots)
          (workspace-quick-start))))
    (should (equal commands
                   (make-list 2 (concat (workspace-mock-robots-command systems) "\n"))))))

;;; workspace-utils-test.el ends here
