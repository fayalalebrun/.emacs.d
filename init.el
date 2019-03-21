(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-truncate-lines)


(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(req-package))
   (unless (package-installed-p package)
       (package-install package)))

(require 'req-package)

(req-package org-download
  :ensure t
  :config(...)
  )

(req-package eww
  :ensure t
  :config(progn
	   (defun eww-new ()
	     (interactive)
	     (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
	       (switch-to-buffer (generate-new-buffer "eww"))
	       (eww-mode)
	       (eww url)))
	   ))

(req-package magit
  :ensure t
  :config(progn
	   (global-set-key (kbd "C-x g") 'magit-status)
	   ))

(req-package rtags
  :ensure t
  :config
  (progn
    (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
    (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
    (rtags-enable-standard-keybindings)

    (setq rtags-use-helm t)

   ;; Shutdown rdm when leaving emacs.
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
    ))

(req-package helm
  :ensure t
  :config (...)
  )

;; TODO: Has no coloring! How can I get coloring?
(req-package helm-rtags
  :require helm rtags
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (global-set-key (kbd "C-s") 'helm-occur)
    (setq rtags-display-result-backend 'helm)
    ))

(req-package company
  :ensure t
  :config (...)
  )

;; Use rtags for auto-completion.
(req-package company-rtags
  :require company rtags
  :ensure t
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)
    (add-hook 'c++-mode-hook 'company-mode)
    ))

(req-package flycheck
  :ensure t
  :config (...)
  )

;; Live code checking.
(req-package flycheck-rtags
  :require flycheck rtags
  :ensure t
  :config
  (progn
    ;; ensure that we use only rtags checking
    ;; https://github.com/Andersbakken/rtags#optional-1
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
      )
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook 'flycheck-mode)
    ))

(req-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ))

(req-package platformio-mode
  :ensure t
  :require projectile
  :config
  (progn    
    (add-hook 'c++-mode-hook 'platformio-conditionally-enable)
    ))

(req-package glsl-mode
  :ensure t
  :config(progn
	   (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
	   (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
           (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
	   (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
	   ))

(req-package clang-format
  :ensure t
  :config(progn
	   (define-key c-mode-base-map (kbd "C-c u") 'clang-format-buffer)
	   ))

(req-package eshell
  :ensure t
  :config(progn
	   (add-hook 'eshell-mode-hook
	     (lambda ()
               (eshell-cmpl-initialize)
               (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
               (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all)
               (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))
	   (add-to-list 'eshell-visual-commands "zangband" ())
	   (add-to-list 'eshell-visual-commands "tmux" ())
	   ))


(req-package elpy
  :require flycheck
  :ensure t
  :config (progn
	    (elpy-enable)
	    (setq elpy-rpc-python-command "python3")
	    (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
	    (setq python-shell-interpreter "ipython3"
		  python-shell-interpreter-args "-i")
	    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))


	    (add-hook 'elpy-mode-hook 'flycheck-mode)
	    (setq flycheck-python-flake8-executable "flake8")
	    ))


(req-package py-autopep8
  :require elpy
  :ensure t
  :config (progn
	    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
	    ))

(req-package cython-mode
  :ensure t
  :config (...))

(req-package flycheck-cython
  :require flycheck cython-mode
  :ensure t
  :config (progn
	    (add-hook 'cython-mode-hook 'flycheck-mode)
	    (setq flycheck-cython-executable "cython3")
	    (setq flycheck-cython-include-dir "~/Documents/workspace/openage/bin/") 
	    ))

(req-package neotree
  :ensure t
  :config (..))

(req-package-finish)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   (quote
    (org-download neotree cython-mode elpy req-package pdf-tools clang-format glsl-mode ## flycheck-rtags company-rtags helm-rtags flycheck company helm projectile rtags magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
