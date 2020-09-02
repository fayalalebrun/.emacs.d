
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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


(require 'cypher-mode)
(load "~/.emacs.d/n4js")
(setq n4js-cli-program "cypher-shell")

(load "~/.emacs.d/js-doc")




(require 'req-package)



(req-package xelb
  :ensure t
  )


(req-package exwm
  :require xelb
  :ensure t
  :config (load "~/.emacs.d/exwm-conf.el")

	 )

  
(req-package org-download
  :ensure t
  :config(...)
  )

(req-package lua-mode
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

(req-package pdf-tools
  :ensure t
  :config(progn
	   (pdf-tools-install))
  )



(req-package helm
  :ensure t
  :config ( (setq helm-display-function 'helm-display-buffer-in-own-frame
		  helm-display-buffer-reuse-frame t
		  helm-use-undecorated-frame-option t)
	    )
  )


(req-package helm-tramp
  :ensure t
  :config (progn
	    (setq tramp-default-method "ssh")
	    (define-key global-map (kbd "C-c s") 'helm-tramp)
	    )
  )

(req-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config (
	   (setq company-tooltip-align-annotations t)
           (setq company-minimum-prefix-length 1)
	   )
  )


(req-package flycheck
  :ensure t
  :config (...)
  )




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

(req-package js2-mode
  :ensure t
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

(req-package js-doc
  :require js2-mode
  :config (progn
	    (setq js-doc-mail-address "frankxlebrun@gmail.com"
		  js-doc-author (format "Francisco Ayala Le Brun <%s>" js-doc-mail-address)
		  js-doc-url ""
		  js-doc-license "")

	    (add-hook 'js2-mode-hook
		      #'(lambda ()
			  (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
			  (define-key js2-mode-map "@" 'js-doc-insert-tag)))

	    ))


(req-package js2-refactor
  :require js2-mode
  :ensure t
  :config (
	   (add-hook 'js2-mode-hook #'js2-refactor-mode)
	   (js2r-add-keybindings-with-prefix "C-c C-r")
	   (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
	   ))

(req-package ag
  :ensure t
  :config (..))

(req-package xref-js2
  :require ag js2-mode
  :ensure t
  :config (
	   (add-hook 'js2-mode-hook (lambda ()
				      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
	   (define-key js-mode-map (kbd "M-.") nil)
	   ))

(req-package company-tern
  :require company
  :ensure t
  :config (
	   (add-to-list 'company-backends 'company-tern)
	   (add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
	   (define-key tern-mode-keymap (kbd "M-.") nil)
	   (define-key tern-mode-keymap (kbd "M-,") nil)
	   ))


(req-package org :ensure org-plus-contrib :pin org)

(req-package meghanada
  :ensure t
  :config (
	   add-hook 'java-mode-hook
		     (lambda ()
		       ;; meghanada-mode on
		       (meghanada-mode t)
		       (flycheck-mode +1)
		       (setq c-basic-offset 2)
		       ;; use code format
		       (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
	   (cond
	    ((eq system-type 'windows-nt)
	     (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
	     (setq meghanada-maven-path "mvn.cmd"))
	    (t
	     (setq meghanada-java-path "java")
	     (setq meghanada-maven-path "mvn"))
	   ))

(req-package-finish)


(require 'ox-publish)

(setq org-publish-project-alist
      '(

  ("org-fal"
          ;; Path to your org files.
          :base-directory "~/Documents/c-quiche/org"
          :base-extension "org"

          ;; Path to your Jekyll project.
          :publishing-directory "~/Documents/c-quiche/_posts"
          :recursive t
          :publishing-function org-html-publish-to-html
          :headline-levels 4
          :html-extension "html"
          :body-only t ;; Only export section between <body> </body>
    )


    ("org-static-fal"
          :base-directory "~/Documents/c-quiche/org/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
          :publishing-directory "~/Documents/c-quiche/assets"
          :recursive t
          :publishing-function org-publish-attachment)

    ("fal" :components ("org-fal" "org-static-fal"))

    ))

(req-package eglot
  :ensure t
  :hook (company-mode)
  :demand)

(req-package toml-mode
  :ensure t
  :mode "\\.toml\\'"
)

(req-package rust-mode
  :ensure t
  :hook (eglot)
  :config (setq rust-format-on-save t)
)

(req-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
  :hook ((rust-mode toml-mode) . cargo-minor-mode))


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
    (helm-tramp exwm exwm-config xelb flycheck-rust cargo toml-mode company-lsp lsp-ui lsp-mode go-mode rust-mode elpygen ein gdscript-mode js3-mode markdown-preview-mode markdown-mode meghanada yaml-mode org htmlize js2-mode cypher-mode lua-mode nasm-mode org-download neotree cython-mode elpy req-package pdf-tools clang-format glsl-mode ## flycheck-rtags company-rtags helm-rtags flycheck company helm projectile rtags magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
