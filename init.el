
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

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))


(require 'cypher-mode)
(load "~/.emacs.d/n4js")
(setq n4js-cli-program "cypher-shell")

(load "~/.emacs.d/js-doc")




(require 'use-package)



(use-package xelb
  :ensure t
  )


(use-package exwm
  :ensure t
	 )
(load "~/.emacs.d/exwm-conf.el")
  
(use-package org-download
  :ensure t
  )

(use-package lua-mode
  :ensure t
  )

(use-package eww
  :ensure t
  )

(defun eww-new ()
(interactive)
(let ((url (read-from-minibuffer "Enter URL or keywords: ")))
  (switch-to-buffer (generate-new-buffer "eww"))
  (eww-mode)
  (eww url)))

(use-package magit
  :ensure t

	   
)
(global-set-key (kbd "C-x g") 'magit-status)


(use-package pdf-tools
  :ensure t
  )
(pdf-tools-install)


(use-package helm
  :ensure t	     
  )



(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-s") #'helm-occur)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

(use-package helm-tramp
  :ensure t	    
  )

(setq tramp-default-method "ssh")
(define-key global-map (kbd "C-c s") 'helm-tramp)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  )
(setq company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 1)


(use-package flycheck
  :ensure t
  )




(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ))

(use-package platformio-mode
  :ensure t
  :config
  (progn    
    (add-hook 'c++-mode-hook 'platformio-conditionally-enable)
    ))

(use-package glsl-mode
  :ensure t
  :config(progn
	   (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
	   (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
           (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
	   (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
	   ))

(use-package clang-format
  :ensure t
  )


(use-package eshell
  :ensure t)

(add-hook 'eshell-mode-hook (lambda ()
  (eshell-cmpl-initialize)
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all)
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))


(use-package elpy
  :ensure t
  )

(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

(add-hook 'elpy-mode-hook 'flycheck-mode)
(setq flycheck-python-flake8-executable "flake8")


(use-package py-autopep8
  :ensure t
  )

(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(use-package cython-mode
  :ensure t
  )

(use-package flycheck-cython
  :ensure t
)

(add-hook 'cython-mode-hook 'flycheck-mode)
(setq flycheck-cython-executable "cython3")
(setq flycheck-cython-include-dir "~/Documents/workspace/openage/bin/") 

(use-package neotree
  :ensure t
  )

(use-package js2-mode
  :ensure t
)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js-doc
  :ensure t)

(setq js-doc-mail-address "frankxlebrun@gmail.com"
		  js-doc-author (format "Francisco Ayala Le Brun <%s>" js-doc-mail-address)
		  js-doc-url ""
		  js-doc-license "")

	    (add-hook 'js2-mode-hook
		      #'(lambda ()
			  (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
			  (define-key js2-mode-map "@" 'js-doc-insert-tag)))


(use-package js2-refactor
  :ensure t
  )

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(use-package ag
  :ensure t
  )

(use-package xref-js2
  :ensure t
  )

(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(define-key js-mode-map (kbd "M-.") nil)
	   

(use-package company-tern
  :ensure t
  )

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
				      (tern-mode)
				      (company-mode)))
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)






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

(use-package eglot
  :ensure t
  :demand)

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'"
)

(use-package rust-mode
  :ensure t
  :hook (eglot)
  :config (setq rust-format-on-save t)
)

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players)
  )

(use-package shell-pop
  :ensure t
  )




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
    (shell-pop typing helm-tramp exwm exwm-config xelb flycheck-rust cargo toml-mode company-lsp lsp-ui lsp-mode go-mode rust-mode elpygen ein gdscript-mode js3-mode markdown-preview-mode markdown-mode meghanada yaml-mode org htmlize js2-mode cypher-mode lua-mode nasm-mode org-download neotree cython-mode elpy use-package pdf-tools clang-format glsl-mode ## flycheck-rtags company-rtags helm-rtags flycheck company helm projectile rtags magit)))
 '(shell-pop-shell-type
   (quote
    ("terminal" "*terminal*"
     (lambda nil
       (term shell-pop-term-shell)))))
 '(shell-pop-universal-key "<C-return>"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
