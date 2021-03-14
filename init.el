
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-truncate-lines)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))






(load "~/.emacs.d/js-doc")




(require 'use-package)


(use-package aweshell
  :bind ([(control return)] . aweshell-dedicated-toggle)
  )

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  )

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
  :bind (("C-x g" . magit-status))	   
)
(global-set-key (kbd "C-x g") 'magit-status)


(use-package pdf-tools
  :ensure t
  )
(pdf-tools-install)


(use-package helm
  :ensure t
  :bind(("M-x" . helm-M-x)
	("C-x r b" . helm-filtered-bookmarks)
	("C-x C-f" . helm-find-files)
	("C-s" . helm-occur)
	("C-x C-b" . helm-buffers-list))
  )


(use-package helm-tramp
  :ensure t
  :bind(("C-c s" . helm-tramp))
  :init(setq tramp-default-method "ssh")
  )




(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :init(setq company-tooltip-align-annotations t)
	(setq company-minimum-prefix-length 1)
  )



(use-package flycheck
  :ensure t
  :config
  (flycheck-define-checker java-checkstyle
    "Java checkstyle"
    :command ("java" "-jar" "checkstyle-8.3.2-all.jar" "-c" "/sun_checks.xml" "-f" "xml" source)
    :error-parser flycheck-parse-checkstyle
    :modes (java-mode))
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
  :hook (eshell-mode-hook . (lambda ()
  (eshell-cmpl-initialize)
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all)
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))
  :ensure t)



(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setq python-shell-interpreter "ipython3"
	python-shell-interpreter-args "-i")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq flycheck-python-flake8-executable "flake8")

  
  )



(use-package py-autopep8
  :ensure t
  :hook (elpy-mode-hook . py-autopep8-enable-on-save)
  )



(use-package cython-mode
  :ensure t
  )

(use-package flycheck-cython
  :ensure t
  :config
  (add-hook 'cython-mode-hook 'flycheck-mode)
  (setq flycheck-cython-executable "cython3")
  (setq flycheck-cython-include-dir "~/Documents/workspace/openage/bin/") 
)



(use-package neotree
  :ensure t
  )





(use-package js-doc
  :ensure t
  :config
  (setq js-doc-mail-address "frankxlebrun@gmail.com"
		  js-doc-author (format "Francisco Ayala Le Brun <%s>" js-doc-mail-address)
		  js-doc-url ""
		  js-doc-license "")

	    (add-hook 'js2-mode-hook
		      #'(lambda ()
			  (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
			  (define-key js2-mode-map "@" 'js-doc-insert-tag)))

  )






(use-package ag
  :ensure t
  )









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
  :demand
  :config (add-to-list 'eglot-server-programs '(vhdl-mode . ("ghdl-ls")))
  :config (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
)

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


(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1)
  )

(use-package ein
  :ensure t
  )


(use-package fal-vhdl
  :bind (:map vhdl-mode-map
         ("C-c C-k" . fal-vhdl-ghdl-ae))
  )

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package acme-mode
  :config
  (add-to-list 'auto-mode-alist '(".acme$" . acme-mode))
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
 '(custom-enabled-themes '(manoj-dark))
 '(ein:output-area-inlined-images t)
 '(highlight-indent-guides-method 'column)
 '(org-export-backends '(ascii html icalendar latex md))
 '(package-selected-packages
   '(typit docker dockerfile-mode mermaid-mode csv-mode auctex web-mode sbt-mode scala-mode highlight-indent-guides volume yasnippet yasnippet-snippets typing helm-tramp exwm exwm-config xelb flycheck-rust cargo toml-mode company-lsp lsp-ui lsp-mode go-mode rust-mode elpygen ein gdscript-mode js3-mode markdown-preview-mode markdown-mode yaml-mode org htmlize js2-mode cypher-mode lua-mode nasm-mode org-download neotree cython-mode elpy use-package pdf-tools clang-format glsl-mode ## flycheck-rtags company-rtags helm-rtags flycheck company helm projectile rtags magit))
 '(safe-local-variable-values
   '((eval let
	   ((root
	     (projectile-project-root)))
	   (let
	       ((command
		 (concat "arm-none-eabi-gdb -i=mi -ex \"target remote localhost:1234\" -ex \"symbol-file " root "src/kernel/build/kernel.sym\"")))
	     (setq-local gud-gud-gdb-command-name command)
	     (setq-local gud-gdb-command-name command)))))
 '(vhdl-upper-case-attributes t)
 '(vhdl-upper-case-keywords t)
 '(vhdl-upper-case-types t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
