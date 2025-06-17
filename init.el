(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(show-paren-mode 1)
(toggle-truncate-lines)
(setq visible-bell t)
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


(use-package quelpa-use-package
  :ensure t)

(use-package plz
  :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

(use-package ement
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

(use-package aweshell
  :bind ([(control return)] . aweshell-dedicated-toggle)
  )

(use-package typst-ts-mode
  :quelpa (typst-ts-mode :fetcher codeberg :repo "meow_king/typst-ts-mode")
  :mode (("\\.typ\\'" . typst-ts-mode)))

(use-package nix-ts-mode
 :ensure t
 :mode "\\.nix\\'")

(use-package scala-ts-mode
  :ensure t
  :mode "\\.scala\\'")

(use-package typst-preview 
  :quelpa (typst-preview :fetcher github :repo "havarddj/typst-preview.el"))

(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")
		   (typst . ("https://github.com/uben0/tree-sitter-typst"))))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))



(use-package nvm
  :ensure t
  )

(use-package prettier
  :ensure t
  :after web-mode nvm
  :config
  (add-hook 'web-mode-hook
            (lambda ()	      
              (prettier-mode +1)
	      (when (string-match "\.tsx?$" buffer-file-name)
		(setq-local prettier-parsers '(typescript)))
	      )
  )
  )

(use-package xelb
  :ensure t
  )


;; (use-package exwm
;;   :if (and (not (getenv "WAYLAND_DISPLAY")) (display-graphic-p))  
;;   :ensure t  
;;   :config
;;   (load "~/.emacs.d/exwm-conf.el")
;;   )

(use-package eyebrowse
  :ensure t  
  :if (getenv "WAYLAND_DISPLAY")
  :demand
  :bind (:map eyebrowse-mode-map 
	      ("C-c 0" . eyebrowse-switch-to-window-config-0)
	      ("C-c 1" . eyebrowse-switch-to-window-config-1)
	      ("C-c 2" . eyebrowse-switch-to-window-config-2)
	      ("C-c 3" . eyebrowse-switch-to-window-config-3)
	      ("C-c 4" . eyebrowse-switch-to-window-config-4)
	      ("C-c 5" . eyebrowse-switch-to-window-config-5)
	      ("C-c 6" . eyebrowse-switch-to-window-config-6)
	      ("C-c 7" . eyebrowse-switch-to-window-config-7)
	      ("C-c 8" . eyebrowse-switch-to-window-config-8)
	      ("C-c 9" . eyebrowse-switch-to-window-config-9)
	      )
  :config
  (eyebrowse-mode 1)
  (eyebrowse-mode 0)
  (eyebrowse-mode 1)
  (eyebrowse-switch-to-window-config-1)
  (display-time-mode t)
  (display-battery-mode 1)
  )

(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(menu-bar-mode -1)
	))))

;; For the case that the init file runs after the frame has been created.
;; Call of emacs without --daemon option.
(my-frame-tweaks) 
;; For the case that the init file runs before the frame is created.
;; Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)

  
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
  :config
  (pdf-tools-install)
  )


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


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))


(use-package neotree
  :ensure t
  )



(use-package ag
  :ensure t
  )

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'"
)

(use-package rust-mode
  :ensure t
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
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook ((rust-mode . lsp-deferred)
	 (js-mode . lsp-deferred)
	 (web-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (vhdl-mode . lsp-deferred)
	 (typst-ts-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred))
  :commands lsp
  :bind (:map lsp-mode-map (("C-l n" . flycheck-next-error)
			    ("C-l p" . flycheck-previous-error)
			    ("C-l o" . helm-lsp-code-actions)))
  :config
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  (lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection "tinymist")
                      :activation-fn (lsp-activate-on "typst")
                      :server-id 'tinymist))

  (defun my/lsp-format-on-save ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t))

  (add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package vhdl-mode
  :config
  (setq lsp-vhdl-server 'vhdl-ls)
)

(use-package dap-mode
  :ensure t
  :bind (:map dap-mode-map (("C-l d" . dap-hydra)))
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-auto-configure-mode +1)
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                           :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))
  )


(use-package lsp-metals
  :ensure t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook (scala-mode . lsp))

(use-package org-roam
  :init
  (ignore-errors (make-directory "~/org-roam"))
  (ignore-errors (make-directory "~/org-roam/daily"))
  (setq org-roam-v2-ack t)
  :after org company
  :ensure t
  :custom
  (org-roam-directory "~/org-roam")
  :config  
  ;; (org-roam-setup)
  (add-to-list 'company-backends '(company-capf))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package haskell-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))


(use-package separedit
  :ensure t
  :config
  (define-key prog-mode-map        (kbd "C-c '") #'separedit)
  (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
  (define-key help-mode-map        (kbd "C-c '") #'separedit)
  (setq separedit-default-mode 'markdown-mode)
  )

(use-package org
  :ensure t
  :config
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  )

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet")
  (aidermacs-architect-model "openrouter/deepseek/deepseek-r1")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3"
    "DeepSkyBlue" "gray50"])
 '(calendar-date-style 'iso)
 '(connection-local-criteria-alist
   '(((:machine "localhost") localhost-vars)
     ((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((localhost-vars (company-gtags--executable-connection))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
			       (tramp-kubernetes--container
				(car tramp-current-connection))
			       104
			       (tramp-kubernetes--pod
				(car tramp-current-connection))
			       120
			       (tramp-kubernetes--context-namespace
				(car tramp-current-connection))))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "state=abcde" "-o"
					"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (euid . number)
					  (user . string)
					  (egid . number) (comm . 52)
					  (state . 5) (ppid . number)
					  (pgrp . number)
					  (sess . number)
					  (ttname . string)
					  (tpgid . number)
					  (minflt . number)
					  (majflt . number)
					  (time . tramp-ps-time)
					  (pri . number)
					  (nice . number)
					  (vsize . number)
					  (rss . number)
					  (etime . tramp-ps-time)
					  (pcpu . number)
					  (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
					"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "stat=abcde" "-o"
					"ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (user . string)
					  (group . string) (comm . 52)
					  (state . 5) (ppid . number)
					  (pgrp . number)
					  (ttname . string)
					  (time . tramp-ps-time)
					  (nice . number)
					  (etime . tramp-ps-time)
					  (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o"
					"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (euid . number)
					  (user . string)
					  (egid . number)
					  (group . string) (comm . 52)
					  (state . string)
					  (ppid . number)
					  (pgrp . number)
					  (sess . number)
					  (ttname . string)
					  (tpgid . number)
					  (minflt . number)
					  (majflt . number)
					  (time . tramp-ps-time)
					  (pri . number)
					  (nice . number)
					  (vsize . number)
					  (rss . number)
					  (etime . number)
					  (pcpu . number)
					  (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(custom-enabled-themes '(manoj-dark))
 '(ein:output-area-inlined-images t)
 '(eldoc-echo-area-use-multiline-p 5)
 '(eldoc-minor-mode-string nil)
 '(ement-initial-sync-timeout 3000)
 '(ement-room-left-margin-width 12)
 '(ement-room-sender-headers nil)
 '(highlight-indent-guides-method 'column)
 '(js-indent-level 2)
 '(leetcode-prefer-language "rust")
 '(lsp-rust-clippy-preference "on")
 '(org-agenda-files '("~/classes/y2/sp/intro.org"))
 '(org-export-backends '(ascii html icalendar latex md))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-pdf-process
   '("%latex -interaction nonstopmode -output-directory %o %f"
     "%bibtex %b"
     "%latex -interaction nonstopmode -output-directory %o %f"
     "%latex -interaction nonstopmode -output-directory %o %f"))
 '(org-startup-truncated nil)
 '(package-selected-packages nil)
 '(pdf-tools-handle-upgrades nil)
 '(safe-local-variable-directories
   '("/home/fal/Sync/rtlking/" "/home/fal/Default Folder/rtlking/"
     "/home/fal/sources/rtlking/"))
 '(safe-local-variable-values
   '((eval progn (c-set-offset 'innamespace '0)
	   (c-set-offset 'inline-open '0))
     (eval progn (c-set-offset 'case-label '0)
	   (c-set-offset 'innamespace '0)
	   (c-set-offset 'inline-open '0))
     (rust-format-on-save)
     (eval ignore-errors (require 'whitespace) (whitespace-mode 1))
     (whitespace-line-column . 79) (whitespace-style face indentation)
     (eval let ((root (projectile-project-root)))
	   (let
	       ((command
		 (concat
		  "arm-none-eabi-gdb -i=mi -ex \"target remote localhost:1234\" -ex \"symbol-file "
		  root "src/kernel/build/kernel.sym\"")))
	     (setq-local gud-gud-gdb-command-name command)
	     (setq-local gud-gdb-command-name command)
	     (set (make-local-variable 'compile-command)
		  (concat "cd " (concat root "src") " && make test"))
	     (let ((map (make-sparse-keymap)))
	       (set-keymap-parent map (current-local-map))
	       (use-local-map map) (local-set-key [f5] 'compile)
	       (local-set-key [f6] 'co/gdb)
	       (defun co/gdb nil
		 (interactive)
		 (async-shell-command
		  (concat "cd "
			  (concat (projectile-project-root) "src")
			  " && " "make debug")
		  nil 0)
		 (gdb gud-gdb-command-name)))))
     (eval let ((root (projectile-project-root)))
	   (let
	       ((command
		 (concat
		  "arm-none-eabi-gdb -i=mi -ex \"target remote localhost:1234\" -ex \"symbol-file "
		  root "src/kernel/build/kernel.sym\"")))
	     (setq-local gud-gud-gdb-command-name command)
	     (setq-local gud-gdb-command-name command)
	     (set (make-local-variable 'compile-command)
		  (concat "cd " (concat root "src") " && make test"))
	     (use-local-map (copy-keymap (current-local-map)))
	     (local-set-key [f5] 'compile)
	     (local-set-key [f6] 'co/gdb)
	     (defun co/gdb nil
	       (interactive)
	       (async-shell-command
		(concat "cd " (concat (projectile-project-root) "src")
			" && " "make debug")
		nil 0)
	       (gdb gud-gdb-command-name))))
     (eval let ((root (projectile-project-root)))
	   (let
	       ((command
		 (concat
		  "arm-none-eabi-gdb -i=mi -ex \"target remote localhost:1234\" -ex \"symbol-file "
		  root "src/kernel/build/kernel.sym\"")))
	     (setq-local gud-gud-gdb-command-name command)
	     (setq-local gud-gdb-command-name command)))))
 '(vhdl-basic-offset 3)
 '(vhdl-upper-case-attributes t)
 '(vhdl-upper-case-keywords nil)
 '(vhdl-upper-case-types nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
