;;; init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs
;;; Commentary:
;;;; Code:

;; Performance improvements - LSP in particular benefits greatly.
(setq gc-cons-threshold (* 16 1024 1024)
      read-process-output-max (* 1024 1024)
      inhibit-compacting-font-caches t)

;; Compatibility with older emacs versions
(when (version< emacs-version "27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory))
  (package-initialize))

;; Configure built-in settings
(custom-set-variables
 ;; Set, but don't load, custom file to keep our configuration clean
 '(custom-file (expand-file-name "custom.el" user-emacs-directory))
 ;; File Preservation
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(delete-by-moving-to-trash nil)
 '(load-prefer-newer t)
 ;; Use undo-tree's defaults
 '(undo-limit (* 80 1024 1024))
 '(undo-strong-limit (* 120 1024 1024))
 '(undo-outer-limit (* 360 1024 1024))
 ;; Help
 '(help-window-select t)
 ;; NS Settings
 '(ns-use-native-fullscreen nil)
 '(ns-pop-up-frames 'fresh)
 '(ns-command-modifier 'meta)
 '(ns-option-modifier 'super)
 '(ns-use-thin-smoothing t)
 ;; Remote and Interop
 '(ffap-machine-p-known 'reject)
 '(ffap-machine-p-unknown 'reject)
 '(ffap-machine-p-unknown 'reject)
 '(save-interprogram-paste-before-kill t)
 ;; User Interface
 '(fill-column 80)
 '(visible-cursor nil)
 '(cursor-in-non-selected-windows nil)
 '(fringe-mode '(16 . 8))
 '(initial-scratch-message nil)
 '(enable-recursive-minibuffers t)
 '(show-paren-delay 0)
 '(lazy-highlight-initial-delay 0)
 '(message-truncate-lines t)
 '(warning-minimum-level :error)
 '(indent-tabs-mode nil)
 ;; Window
 '(window-combination-resize t)
 '(window-divider-default-bottom-width 2)
 '(window-divider-default-right-width 2)
 '(fit-window-to-buffer-horizontally t)
 ;; Disallow vertical window splits except when called explicitly or through
 ;; specialized entries in `display-buffer-alist'.
 '(split-height-threshold nil)
 ;; Leave some space for the desktop on automatic frame resizing. 
 '(fit-frame-to-buffer-margins '(100 100 100 100))
 ;; Isearch
 '(isearch-allow-scroll 'unlimited)
 '(isearch-wrap-pause 'no)
 '(isearch-lazy-count t)
 ;; Adds more flexible matching to isearch
 '(search-whitespace-regexp ".*")
 ;; Apropos
 '(apropos-do-all t)
 ;; Remove partial completion to avoid performance hit.
 '(completion-styles '(basic emacs22))
 ;; Session persistence
 '(desktop-load-locked-desktop t)
 '(recentf-max-saved-items 1000)
 '(history-delete-duplicates t)
 '(package-native-compile t)
 '(use-short-answers t))

;; Default Modes - tool-bars, scroll-bars, and menu-bars are disabled in
;; early-init as it avoids frame flashing.
(show-paren-mode)
(global-visual-line-mode)
(column-number-mode)
(window-divider-mode)
(winner-mode)
(global-auto-revert-mode)
(desktop-save-mode)
(global-so-long-mode)
(repeat-mode)

;; Ensure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  ;; Assume that if we needed to install use-package, we will also need to
  ;; ensure that our other packages are installed
  (custom-set-variables
   '(use-package-always-ensure t)))

;; Configure use-package
(require 'use-package)
(custom-set-variables
 '(use-package-always-defer t)
 '(use-package-minimum-reported-time 0.001)
 '(use-package-expand-minimally nil)
 '(use-package-verbose nil))

;; Configure Packages
(use-package no-littering
  :demand t)

(use-package doom-themes
  :custom
  (doom-one-brighter-comments t)
  (doom-one-comment-bg nil)
  (doom-one-light-brighter-comments t)
  (doom-zenburn-brighter-comments t)
  :init (load-theme 'doom-one 'no-confirm))

(use-package bind-key
  :demand t
  :bind*
  (("M-m" . mnemonic-map)
   ("M-u" . undo)
   ("M-U" . undo-redo)
   ("M-o" . other-window))
  :bind
  (("C-x C-c" . ignore)
   ("C-z" . ignore)
   ("M-n" . next-buffer)
   ("M-p" . previous-buffer)
   :map isearch-mode-map
   ("DEL" . isearch-del-char)
   :map mnemonic-map
   ("ap" . package-list-packages)
   ("cg" . customize-group)
   ("cf" . customize-apropos-faces)
   ("ca" . customize-apropos)
   ("ws" . split-window-right)
   ("wv" . split-window-below)
   ("wd" . delete-window)
   ("wm" . delete-other-windows)
   ("wb" . balance-windows)
   ("wf" . fit-window-to-buffer)
   ("wq" . kill-buffer-and-window)
   ("wu" . winner-undo)
   ("wr" . winner-redo)
   ("tF" . toggle-frame-fullscreen)
   ("bd" . kill-this-buffer)
   ("br" . rename-buffer)
   ("bq" . kill-buffer-and-window)
   ("qq" . save-buffers-kill-emacs)
   ("qf" . delete-frame)
   ("ad" . dired)
   ("sgg" . rgrep)
   ("ts" . scroll-bar-mode)
   ("tv" . visual-line-mode)
   ("tf" . flymake-mode)
   ("tn" . normal-mode)
   ("th" . hl-line-mode)
   ("tH" . global-hl-line-mode)
   ("tl" . display-line-numbers-mode)
   ("tL" . global-display-line-numbers-mode))
  :config
  (unless (bound-and-true-p mnemonic-map)
    (define-prefix-command 'mnemonic-map)))

(use-package m-extras
  :load-path "site-lisp"
  :hook ((emacs-lisp-mode . m-extras-imenu-elisp-extras))
  :bind (:map mnemonic-map ("wD" . m-extras-dedicate-window)))

(use-package auth-source
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-cache-expiry 86400)
  (epg-pinentry-mode 'loopback))

(use-package desktop
  :hook ((desktop-not-loaded . desktop-save-mode-off)))

(use-package tab-bar
  :custom
  (tab-bar-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :config
  (tab-bar-mode))

(use-package xref
  :custom
  (xref-search-program 'ripgrep)
  (xref-file-name-display 'project-relative)
  (xref-show-definitions-function 'xref-show-definitions-completing-read)
  (xref-show-xrefs-function 'xref-show-definitions-completing-read))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package compile
  :config
  (add-hook 'compilation-finish-functions 'switch-to-buffer-other-window))

(use-package spacemacs-theme
  :custom
  (spacemacs-theme-underline-parens nil)
  (spacemacs-theme-comment-bg t))

(use-package doom-modeline
  :load-path "site-lisp/doom-modeline"
  :init
  (doom-modeline-mode))

(use-package which-key
  :init (which-key-mode)
  :config
  (which-key-add-key-based-replacements
    ;; C-x Map
    "C-x RET" "Encoding"
    "C-x 8" "Char Insertions"
    "C-x @" "Events"
    "C-x X" "Edebug"
    "C-x a" "Abbrev"
    "C-x C-a" "Edebug"
    "C-x 4" "Other Window"
    "C-x 5" "Frames"
    "C-x C-k" "K-Macro"
    "C-x n" "Narrowing"
    "C-x r" "Registers"
    "C-x v" "Version Control"
    "C-x t" "Tabs"
    "C-x p" "Projects"
    ;; C-c Map
    "C-c l" "Layouts"
    "C-c p" "Projects"
    "C-c C-w" "Eyebrowse"
    ;; M-s Map
    "M-s h" "Highlight"
    ;; Mnemonic Map
    "M-m b" "Buffers"
    "M-m f" "Files"
    "M-m a" "Applications"
    "M-m v" "Version Control"
    "M-m h" "Helm"
    "M-m c" "Customization"
    "M-m l" "LSP"
    "M-m l =" "Formatting"
    "M-m l F" "Folders"
    "M-m l G" "Peek"
    "M-m l T" "Toggle"
    "M-m l a" "Code Actions"
    "M-m l g" "Goto"
    "M-m l r" "Refactor"
    "M-m l h" "Help"
    "M-m l s" "Sessions"
    "M-m p" "Projects"
    "M-m q" "Quit"
    "M-m s" "Search"
    "M-m s r" "Ripgrep"
    "M-m s a" "Ag"
    "M-m s g" "Grep/Git-Grep"
    "M-m t" "Toggle"
    "M-m w" "Window"))

(use-package elisp-slime-nav
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d" . elisp-slime-nav-describe-elisp-thing-at-point)
	:map lisp-interaction-mode-map
	("C-c C-d" . elisp-slime-nav-describe-elisp-thing-at-point)))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package company
  :custom
  (company-idle-delay .2)
  (company-tooltip-limit 12)
  (company-require-match 'never)
  (company-tooltip-align-annotations t)
  (company-dabbrev-other-buffers t)
  (company-format-margin-function 'company-vscode-dark-icons-margin)
  :hook ((prog-mode . company-mode))
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-c C-d" . company-show-doc-buffer)
	("C-h" . nil)
        ("RET" . nil)
        ("<return>" . nil)
        ("<tab>" . company-complete-selection)
        ("TAB" . company-complete-selection)))

(use-package yassnippet
  :hook ((web-mode . yas-minor-mode))
  :config
  (yas-global-mode))

;; Begin configuration of Helm base packages
(use-package helm
  :custom
  (helm-candidate-number-limit 100)
  (helm-help-full-frame nil)
  (helm-echo-input-in-header-line t)
  (helm-follow-mode-persistent t)
  (helm-split-window-inside-p t)
  (helm-display-buffer-default-height 20)
  (helm-locate-project-list '("~/Documents/Repos" "~/.emacs.d"))
  (helm-display-header-line nil)
  (helm-buffer-max-length nil)
  (helm-ff-skip-boring-files t)
  (helm-grep-file-path-style 'relative)
  (helm-boring-buffer-regexp-list
   '("\\*helm"
     "\\*Echo Area"
     "\\*Minibuf"
     "eldoc for.*"
     "^\\ .*"
     "^magit.*:"
     "\\*.*-ls.*\\*"))
  (switch-to-prev-buffer-skip 'helm-boring-buffer-p)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-mini)
   ("M-s o" . helm-occur)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info)
   ("C-M-s" . helm-occur)
   :map mnemonic-map
   ("hr" . helm-resume)
   ("hk" . helm-show-kill-ring)
   ("ff" . helm-find-files)
   ("hf" . helm-find)
   ("hl" . helm-locate)
   ("hL" . helm-locate-library)
   ("hm" . helm-man-woman)
   ("hc" . helm-colors)
   :map helm-map
   ("C-z" . helm-select-action)
   ("C-c t" . helm-toggle-full-frame)
   ("<tab>" . helm-execute-persistent-action)
   ("TAB" . helm-execute-persistent-action)
   ("C-M-n" . helm-scroll-other-window)
   ("C-M-p" . helm-scroll-other-window-down))
  :init
  ;; Helm Buffer List allows for duplicate entries, if the same buffer is
  ;; present on multiple visible windows. Fix that.
  (define-advice helm-buffers-get-visible-buffers
      (:override (&rest _r) remove-duplicates)
    (let (result)
      (walk-windows
       (lambda (x)
	 (cl-pushnew (buffer-name (window-buffer x)) result))
       nil 'visible)
      result))
  
  ;; FIX: `helm-read-file-name-handler-1' forced display behavior.
  (define-advice helm-read-file-name-handler-1
      (:override (prompt dir default-filename
                         mustmatch initial predicate
                         name buffer)
		 display-fixes)
    (let ((init (or initial dir default-directory)))
      (helm-read-file-name
       prompt
       :name name
       :history helm-ff-history
       :buffer buffer
       :default default-filename
       :initial-input (expand-file-name init dir)
       :alistp nil
       :must-match mustmatch
       :test predicate)))
  
  ;; As of 27.1, `switch-to-prev-buffer-skip' allows the built-in `next-buffer'
  ;; and `previous-buffer' to skip based on a user defined predicate. We
  ;; want to sync that behavior up with Helm's built in boring buffers.
  (defun helm-boring-buffer-p (_window buffer _bury-or-skip)
    "Return t if buffer is contained in `helm-boring-buffer-regexp-list'."
    (seq-contains-p helm-boring-buffer-regexp-list
		    (buffer-name buffer)
		    #'string-match-p))
  :config
  (custom-set-variables
   '(helm-boring-file-regexp-list
     ;; Treat native-compiled files, and temporary files, as boring
     (append helm-boring-file-regexp-list
	     '("\\.eln$" "\\.tmp$"))))
  (helm-mode)
  (helm-adaptive-mode))

;; Extend helm-imenu to recognize package types, e.g. `use-package'.
(use-package helm-imenu
  :config
  (add-to-list 'helm-imenu-type-faces '("^Package$" . font-lock-type-face)))

;; Begin configuration of Helm addons:
;; We prefer using base Helm, but make exceptions for a few, simple and well
;; maintained packages.
(use-package helm-themes
  :bind (:map mnemonic-map
	      ("ht" . helm-themes)))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-lsp
  :demand t
  :after lsp-mode
  :bind
  (:map lsp-mode-map
	([remap xref-find-apropos] . helm-lsp-workspace-symbol)
	:map mnemonic-map
	("ls" . helm-lsp-workspace-symbol)
	("lS" . helm-lsp-global-workspace-symbol)
	("la" . helm-lsp-code-actions)))

;; project.el has matured enough that it's worth using to manage projects.
;; But bring the interface into helm.
(use-package project
  :custom
  (project-vc-ignores
   '(".yarn/" ".log/" "node_modules/" "*.cache" "*.elc" "*.eln" "*.tmp")))

(use-package helm-project
  :load-path "site-lisp/helm-project"
  :bind (("C-x C-p" . helm-project)
	 ("M-s p" . helm-project-grep-ag)
	 ([remap project-find-regexp] . helm-project-grep-ag)
	 ([remap project-switch-to-buffer] . helm-project-buffers)
	 ([remap project-find-files] . helm-project-files)
	 ([remap project-switch-project] . helm-project-list-projects)
	 :map mnemonic-map
	 ("ps" . helm-project-grep-ag)
	 ("pf" . helm-project-files)
	 ("pp" . helm-project-list-projects)
	 ("pb" . helm-project-buffers)))

(use-package treemacs
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-width 30)
  :bind (:map mnemonic-map
	      ("tT" . treemacs)
	      ("tt" . treemacs-select-window)
	      :map treemacs-mode-map
	      ("S" . helm-do-grep-ag)
	      ("/" . helm-find))
  :config
  (treemacs-fringe-indicator-mode))

(use-package project-treemacs
  :demand t
  :after treemacs
  :load-path "site-lisp/project-treemacs"
  :config
  (customize-set-variable
   'project-treemacs-ignores
   (append project-treemacs-ignores
	   '("*.cache" "*.elc" "*.eln")
	   '("*.yarn/" "*.log/" "node_modules/"))))

(use-package treemacs-all-the-icons
  :demand t
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

(use-package visual-regexp
  :custom (vr/match-separator-use-custom-face t)
  :bind (([remap query-replace] . vr/query-replace)))

(use-package visual-regexp-steroids)

(use-package pcre2el
  :demand t)

(use-package undo-tree
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t)
  :init (global-undo-tree-mode)
  :bind (("M-u" . undo-tree-undo)
	 ("M-U" . undo-tree-redo)))

(use-package wgrep)

(use-package wgrep-helm)

(use-package lispy
  :hook ((lisp-mode . lispy-mode)
	 (emacs-lisp-mode . lispy-mode)
	 (scheme-mode . lispy-mode)
	 (clojure-mode . lispy-mode)
	 (racket-mode . lispy-mode))
  :bind (:map lispy-mode-map
	      ("M-." . nil)
	      ("[" . lispy-brackets))
  
  :config
  (with-eval-after-load 'helm
    (lispy-define-key lispy-mode-map "g" #'helm-imenu)
    (lispy-define-key lispy-mode-map "G" #'helm-do-grep-ag)))

(use-package ace-window
  :bind (:map mnemonic-map
	      ("wS" . ace-swap-window)))

(use-package lsp-mode
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-function 'eldoc-minibuffer-message)
  (lsp-signature-render-documentation nil)
  (lsp-keep-workspace-alive nil)
  (lsp-keymap-prefix "M-m l")
  ;; lsp-deferred waits until the buffer is visible before starting up an lsp
  ;; process, which is a better fit since we use desktop to persist sessions,
  ;; and want to avoid spinning up multiple servers on emacs initialization.
  :hook ((haskell-mode . lsp-deferred)
	 (js-mode . lsp-deferred)
         (js-jsx-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (html-mode . lsp-deferred)
	 (css-mode . lsp-deferred))
  :bind (:map mnemonic-map
	      ("ll" . lsp)
	      :map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point)))

(use-package lsp-ui
  :custom
  ;; Prefer to call documentation explictly, either with mouse hover or through
  ;; `lsp-describe-thing-at-point'.
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-sideline-enable nil))

(use-package lsp-haskell)

(use-package tree-sitter
  ;; Use tree-sitter for syntax highlighting where the tree-sitter highlighting
  ;; is better maintained/more robust than the major-mode font-locking.
  ;; Expect this list to expand dramatically as tree-sitter development continues.
  :hook ((js-mode . tree-sitter-hl-mode)
	 (js2-mode . tree-sitter-hl-mode)
	 (typescript-mode . tree-sitter-hl-mode)
	 (c-mode-common . tree-sitter-hl-mode)
	 (python-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :demand t
  :after tree-sitter)

(use-package magit
  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  :bind
  ("C-x C-v" . magit-status)
  ("C-x g" . magit-status))

(use-package forge
  :demand t
  :after magit)

(use-package transient
  :custom
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (transient-bind-q-to-quit))

(use-package diff-hl
  :custom
  (diff-hl-side 'right)
  :hook ((prog-mode . diff-hl-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)))

(use-package zoom
  :custom (zoom-size '(110 . 36))
  :bind (:map mnemonic-map ("tz" . zoom-mode)))

(use-package expand-region
  :bind*
  (:map mnemonic-map
	("M-m" . er/expand-region)))

;; vterm configuration relies heavily on extending shell configuration, see
;; the homepage for instructions.
(use-package vterm
  :custom
  (vterm-shell "/bin/fish")
  (vterm-clear-scrollback-when-clearing t)
  (vterm-buffer-name-string "vterm %s"))

(use-package vterm-toggle
  :custom
  (vterm-toggle-hide-method 'bury-all-vterm-buffer)
  (vterm-toggle-reset-window-configration-after-exit nil)
  :bind (:map mnemonic-map
	      ("at" . vterm-toggle-cd))
  :config
  ;; Display vterm below window from which it is called.
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _)
		   (with-current-buffer bufname
		     (or (equal major-mode 'vterm-mode)
			 (string-match-p (s-concat "^" vterm-buffer-name)
					 (buffer-name)))))
		 (display-buffer-in-direction)
		 (direction . below)
		 (dedicated . t)
		 (reusable-frames . visible)
		 (window-height . 0.25))))

(use-package elec-pair
  :hook ((typescript-mode . electric-pair-local-mode)
	 (js-mode . electric-pair-local-mode)
	 (web-mode . electric-pair-local-mode)))

(use-package flymake-mode
  :custom
  (flymake-fringe-indicator-position nil))

(use-package markdown
  :mode ("README\\.md" . gfm-mode))

(use-package web-mode
  :custom
  (web-mode-enable-auto-expanding t)
  (web-mode-markup-indent-offset 2)
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.ert\\'"
         "\\.mustache\\'"
         "\\.djthml\\'"))

(use-package js
  :custom
  (js-chain-indent t)
  (js-indent-level 2)
  :bind (:map js-mode-map
	      ("M-." . nil)))

(use-package js2-mode
  :custom
  (js2-missing-semi-one-line-override t)
  (js2-mode-show-strict-warnings nil)
  (js2-mode-show-parse-errors nil)
  :hook ((js-mode . js2-minor-mode)))

(use-package indium
  :custom
  (indium-chrome-executable "google-chrome-stable")
  :hook ((js-mode . indium-interaction-mode))
  :bind (:map indium-interaction-mode-map
	      ("M-m i i" . indium-launch)
	      ("M-m i q" . indium-quit)
	      ("M-m i b" . indium-eval-buffer)
	      ("M-m i r" . indium-eval-region)))

(use-package clojure-mode)

(use-package haskell-mode)

;;; init.el ends here 
