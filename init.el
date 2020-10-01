;;; init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs
;;; Commentary:
;;;; Code:

;; Performance improvements - LSP in particular benefits greatly.
(setq gc-cons-threshold (* 64 1024 1024)
      read-process-output-max (* 1024 1024)
      inhibit-compacting-font-caches t)

;; Compatibility with older emacs versions
(when (version< emacs-version "27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory))
  (package-initialize))

;; Configure built-in settings
(setq-default frame-title-format "GNU Emacs")

(custom-set-variables
 ;; Introductions
 '(user-full-name "Christopher McCloud")
 '(user-mail-address "mccloud.christopher@gmail.com")
 ;; Set, but don't load, custom file to keep our configuration clean
 '(custom-file (expand-file-name "custom.el" user-emacs-directory))
 ;; Files
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(delete-by-moving-to-trash t)
 '(load-prefer-newer t)
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
 '(fringe-mode 6)
 '(initial-scratch-message nil)
 '(enable-recursive-minibuffers t)
 '(show-paren-delay 0)
 '(lazy-highlight-initial-delay 0)
 ;; Window
 '(window-combination-resize t)
 '(window-divider-default-bottom-width 2)
 '(window-divider-default-right-width 2)
 '(fit-window-to-buffer-horizontally t)
 '(split-height-threshold nil)
 '(fit-frame-to-buffer-margins '(100 100 100 100))
 ;; Isearch
 '(isearch-allow-scroll 'unlimited)
 ;; Adds more flexible matching to isearch
 '(search-whitespace-regexp ".*")
 ;; Apropos
 '(apropos-do-all t)
 ;; Desktop
 '(desktop-load-locked-desktop nil)
 ;; Recentf
 '(recentf-max-saved-items 1000)
 ;; History
 '(history-delete-duplicates t))

;; Default Modes - scroll-bars, tool-bars, and menu-bars disabled in early-init
(show-paren-mode)
(global-visual-line-mode)
(column-number-mode)
(global-hl-line-mode)
(window-divider-mode)
(winner-mode)
(global-auto-revert-mode)
(desktop-save-mode)
(global-so-long-mode)

(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; Setup use-package
(require 'use-package)
(custom-set-variables
 '(use-package-always-defer t)
 '(use-package-minimum-reported-time 0.01)
 '(use-package-expand-minimally nil))

;; Configure Packages
(use-package no-littering
  :demand t)

(use-package bind-key
  :demand t
  :bind*
  (("M-m" . mnemonic-map)
   ("M-u" . undo)
   ("M-o" . other-window))
  :bind
  (("C-x C-c" . ignore)
   ("C-z" . ignore)
   ("M-n" . next-buffer)
   ("M-p" . previous-buffer)
   :map isearch-mode-map
   ("DEL" . isearch-del-char)
   :map mnemonic-map
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
   ("tf" . display-fill-column-indicator-mode)
   ("th" . hl-line-mode)
   ("tH" . global-hl-line-mode)
   ("tl" . display-line-numbers-mode)
   ("tL" . global-display-line-numbers-mode))
  :config
  (unless (bound-and-true-p mnemonic-map)
    (define-prefix-command 'mnemonic-map)))

(use-package m-extras
  :load-path "site-lisp"
  :hook ((emacs-lisp-mode . m-extras-imenu-elisp-extras)
	 (desktop-not-loaded . m-extras-desktop-not-loaded)))

(use-package doom-themes
  :init (load-theme 'doom-one 'no-confirm))

(use-package spacemacs-common
  :custom
  (spacemacs-theme-underline-parens nil)
  (spacemacs-theme-comment-bg t))

(use-package doom-modeline
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

(use-package company
  :custom
  (company-idle-delay .2)
  (company-tooltip-limit 12)
  (company-require-match 'never)
  (company-tooltip-align-annotations t)
  (company-dabbrev-other-buffers t)
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

;; Begin configuration of Helm base packages
(use-package helm
  :custom
  (helm-candidate-number-limit 100)
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
  ;; Helm Buffer List allows for duplicates, if the same buffer is present on
  ;; multiple visible windows. Fix that.
  (define-advice helm-buffers-get-visible-buffers
      (:override (&rest r) remove-duplicates)
    (let (result)
      (walk-windows
       (lambda (x)
	 (cl-pushnew (buffer-name (window-buffer x)) result))
       nil 'visible)
      result))
  ;; As of 27.1, `switch-to-prev-buffer-skip' allows the built-in `next-buffer'
  ;; and `previous-buffer' to skip based on a user defined predicate. We
  ;; want to sync that behavior up with Helm's built in boring buffers.
  (defun helm-boring-buffer-p (_window buffer _bury-or-skip)
    "Return t if buffer is contained in `helm-boring-buffer-regexp-list'."
    (seq-contains-p helm-boring-buffer-regexp-list
		    (buffer-name buffer)
		    #'string-match-p))
  :config
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

(use-package helm-xref
  :demand t
  :after helm)

(use-package helm-project
  :load-path "site-lisp/helm-project"
  :bind (("C-x C-p" . helm-project)
	 ("M-s p" . helm-project-grep-ag)
	 ([remap project-find-regexp] . helm-project-grep-ag)
	 ([remap project-switch-to-buffer] . helm-project-project-buffers)
	 ([remap project-find-files] . helm-project-project-files)
	 :map mnemonic-map
	 ("ps" . helm-project-grep-ag)
	 ("pf" . helm-project-project-files)
	 ("pp" . helm-project)
	 ("pb" . helm-project-source-project-buffers)))

(use-package treemacs
  :custom
  (treemacs-is-never-other-window t)
  :bind (:map mnemonic-map
	      ("tT" . treemacs)
	      ("tt" . treemacs-select-window)
	      :map treemacs-mode-map
	      ("S" . helm-do-grep-ag)
	      ("/" . helm-find))
  :config
  (treemacs-fringe-indicator-mode))

(use-package treemacs-all-the-icons
  :demand t
  :config (treemacs-load-theme 'all-the-icons))

(use-package lispy
  :hook ((lisp-mode . lispy-mode)
	 (emacs-lisp-mode . lispy-mode)
	 (scheme-mode . lispy-mode)
	 (clojure-mode . lispy-mode)
	 (racket-mode . lispy-mode))
  :bind (:map lispy-mode-map
	      ("M-." . nil))
  :config
  (with-eval-after-load 'helm
    (lispy-define-key lispy-mode-map "g" #'helm-imenu)
    (lispy-define-key lispy-mode-map "G" #'helm-do-grep-ag)))

(use-package lsp-mode
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-function 'eldoc-minibuffer-message)
  (lsp-signature-render-documentation nil)
  (lsp-keep-workspace-alive nil)
  (lsp-keymap-prefix "M-m l")
  :hook ((haskell-mode . lsp-deferred)
	 (js-mode . lsp-deferred)
	 (web-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (css-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred))
  :bind (:map mnemonic-map
	      ("ll" . lsp)
	      :map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point)))

(use-package lsp-haskell)

(use-package tree-sitter
  :hook ((js-mode . tree-sitter-hl-mode)
	 (typescript-mode . tree-sitter-hl-mode)
	 (c-mode-common . tree-sitter-hl-mode)
	 (python-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :demand t)

(use-package magit
  :bind
  ("C-x C-g" . magit-status))

(use-package forge
  :after magit
  :demand t)

(use-package transient
  :config
  (transient-bind-q-to-quit))

(use-package diff-hl
  :custom
  (diff-hl-side 'right)
  :hook ((prog-mode . diff-hl-mode)
	 (prog-mode . diff-hl-flydiff-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh)
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)))

(use-package expand-region
  :bind*
  (:map mnemonic-map
	("M-m" . er/expand-region)))

(use-package vterm
  :custom
  (vterm-clear-scrollback-when-clearing t)
  (vterm-buffer-name-string "vterm %s"))

(use-package vterm-toggle
  :bind (:map mnemonic-map
	      ("at" . vterm-toggle-cd))
  :config
  ;; By default, on hiding vterm, vterm-toggle restores the window configuration
  ;; to what it was when vterm was shown. Lets introduce some functionality to
  ;; make that optional, and disable it by default.
  (defcustom vterm-toggle-restore-windows nil
    "Whether to reset window configuration after hiding vterm buffer.")

  (define-advice vterm-toggle-hide
      (:around (oldfun &rest r) preserve-windows)
    (let ((vterm-toggle--window-configration
	   (and vterm-toggle-restore-windows
		vterm-toggle--window-configration)))
      (apply oldfun r)))

  ;; Display vterm below window from which it is called. 
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _)
		   (with-current-buffer bufname (equal major-mode 'vterm-mode)))
		 (display-buffer-reuse-window display-buffer-in-direction)
		 (direction . below)
		 (dedicated . t)
		 (reusable-frames . visible)
		 (window-height . 0.25))))

(use-package markdown
  :mode ("README\\.md" . gfm-mode))

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.ert\\'"
         "\\.mustache\\'"
         "\\.djthml\\'"))

(use-package js
  :hook ((js-mode . electric-pair-mode))
  :bind (:map js-mode-map
	      ("M-." . nil)))

(use-package haskell-mode)
