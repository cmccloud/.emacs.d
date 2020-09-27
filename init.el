;;; init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs
;;; Commentary:
;;;; Code:

;; Performance improvements
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
 '(scroll-conservatively 101)
 ;; Window
 '(window-combination-resize t)
 '(window-divider-default-bottom-width 2)
 '(window-divider-default-right-width 2)
 '(fit-window-to-buffer-horizontally t)
 '(split-height-threshold nil)
 '(fit-frame-to-buffer-margins '(100 100 100 100))
 ;; Isearch
 '(isearch-allow-scroll 'unlimited)
 '(search-whitespace-regexp ".*")
 ;; Apropos
 '(apropos-do-all t)
 ;; Desktop
 '(desktop-load-locked-desktop nil))

;; Default Modes
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
  (helm-boring-buffer-regexp-list
   '("\\*helm" "\\*Echo Area" "\\*Minibuf" "eldoc for.*" "^\\ .*" "^magit.*:"))
  (switch-to-prev-buffer-skip 'helm-boring-buffer-p)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-mini)
   ("M-s o" . helm-occur)
   ("C-h a" . helm-apropos)
   ("C-h b" . helm-descbinds)
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
   ("ht" . helm-themes)
   :map helm-map
   ("C-z" . helm-select-action)
   ("C-c t" . helm-toggle-full-frame)
   ("<tab>" . helm-execute-persistent-action)
   ("TAB" . helm-execute-persistent-action)
   ("C-M-n" . helm-scroll-other-window)
   ("C-M-p" . helm-scroll-other-window-down))
  :init
  (defun helm-boring-buffer-p (_window buffer _bury-or-skip)
    "Return t if buffer is contained in `helm-boring-buffer-regexp-list'."
    (seq-contains-p helm-boring-buffer-regexp-list
		    (buffer-name buffer)
		    #'string-match-p))
  :config
  (helm-mode))

(use-package helm-themes)

(use-package helm-descbinds)

(use-package helm-xref
  :demand t
  :after helm)

(use-package treemacs
  :custom
  (treemacs-is-never-other-window t)
  :bind (:map mnemonic-map
	      ("tT" . treemacs)
	      ("tt" . treemacs-select-window)))

(use-package treemacs-all-the-icons
  :demand t
  :config (treemacs-load-theme 'all-the-icons))

(use-package lispy
  :hook ((lisp-mode . lispy-mode)
	 (emacs-lisp-mode . lispy-mode)
	 (scheme-mode . lispy-mode)
	 (clojure-mode . lispy-mode)
	 (racket-mode . lispy-mode))
  :bind (:map mnemonic-map
	      ("M-m" . lispy-mark-symbol)
	      :map lispy-mode-map
	      ("M-." . nil))
  :config
  (with-eval-after-load 'helm
    (lispy-define-key lispy-mode-map "g" #'helm-imenu)
    (lispy-define-key lispy-mode-map "G" #'helm-do-grep-ag)))

(use-package lsp
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-function 'eldoc-minibuffer-message)
  (lsp-signature-render-documentation nil)
  :bind (:map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point)
	      :map mnemonic-map
	      ("ll" . lsp)
	      ("la" . lsp-execute-code-action)
	      ("lr" . lsp-rename)))

(use-package magit
  :bind
  ("C-x C-g" . magit-status))

(use-package forge
  :demand t
  :after magit)

(use-package transient
  :config
  (transient-bind-q-to-quit))

(use-package diff-hl
  :custom
  (diff-hl-side 'right)
  :hook ((prog-mode . diff-hl-mode)
	 (prog-mode . diff-hl-flydiff-mode)))

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
