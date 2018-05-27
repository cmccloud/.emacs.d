;;; init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;; Commentary:
;; Naming conventions:
;;
;; m-...  public variables or functions
;; m--..  private identifiers
;; m:...  interactive function
;; m|...  hook function
;; m*...  advising function
;; +....  as above, but part of a package, e.g. +helm:layouts,
;;        +golden-ratio:toggle, +persp*helm-wrapper
;;

;;; Code: 
;; Suppress Garbage Collection During Initialization
(setq gc-cons-threshold (* 1024 1024 64)
      gc-cons-percentage .6)
;; Begin Emacs Initialization
;; Load Customization Settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

;; Introductions
(customize-set-variable 'user-full-name "Christopher McCloud")
(customize-set-variable 'user-mail-address "mccloud.christopher@gmail.com")

;; In Emacs 27+ this configuration block is contained in early-init.el
(when (version< emacs-version "27.0")
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . nil))
  (customize-set-variable 'scroll-bar-mode nil)
  (customize-set-variable 'tool-bar-mode nil)
  (customize-set-variable 'frame-resize-pixelwise t)
  (customize-set-variable 'package-user-dir (concat user-emacs-directory "elpa"))
  (package-initialize))

;; Customize MacOS settings
(customize-set-variable 'ns-use-native-fullscreen nil)
(customize-set-variable 'ns-pop-up-frames 'fresh)
(customize-set-variable 'ns-command-modifier 'meta)
(customize-set-variable 'ns-option-modifier 'super)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     
(prefer-coding-system        'utf-8)   
(set-terminal-coding-system  'utf-8)   
(set-keyboard-coding-system  'utf-8)   
(set-selection-coding-system 'utf-8)   
(setq-default locale-coding-system   'utf-8
              buffer-file-coding-system 'utf-8)

(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'confirm-kill-emacs #'yes-or-no-p)
(customize-set-variable 'save-interprogram-paste-before-kill t)

;; Don't let find-file-at-point hang emacs with a bad ping attempt
(customize-set-variable 'ffap-machine-p-unknown 'reject)
(customize-set-variable 'ffap-machine-p-known 'reject)
(customize-set-variable 'ffap-machine-p-local 'reject)
;; Set the active region less eagerly. See GNU: #29661 and #29889
(customize-set-variable 'select-active-regions 'only)

(customize-set-variable 'blink-matching-paren nil)
(customize-set-variable 'visible-cursor nil)
(customize-set-variable 'cursor-in-non-selected-windows nil)
(customize-set-variable 'highlight-nonselected-windows nil)
(customize-set-variable 'indicate-buffer-boundaries nil)
(customize-set-variable 'indicate-empty-lines nil)
(customize-set-variable 'fringe-mode 6)

(customize-set-variable 'jit-lock-defer-time nil)
(customize-set-variable 'jit-lock-stealth-nice 0.1)
(customize-set-variable 'jit-lock-stealth-time 0.2)
(customize-set-variable 'jit-lock-stealth-verbose nil)

(customize-set-variable 'compilation-scroll-output nil)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'ring-bell-function #'ignore)
(customize-set-variable 'visible-bell nil)
(customize-set-variable 'apropos-do-all t)
(customize-set-variable 'history-length 1000)

(customize-set-variable 'display-line-numbers-type t)
(customize-set-variable 'display-line-numbers-current-absolute t)

(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'initial-major-mode 'fundamental-mode)
(customize-set-variable 'initial-scratch-message nil)

(advice-add 'yes-or-no-p :override #'y-or-n-p)

(setq-default frame-title-format nil
              fringes-outside-margins t
              bidi-display-reordering nil
              inhibit-compacting-font-caches t
              image-animate-loop t)

;; Use-package
(customize-set-variable 'use-package-always-defer t)
(customize-set-variable 'use-package-verbose nil)
(customize-set-variable 'use-package-minimum-reported-time 0.01)
(eval-when-compile
  (require 'use-package))

;;;; Libraries
(use-package dash :demand t :config (dash-enable-font-lock))
(use-package s :demand t)
(use-package hydra :demand t)
(use-package seq)
(use-package map)
(use-package dash-functional)
(use-package request)
(use-package deferred)
(use-package f)

;;;; Core Packages
(use-package bind-key
  :demand t
  :custom
  (bind-key-describe-special-forms t)
  :bind
  (("M-m" . leader-map)
   ("M-u" . undo)
   ("C-x C-c" . nil)
   ("M-n" . next-buffer)
   ("M-p" . previous-buffer)
   :map leader-map
   ("tF" . toggle-frame-fullscreen)
   ("tl" . display-line-numbers-mode)
   ("bd" . kill-this-buffer)
   ("br" . rename-buffer)
   ("ws" . split-window-right)
   ("wd" . delete-window)
   ("wm" . delete-other-windows)
   ("wv" . split-window-below)
   ("qq" . save-buffers-kill-emacs)
   ("qf" . delete-frame)
   ("ad" . dired))
  :config
  (unless (bound-and-true-p leader-map)
    (define-prefix-command 'leader-map)))

(use-package which-key
  :demand t
  :custom
  (which-key-enable-extended-define-key t)
  (which-key-min-display-lines 2)
  (which-key-sort-order 'which-key-prefix-then-key-order-reverse)
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
    ;; C-c Map
    "C-c l" "Layouts"
    "C-c p" "Projects"
    "C-c C-w" "Eyebrowse"
    ;; M-s Map
    "M-s a" "Ag"
    "M-s r" "Riggrep"
    "M-s h" "Highlight"
    ;; Leader Map
    "M-m b" "Buffers"
    "M-m f" "Files"
    "M-m a" "Applications"
    "M-m g" "Git"
    "M-m gg" "Git Gutter"
    "M-m gG" "Gist"
    "M-m h" "Helm"
    "M-m hd" "Describe"
    "M-m hl" "Locate"
    "M-m l" "Layouts"
    "M-m p" "Projects"
    "M-m q" "Quit"
    "M-m s" "Search"
    "M-m s r" "Ripgrep"
    "M-m t" "Toggle"
    "M-m w" "Window")
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package frame
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :config
  (window-divider-mode t))

(use-package window
  :custom
  (window-combination-limit 'window-size)
  (window-combination-resize t)
  (fit-window-to-buffer-horizontally t)
  (split-width-threshold 160)
  (split-height-threshold 80)
  (split-window-preferred-function 'split-window-sensibly)
  :bind (:map leader-map
              ("ws" . m-split-windows)
              ("wd" . delete-window))
  :init
  (defun m-split-windows ()
    "Splits windows while preserving a large editing space for the first window.

With only one window, behaves as `split-window-right', with multiple windows,
Always splits right from the second window."
    (interactive)
    (let* ((window-combination-limit t)
           (root (frame-root-window))
           (parent (window-parent)))
      ;; In order to maintain the largest layout possible, we never want to
      ;; split our left-most window more than once.  First we determine if
      ;; we are in the left-most window of a 2+ window configuration
      ;; i.e. whether we are in a left-most window that has already been
      ;; split.
      (if (and (equal parent root) (not (window-prev-sibling)))
          ;; If we are, instead of allocating space from our partition of the
          ;; frame, we create a new *left* split from the root, and move to it.
          ;; This preserves the appearance of a right-split.
          (progn (split-window root nil 'left t)
                 (other-window -1))
        ;; Under any other circumstances we simply split normally.
        (split-window-right)))))

(use-package faces
  :init
  (defun m-font-lock-toggle-weight (&optional new-value)
    "For font-lock faces, toggles weight to either normal or bold."
    (interactive)
    (let* ((old-value (face-attribute 'font-lock-type-face :weight))
           (new-value (or new-value (if (eq old-value 'normal) 'bold 'normal))))
      (cl-loop for face in (face-list)
               for name = (symbol-name face)
               when (string-match "^font-lock.*" name)
               do (set-face-attribute face nil :weight new-value)))))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init 
  (load-theme 'spacemacs-dark 'no-confirm)
  (m-font-lock-toggle-weight 'normal))

(use-package doom-modeline
  :demand t
  :load-path "site-lisp/doom-modeline"
  :config
  (doom-modeline-mode 1))

(use-package page-break-lines
  :hook ((emacs-lisp-mode . page-break-lines-mode)
         (lisp-mode . page-break-lines-mode)
         (scheme-mode . page-break-lines-mode)
         (compilation-mode . page-break-lines-mode)
         (outline-mode . page-break-lines-mode)
         (help-mode . page-break-lines-mode)))

(use-package paradox
  :custom
  (package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("melpa-stable" . "https://stable.melpa.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("gnu" . "http://elpa.gnu.org/packages/")))
  (package-archive-priorities '(("melpa" . 10)
                                ("melpa-stable" . 5)
                                ("gnu" . 0)
                                ("marmalade" . -5)))
  (package-menu-hide-low-priority nil)
  (paradox-lines-per-entry 1)
  (paradox-use-homepage-buttons nil)
  :config
  (paradox-enable))

(use-package autorevert
  :demand t
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package re-builder
  :config
  (setq reb-auto-match-limit 500))

(use-package visual-regexp
  :custom
  (vr/default-replace-preview nil)
  :bind (("C-x /" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :demand t
  :custom
  (vr/engine 'pcre2el))

(use-package hl-line
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  :bind
  (:map leader-map
        ("th" . hl-line-mode)
        ("tH" . global-hl-line-mode))
  :hook (prog-mode . hl-line-mode))

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :init
  (eval-when-compile (exec-path-from-shell-initialize))

  (customize-set-variable 'exec-path (eval-when-compile exec-path))

  (setenv "PATH" (eval-when-compile (getenv "PATH")))

  (when-let ((gls (executable-find "gls"))
             (ls (executable-find "ls")))
    (setq insert-directory-program gls
          dired-listing-switches "-aBhlp --group-directories-first")))

(use-package term
  :custom
  (term-suppress-hard-newline t)
  (term-buffer-maximum-size 1024)
  :bind
  (:map leader-map
        ("at" . ansi-term)
        :map term-raw-map
        ("M-x" . helm-M-x)
        ("M-m" . nil)))

(use-package eshell
  :bind (:map leader-map ("ae" . eshell))
  :init (setenv "NODE_NO_READLINE" "1")
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys :map eshell-mode-map
                         ("M-n" . nil)
                         ("M-p" . nil)))))

(use-package yasnippet
  :hook (web-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package pdf-tools
  :disabled t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind
  (:map pdf-view-mode-map
   ("C-s" . pdf-occur)
   ("k" . nil)
   ("g" . pdf-view-goto-page)
   ("j" . pdf-view-next-line-or-next-page)
   ("k" . pdf-view-previous-line-or-previous-page)
   :map pdf-occur-buffer-mode-map
   ("v" . pdf-occur-view-occurrence))
  :config
  (pdf-tools-install))

(use-package doc-view
  :config
  :bind
  (:map doc-view-mode-map
   ("k" . nil)
   ("n" . doc-view-next-page)
   ("p" . doc-view-previous-page)
   ("w" . doc-view-fit-width-to-window)
   ("h" . doc-view-fit-height-to-window)
   ("s" . doc-view-search)
   ("g" . doc-view-goto-page)))

(use-package eyebrowse
  :bind*
  (:map leader-map
        ("l1" . eyebrowse-switch-to-window-config-1)
        ("l2" . eyebrowse-switch-to-window-config-2)
        ("l3" . eyebrowse-switch-to-window-config-3)
        ("l4" . eyebrowse-switch-to-window-config-4)
        ("l5" . eyebrowse-switch-to-window-config-5)
        ("l6" . eyebrowse-switch-to-window-config-6)
        ("lS" . eyebrowse-switch-to-window-config)
        ("lR" . eyebrowse-rename-window-config)
        ("lD" . eyebrowse-close-window-config))
  :config
  (eyebrowse-mode))

(use-package persp-mode
  :custom
  (persp-nil-name "Home")
  (persp-add-buffer-on-after-change-major-mode t)
  (persp-reset-windows-on-nil-window-conf t)
  (persp-restrict-buffers-to-if-foreign-buffer nil)
  (persp-save-dir (expand-file-name "cache/persp-confs/" user-emacs-directory))
  (persp-set-last-persp-for-new-frames t)
  (persp-switch-to-added-buffer nil)
  (persp-switch-wrap t)
  (persp-auto-save-opt 2)
  (persp-autokill-buffer-on-remove nil)
  (persp-keymap-prefix (kbd "C-c l"))
  (persp-auto-resume-time .1)
  (persp-init-frame-behaviour 'persp-ignore-wconf-once)
  :bind
  (:map leader-map
        ("ll" . persp-mode)
        ("ls" . persp-switch)
        ("la" . persp-add-buffer)
        ("lr" . persp-remove-buffer)
        ("lw" . persp-save-state-to-file)
        ("lf" . persp-load-state-from-file))
  :config
  (defvar persp-timed-auto-save-enable t
    "If t, `persp-mode' will save perspectives to file every
`persp-mode-timed-auto-save-interval' seconds.")
  
  (defvar persp-timed-auto-save-interval 600
    "Interval, in seconds, between `persp-mode' auto-saves if
`persp-timed-auto-save-enable' is t.")
  
  (defvar persp--timed-auto-save-handler nil
    "Reference to handler for `persp-timed-auto-save'")
  
  (defun persp-timed-auto-save ()
    "Timed auto-save for `persp-mode'.
Saves layouts every `persp-timed-auto-save-interval' seconds.
Cancels autosave on exiting persp-mode."
    (if (and persp-mode persp-timed-auto-save-enable)
        (progn
          (message "Persp-mode timed auto-save enabled.")
          (setq persp--timed-auto-save-handler
                (run-with-timer
                 persp-timed-auto-save-interval
                 persp-timed-auto-save-interval
                 (lambda ()
                   (if persp-timed-auto-save-enable
                       (progn (message "Saving perspectives to file.")
                              (persp-save-state-to-file))
                     (cancel-timer persp--timed-auto-save-handler)
                     (setq persp--timed-auto-save-handler nil))))))
      (when persp--timed-auto-save-handler
        (cancel-timer persp--timed-auto-save-handler)
        (setq persp--timed-auto-save-handler nil))))

  (defun persp--helm-wrapper (wrapped-buffer-command &rest r)
    "Wrapper for helm-mini for use with `persp-mode'.
Only for use with `advice-add'."
    (with-persp-buffer-list () (apply wrapped-buffer-command r)))
  
  (add-hook 'persp-mode-hook #'persp-timed-auto-save)

  ;; Integrations
  (advice-add 'next-buffer :around
              #'persp--helm-wrapper)
  (advice-add 'previous-buffer :around
              #'persp--helm-wrapper)
  (with-eval-after-load 'helm-buffers
    (advice-add 'helm-mini :around
                #'persp--helm-wrapper))
  (with-eval-after-load 'helm-imenu
    (advice-add 'helm-imenu-in-all-buffers :around
                #'persp--helm-wrapper))
  (with-eval-after-load 'helm-swoop
    (advice-add 'helm-multi-swoop :around
                #'persp--helm-wrapper)
    (advice-add 'helm-multi-swoop-all :around
                #'persp--helm-wrapper)))

(use-package eyebrowse-persp-bridge
  :after (eyebrowse persp-mode)
  :demand t
  :load-path "site-lisp/eyebrowse-persp-bridge")

(use-package osx-trash
  :if (equal system-type 'darwin)
  :init
  (osx-trash-setup))

(use-package semantic
  :custom
  (semantic-elisp-store-documentation-in-tag nil)
  (semantic-edits-verbose-flag nil)
  (semantic-idle-scheduler-idle-time 10)
  (semantic-stickyfunc-indent-string " ")
  (semanticdb-default-save-directory
   (concat user-emacs-directory "cache/semanticdb"))
  (semantic-analyze-summary-function 'semantic-format-tag-summarize)
  :config
  (semantic-default-elisp-setup)
  (setq-mode-local emacs-lisp-mode
                   semanticdb-find-default-throttle
                   (default-value 'semanticdb-find-default-throttle)
                   imenu-create-index-function
                   (default-value 'imenu-create-index-function))
  (semantic-elisp-setup-form-parser
      (lambda (form _start _end)
        (let ((name (nth 1 form)))
          (semantic-tag-new-include
           (symbol-name (if (eq (car-safe name) 'quote)
                            (nth 1 name)
                          name))
           nil)))
    use-package))

(use-package mode-local
  :commands (mode-local-bind))

(use-package imenu
  :defines imenu-generic-expression
  :init
  ;; Let imenu recognize use-package declarations
  (defun setup--imenu-for-use-package ()
    "Recognize `use-package' in imenu when in emacs-lisp-mode."
    (add-to-list
     'imenu-generic-expression
     '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2) t))
  
  (add-hook 'emacs-lisp-mode-hook #'setup--imenu-for-use-package))

(use-package smartparens
  :custom
  (sp-echo-match-when-invisible nil)
  :hook (prog-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode))

(use-package expand-region
  :bind ("C-r" . er/expand-region))

(use-package lispy
  :custom
  (lispy-avy-style-char 'at)
  (lispy-avy-style-symbol 'at)
  (lispy-compat '(edebug cider))
  (lispy-completion-method 'helm)
  (lispy-occur-backend 'helm)
  (lispy-eval-display-style 'overlay)
  (lispy-no-permanent-semantic t)
  (lispy-safe-delete t)
  (lispy-safe-copy t)
  (lispy-safe-paste t)
  :bind
  (("C-a" . lispy-move-beginning-of-line)
   ("C-e" . lispy-move-end-of-line)
   :map leader-map
   ("M-m" . lispy-mark-symbol)
   :map lispy-mode-map
   ("C-j" . avy-goto-char-timer)
   ("C-0" . lispy-describe-inline)
   ("M-m" . nil)
   ("M-n" . nil)
   (":" . self-insert-command)
   ("[" . lispy-brackets)
   ("]" . self-insert-command))
  :hook ((lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (racket-mode . lispy-mode))
  :config
  (defun lispy-ace-paren-unbounded (&optional arg)
    "Jump to any open visible paren on the page.
ARG can constrct the bounds to the current defun."
    (interactive "P")
    (funcall-interactively
     'lispy-ace-paren
     (not arg)))
  
  (lispy-define-key lispy-mode-map "q" 'lispy-ace-paren-unbounded)
  ;; Until we find a better alternative, use i-menu for tag navigation
  (lispy-define-key lispy-mode-map "g" 'helm-imenu-in-all-buffers)
  (lispy-define-key lispy-mode-map "G" 'helm-semantic-or-imenu)
  ;; Do everything we can to prevent semantic from killing emacs
  (dolist (command '(lispy-goto
                     lispy-goto-recursive
                     lispy-goto-local
                     lispy-goto-elisp-commands
                     lispy-goto-projectile))
    (fset command #'ignore)))

(use-package elisp-slime-nav
  :bind (("C-c C-d" . elisp-slime-nav-describe-elisp-thing-at-point)))

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-background t)
  (avy-style 'at)
  :bind
  (("M-g c" . avy-goto-char-timer)
   ("M-g g" . avy-goto-line)
   ("M-g M-g" . avy-goto-line)))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'helm)
  :bind (("M-g d" . dumb-jump-go)
         ("M-g D" . dumb-jump-go-prompt)))

(use-package smart-jump
  :demand t
  :config
  (smart-jump-setup-default-registers))

(use-package rg
  :custom
  (rg-group-result nil)
  :bind
  (:map leader-map
        ("s r r" . rg)
        ("s r p" . rg-project)
        ("s r d" . rg-dwim)
        ("s r l" . rg-literal)
        ("s r s" . rg-list-searches)))

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t)
  (ag-ignore-list '("archive-contents"))
  :bind
  (("M-s a a" . ag-regexp)
   ("M-s a p" . ag-project-regexp)
   ("M-s a A" . ag)
   ("M-s a P" . ag-project)
   ("M-s a d" . ag-dired-regexp)
   ("M-s a D" . ag-dired)))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer nil)
  (wgrep-enable-key (kbd "C-c C-e")))

(use-package helm
  :custom
  (helm-candidate-number-limit 100)
  (helm-autoresize-max-height 30)
  (helm-display-header-line nil)
  (helm-split-window-inside-p t)
  (helm-follow-mode-persistent t)
  :custom-face
  (helm-match ((t (:inherit font-lock-keyword-face :weight bold))))
  :bind
  (("M-n" . next-file-buffer)
   ("M-p" . previous-file-buffer)
   :map leader-map
   ("hdf" . describe-function)
   ("hdv" . describe-variable)
   :map helm-map
   ("C-z" . helm-select-action)
   ("<tab>" . helm-execute-persistent-action)
   ("TAB" . helm-execute-persistent-action)
   ("C-M-n" . helm-scroll-other-window)
   ("C-M-p" . helm-scroll-other-window-down)
   ("C-s" . +helm-into-next))
  :init
  (defvar helm-into-next-alist nil)
  :config
  (defun +helm-into-next ()
    (interactive)
    (when-let ((input helm-input)
               (next-func (cdr (assoc helm-buffer helm-into-next-alist))))
      (with-helm-alive-p
        (helm-run-after-exit next-func input))))
  
  (defun buffer-call-until (pred change-buffer)
    "Call CHANGE-BUFFER until PRED returns t on the current buffer.."
    (let ((initial (current-buffer)))
      (funcall change-buffer)
      (let ((first-change (current-buffer)))
        (catch 'loop
          (while (not (funcall pred (current-buffer)))
            (funcall change-buffer)
            (when (eq (current-buffer) first-change)
              (switch-to-buffer initial)
              (throw 'loop t)))))))

  (defun next-file-buffer ()
    "As `next-buffer' but ignores buffers without file names."
    (interactive)
    (buffer-call-until
     #'buffer-file-name #'next-buffer))

  (defun previous-file-buffer ()
    "As `previous-buffer' but ignores buffers without file names."
    (interactive)
    (buffer-call-until
     #'buffer-file-name #'previous-buffer)))

(use-package helm-config
  :after helm
  :demand t)

(use-package helm-mode
  :after helm
  :demand t
  :custom
  (helm-completion-in-region-fuzzy-match t)
  :config
  (helm-mode 1))

(use-package helm-utils
  :custom
  (helm-window-prefer-horizontal-split t)
  :custom-face
  (helm-match-item ((t (:inherit helm-match :underline t)))))

(use-package helm-command
  :custom
  (helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)))

(use-package helm-adaptive
  :custom
  (helm-adaptive-history-file "~/.emacs.d/cache/helm-adaptive-history")
  :hook (helm-mode . helm-adaptive-mode))

(use-package helm-buffers
  :custom
  (helm-buffer-max-length nil)
  (helm-buffer-skip-remote-checking t)
  (helm-mini-default-sources
   '(helm-source-buffers-list
     helm-source-recentf
     helm-source-buffer-not-found))
  (helm-boring-buffer-regexp-list
   '("\\` "
     "\\*helm"
     "\\*helm-mode"
     "\\*Echo Area"
     "\\*Minibuf"
     "\\magit"
     "\\*Diff*"
     "\\*lispy-goto*"
     "\\*Backtrace*"))
  :bind
  (("C-x C-b" . helm-mini)
   :map leader-map
   ("bb" . helm-mini))
  :commands (helm-buffers-boring-p)
  :config
  (defun helm-buffers-boring-p (buffer)
    (cl-some (lambda (regexp)
               (string-match-p regexp (buffer-name buffer)))
             helm-boring-buffer-regexp-list)))

(use-package helm-files
  :custom
  (helm-ff-tramp-not-fancy t)
  :bind
  (("C-x C-f" . helm-find-files)
   :map leader-map
   ("ff" . helm-find-files)))

(use-package helm-grep
  :custom
  (helm-grep-ag-command
   "rg -M 256 --color=always --smart-case --no-heading --line-number %s %s %s")
  :bind (:map helm-grep-mode-map
              ("RET" . helm-grep-mode-jump-other-window)))

(use-package helm-regexp
  :custom
  (helm-moccur-always-search-in-current t)
  (helm-moccur-auto-update-on-resume 'noask)
  (helm-moccur-show-buffer-fontification nil)
  (helm-moccur-use-ioccur-style-keys nil)
  :bind (("C-s" . helm-occur)
         :map helm-moccur-map
         ("C-o" . helm-goto-next-file)
         ("C-i" . helm-goto-precedent-file))
  :config
  (defun helm-multi-occur-all (&optional input)
    (interactive)
    (let ((buffers (cl-remove-if #'helm-buffers-boring-p (buffer-list))))
      (helm-multi-occur-1 buffers input)))

  (helm-attrset 'candidate-number-limit 500 helm-source-regexp)
  (add-to-list 'helm-into-next-alist
               '("*helm occur*" . helm-multi-occur-all)))

(use-package helm-locate
  :bind (:map leader-map
              ("fl" . helm-locate))
  :config
  (when (equal system-type 'darwin)
    (customize-set-variable 'helm-locate-fuzzy-match nil)
    (customize-set-variable 'helm-locate-command "mdfind -name %s %s")))

(use-package helm-elisp
  :bind (("C-h a" . helm-apropos)
         :map leader-map
         ("hll" . helm-locate-library)))

(use-package helm-info
  :bind (("C-h i" . helm-info)))

(use-package helm-color
  :config
  (helm-attrset 'candidate-number-limit 9999 helm-source-colors)
  (helm-attrset 'candidate-number-limit 9999 helm-source-customize-face))

(use-package helm-imenu
  :custom
  (helm-imenu-fuzzy-match t)
  :bind (("C-x C-j" . helm-imenu-in-all-buffers))
  :config
  (cl-defmethod helm-setup-user-source ((source helm-imenu-source))
    (setf (slot-value source 'candidate-number-limit) 200))
  
  (add-to-list 'helm-imenu-type-faces
               '("^Packages$" . font-lock-type-face)))

(use-package helm-semantic
  :custom
  (helm-semantic-display-style nil))

(use-package helm-ls-git
  :custom
  (helm-ls-git-status-command 'magit-status-internal)
  :bind (("C-x C-d" . helm-browse-project)
         :map helm-ls-git-map
         ("C-s" . helm-ls-git-run-grep)))

(use-package helm-projectile
  :bind (("C-x C-p" . helm-projectile)
         :map leader-map
         ("ps" . helm-projectile-switch-project)
         ("pf" . helm-projectile-find-file)
         ("pp" . helm-projectile)
         ("pb" . helm-projectile-switch-to-buffer))
  :config
  (helm-projectile-on))

(use-package helm-persp
  :load-path "site-lisp/helm-persp"
  :bind
  ("C-x C-l" . helm-persp-layouts))

(use-package helm-ag
  :custom
  (helm-ag-always-set-extra-option nil)
  :bind
  (:map leader-map
        ("ss" . helm-do-ag)
        ("sp" . helm-do-ag-project-root)))

(use-package helm-rg
  :custom
  (helm-rg-default-directory 'default)
  :bind (:map leader-map
              ("sh" . helm-rg)
              ("sH" . helm-projectile-rg))
  :init
  (add-to-list 'helm-into-next-alist
               '("*helm multi occur*" . helm-rg)))

(use-package helm-swoop
  :disabled t
  :custom
  (helm-swoop-speed-or-color t)
  (helm-swoop-split-with-multiple-windows t)
  (helm-swoop-pre-input-function (lambda () ""))
  :bind
  (("C-s" . helm-swoop)
   :map helm-swoop-map
   ("C-s" . +helm-swoop-next)
   :map helm-multi-swoop-map
   ("C-s" . +helm-multi-swoop-next)
   :map helm-map
   ("C-s" . +helm-multi-swoop-next))
  :config
  (defun +helm-swoop-next ()
    "From `helm-swoop', calls `helm-multi-swoop-projectile' or `helm-multi-swoop-all'
Preserves input from `helm-swoop'."
    (interactive)
    (helm-run-after-exit
     (lambda ()
       (if (ignore-errors (projectile-project-p))
           (helm-multi-swoop-projectile helm-input)
         (helm-multi-swoop-all helm-input)))))
  
  (defun +helm-multi-swoop-next ()
    "From '`helm-multi-swoop', calls `helm-do-grep-ag'.
Searches from either `projectile-project-root' or `default-directory'.
Preserves input from `helm-multi-swoop'."
    (interactive)
    (when (string= helm-buffer "*Helm Multi Swoop*")
      (helm-run-after-exit
       (lambda ()
         (minibuffer-with-setup-hook (lambda () (insert helm-input))
           (let ((default-directory
                   (or (ignore-errors (projectile-project-root))
                       default-directory)))
             (helm-do-grep-ag t)))))))
  
  (setq helm-swoop-candidate-number-limit 200
        ;; Bring helm-swoop under shackle control
        helm-swoop-split-window-function 'switch-to-buffer-other-window))

(use-package helm-descbinds
  :custom
  (helm-descbinds-window-style 'split-window)
  :bind
  (("C-h b" . helm-descbinds)))

(use-package helm-themes
  :after (helm))

(use-package helm-dash
  :disabled t
  :commands (helm-dash)
  :after (helm)
  :config
  (bind-keys :map leader-map
             ("hdd" . helm-dash)))

(use-package projectile
  :commands (projectile-project-root projectile-project-p)
  :config
  (add-to-list 'projectile-globally-ignored-directories "semanticdb")
  (projectile-mode))

(use-package stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package magit
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (magit-auto-revert-mode nil)
  :bind
  (("C-x g" . magit-status)
   ("C-x C-v" . magit-status)
   :map leader-map
   ("gs" . magit-status)
   ("gb" . magit-blame)
   ("gh" . magit-dispatch-popup)
   ("gc" . magit-commit-popup)
   :map magit-process-mode-map
   ("M-n" . nil)
   ("M-p" . nil)
   :map magit-diff-mode-map
   ("M-n" . nil)
   ("M-p" . nil)
   :map magit-status-mode-map
   ("M-n" . nil)
   ("M-p" . nil)))

(use-package magithub
  :disabled t
  :custom
  (magithub-dir
   (concat user-emacs-directory "cache/magithub")))

(use-package diff-hl
  :bind (:map leader-map
              ("gg" . hydra-diff-hl/body))
  :hook ((prog-mode . diff-hl-mode)
         (prog-mode . diff-hl-flydiff-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (defhydra hydra-diff-hl (:columns 2 :exit nil :foreign-keys nil)
    "Navigate diffs"
    ("n" diff-hl-next-hunk "Next Hunk")
    ("p" diff-hl-previous-hunk "Previous Hunk")
    ("c" magit-commit-popup "Commit" :exit t)
    ("q" nil "Quit" :exit t)))

(use-package gist
  :bind
  (:map leader-map
        ("gGb" . gist-buffer)
        ("gGl" . gist-list)
        ("gGr" . gist-region)
        ("gGg" . gist-region-or-buffer)))

(use-package company
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-limit 12)
  (company-require-match 'never)
  (company-tooltip-align-annotations t)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-c C-d" . company-show-doc-buffer)
              ("RET" . nil)
              ("<return>" . nil)
              ("<tab>" . company-complete-selection)
              ("TAB" . company-complete-selection)))

(use-package company-box
  :if (display-graphic-p)
  :custom
  (company-box-max-candidates 500)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil)
  (when (package-installed-p 'all-the-icons)
    (cl-flet ((icons 'all-the-icons-material))
      (setq company-box-icons-elisp
            (list
             (icons "functions" :face 'all-the-icons-purple :height .8)
             (icons "check_circle" :face 'all-the-icons-blue :height .8)
             (icons "stars" :face 'all-the-icons-yellow :height .8)
             (icons "format_paint" :face 'all-the-icons-pink :height .8))
            company-box-icons-unknown
            (icons "find_in_page" :face 'all-the-icons-silver :height .8)
            company-box-icons-yasnippet
            (icons "short_text" :face 'all-the-icons-green :height .8)))))

(use-package flycheck
  :config
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector 0 0 0 0 0 0 0 28 62 62 62 28 0 0 0 0 0))
  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)
  :bind
  (:map leader-map
        ("tc" . flycheck-mode)))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package ace-window
  :custom
  (aw-keys '(49 50 51 52 53 54 55 56 57 48))
  :bind ("C-x o" . ace-window))

(use-package window-numbering
  :bind
  (("M-1" . select-window-1)
   ("M-2" . select-window-2)
   ("M-3" . select-window-3)
   ("M-4" . select-window-4)
   ("M-5" . select-window-5))
  :config
  (window-numbering-mode 1))

(use-package winner
  :hook (ediff-quit . winner-undo)
  :bind (:map leader-map
              ("wu" . winner-undo)
              ("wr" . winner-redo))
  :config
  (winner-mode))

(use-package golden-ratio
  :demand t
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-buffer-names '("*Helm Swoop*" "*Helm Multi Swoop*"))
  (golden-ratio-exclude-modes '("magit-popup-mode" "magit-status-mode" "ediff-mode"))
  :bind (:map leader-map ("tg" . golden-ratio-mode))
  :hook (golden-ratio-mode . balance-windows)
  :config
  (defun golden-ratio-helm-alive-p ()
    (ignore-errors helm-alive-p))

  (defun golden-ratio-in-ediff-p ()
    (ignore-errors ediff-this-buffer-ediff-sessions))

  (defun golden-ratio-company-box-p ()
    (frame-parameter (selected-frame) 'company-box-buffer))
  
  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-helm-alive-p)
  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-in-ediff-p)
  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-company-box-p)

  ;; window-numbering
  (cl-loop for num from 0 to 9
           for wnum = (number-to-string num)
           do (add-to-list 'golden-ratio-extra-commands
                           (intern (concat "select-window-" wnum))))
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (golden-ratio-mode t))

(use-package shackle
  :demand t
  :custom
  (shackle-select-reused-windows t)
  :config
  (defun +shackle-maybe-split (consequent alternative)
    "Determines window split alignment based on frame-width.
Given a large enough frame, splits to consequent. Otherwise splits to alternative.
Valid alignments are `above', `below', `left', and `right'."
    (if (>= (frame-width) 160)
        consequent
      alternative))
  
  (defun +shackle-elisp-slime-nav-help (b)
    (and (string= (buffer-name b) "*Help*")
         (eql this-command 'elisp-slime-nav-describe-elisp-thing-at-point)))

  (defun +shackle-helm-help (b)
    (and (string= (buffer-name b) "*Help*")
         helm-alive-p))
  
  (let ((left-or-below (apply-partially #'+shackle-maybe-split 'left 'below)))
    (setq shackle-rules
          `(("*Process List*" :select t :align below :size 0.4)
            ("*Apropos*" :select t :align below :size 0.4)
            ("Outline.*pdf" :regexp t :select t :align below :size 0.3)
            ("*Geiser documentation*" :select t :align t :size 0.4)
            ("*slime-description*" :select t :align t :size 0.4)
            ("\\`\\*\[h|H]elm.*?\\*\\'" :regexp t :align t :size 0.3)
            ("*Help*" :select t :align below :size 0.4 :popup t)
            ("^\\*helpful.*" :regexp t :select t :align below :size 0.4 :popup t)
            ("*Completions*" :select t :align t :size 0.4)
            ("*Compile-Log*" :select t :align below :size 0.4)
            ("*Man.*" :regexp t :select t :align ,left-or-below :size .5)
            ("*lispy-goto*" :align t :size 0.4)
            ("*git-gutter:diff*" :align ,left-or-below :size 0.4)
            ("*Diff*" :select t :align ,left-or-below :size 0.4)
            ("*Package Commit List*" :select t :align ,left-or-below :size 0.4))))

  (shackle-mode 1))

;;;; Languages
(use-package elisp-mode
  :hook (emacs-lisp-mode . company-mode)
  :config
  (setq-mode-local emacs-lisp-mode
                   company-backends
                   '(company-elisp company-capf)))

(use-package slime
  :load-path "elpa/slime-20180519.1327/"
  :init
  ;; slime-autoloads are not recognized by package
  ;; and so are not automatically bundled up when
  ;; using package-quickstart. So we extend our
  ;; load path and require them manually.
  ;; See https://github.com/slime/slime/issues/443
  (require 'slime-autoloads)
  :hook (lisp-mode . slime-mode)
  :defines (inferior-lisp-program)
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :after (company))

(use-package clojure-mode)

(use-package cider
  :commands (cider--display-interactive-eval-result)
  :hook (clojure-mode . cider-mode))

(use-package clojure-semantic
  :load-path "site-lisp/clojure-semantic")

(use-package haskell-mode
  :hook (haskell-mode . haskell-doc-mode))

(use-package intero
  :hook (haskell-mode . intero-mode))

(use-package shm)

(use-package racket-mode)

(use-package geiser)

(use-package oz
  :load-path "/Applications/Mozart2.app/Contents/Resources/share/mozart/elisp"
  :mode (("\\.oz\\'" . oz-mode)
         ("\\.ozg\\'" . oz-gump-mode))
  :init
  (setenv "OZHOME" "/Applications/Mozart2.app/Contents/Resources"))

(use-package markdown-mode
  :mode ("\\.m[k]d" . markdown-mode))

(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.ert\\'"
         "\\.mustache\\'"
         "\\.djthml\\'"))

(use-package company-web
  :after (company))

(use-package js2-mode
  :mode "\\.js$"
  :init
  (when (package-installed-p 'company)
    (defvar js2-mode-company-backends
      '(company-tern company-capf)
      "`js2-mode' company-backends for use with `company-mode'.")

    (defun js2-company-mode ()
      (interactive)
      (set (make-local-variable 'company-backends)
           js2-mode-company-backends))

    (add-hook 'js2-mode-hook #'js2-company-mode)
    (add-hook 'js2-mode-hook #'company-mode t)))

(use-package tern
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :after (company)
  :commands (company-tern))

;; End Emacs Initialization
;; Re-enable Garbage Collection
(setq gc-cons-threshold (* 1024 1024 16)
      gc-cons-percentage .1)

;;; init.el ends here. 
