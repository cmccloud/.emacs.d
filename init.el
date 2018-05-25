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

(customize-set-variable 'fit-window-to-buffer-horizontally t)
(customize-set-variable 'window-combination-resize t)
(customize-set-variable 'split-width-threshold 160)
(customize-set-variable 'split-height-threshold 80)
(customize-set-variable 'save-interprogram-paste-before-kill t)

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

(fset #'yes-or-no-p #'y-or-n-p)

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
  (define-prefix-command 'leader-map))

(use-package frame
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :config
  (window-divider-mode t))

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
  (vr/default-replace-preview t)
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

(use-package recentf
  :disabled t
  :functions
  (recentf-track-opened-file)
  :config
  (recentf-mode)
  (recentf-track-opened-file))

(use-package smooth-scrolling
  :disabled t
  :config
  (smooth-scrolling-mode))

(use-package semantic
  :custom
  (semantic-edits-verbose-flag t)
  (semantic-idle-scheduler-idle-time 10)
  (semantic-stickyfunc-indent-string " ")
  (semanticdb-default-save-directory
   (concat user-emacs-directory "cache/semanticdb")))

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
  :bind
  (("C-a" . lispy-move-beginning-of-line)
   ("C-e" . lispy-move-end-of-line)
   :map leader-map
   ("M-m" . lispy-mark-symbol)
   :map lispy-mode-map
   ("C-j" . avy-goto-char-timer)
   ("C-z" . avy-goto-line)
   ("C-0" . lispy-describe-inline)
   ("M-m" . nil)
   ("M-n" . nil)
   (":" . self-insert-command)
   ("[" . lispy-brackets)
   ("]" . self-insert-command)
   ("C-z" . lispy-ace-paren))
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
  
  ;; Until we find a better alternative, use i-menu for tag navigation
  (lispy-define-key lispy-mode-map "g" 'helm-imenu-in-all-buffers)
  (lispy-define-key lispy-mode-map "G" 'helm-imenu)
  (lispy-define-key lispy-mode-map "q" 'lispy-ace-paren-unbounded)
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

(use-package rg
  :custom
  (rg-group-result t)
  :bind
  (:map leader-map
        ("s r r" . rg)
        ("s r p" . rg-project)
        ("s r d" . rg-dwim)
        ("s r l" . rg-literal)
        ("s r s" . rg-list-searches))
  :init
  (rg-enable-default-bindings (kbd "M-s r")))

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

(use-package helm
  :custom
  (helm-candidate-number-limit 100)
  (helm-autoresize-max-height 30)
  (helm-display-header-line nil)
  (helm-split-window-inside-p t)
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
   ("C-M-p" . helm-scroll-other-window-down))
  :config
  ;; TODO - Move this to more general location
  ;; All these definitions should go into an integration package
  ;; For the time being it only affects helm, later might also
  ;; affect purpose
  (defun filtered--change-buffer (pred change-buffer)
    "Call CHANGE-BUFFER until pred returns nil on the current buffer.."
    (let ((initial (current-buffer)))
      (funcall change-buffer)
      (let ((first-change (current-buffer)))
        (catch 'loop
          (while (funcall pred (current-buffer))
            (funcall change-buffer)
            (when (eq (current-buffer) first-change)
              (switch-to-buffer initial)
              (throw 'loop t)))))))

  (defun helm--change-buffer (change-buffer)
    "Call change bufffer until current buffer does not match any pattern in
`helm-boring-buffer-regexp-list'."
    (funcall 'filtered--change-buffer
             (lambda (buffer)
               (--some (string-match it (buffer-name buffer))
                       helm-boring-buffer-regexp-list))
             change-buffer))

  (defun next-file-buffer ()
    "As `next-buffer' but ignores buffers without file names."
    (interactive)
    (filtered--change-buffer
     (lambda (b) (not (buffer-file-name b)))
     #'next-buffer))

  (defun previous-file-buffer ()
    "As `previous-buffer' but ignores buffers without file names."
    (interactive)
    (filtered--change-buffer (lambda (b) (not (buffer-file-name b)))
                             #'previous-buffer))
  
  (defun helm-next-interesting-buffer ()
    "As `next-buffer' but respects `helm-boring-buffer-regexp-list'."
    (interactive)
    (helm--change-buffer 'next-buffer))
  
  (defun helm-previous-interesting-buffer ()
    "As `previous-buffer' but respects `helm-boring-buffer-regexp-list'."
    (interactive)
    (helm--change-buffer 'previous-buffer))
  
  (use-package helm-config :demand t)
  (helm-mode 1))

(use-package helm-command
  :custom
  (helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)))

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
   ("bb" . helm-mini)))

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
   "rg -M 256 --color=always --smart-case --no-heading --line-number %s %s %s"))

(use-package helm-locate
  :bind (:map leader-map
              ("fl" . helm-locate))
  :config
  (when (equal system-type 'darwin)
    (customize-set-variable 'helm-locate-fuzzy-match nil)
    (customize-set-variable 'helm-locate-command "mdfind -name %s %s")))

(use-package helm-elisp
  :bind
  (("C-h a" . helm-apropos)
   :map leader-map
   ("hll" . helm-locate-library)))

(use-package helm-info
  :bind
  (("C-h i" . helm-info)))

(use-package helm-color
  :config
  (helm-attrset 'candidate-number-limit 9999 helm-source-colors)
  (helm-attrset 'candidate-number-limit 9999 helm-source-customize-face))

(use-package helm-imenu
  :config
  (add-to-list 'helm-imenu-type-faces
               '("^Packages$" . font-lock-type-face)))

(use-package helm-projectile
  :bind
  (("C-x C-p" . helm-projectile)
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
  :bind
  (:map leader-map
        ("ss" . helm-do-ag)
        ("sp" . helm-do-ag-project-root)))

(use-package helm-swoop
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
  (defvar +helm-swoop-next-input "")
  
  (defun +helm-swoop-next ()
    "From `helm-swoop', calls `helm-multi-swoop-projectile' or `helm-multi-swoop-all'
Preserves input from `helm-swoop'."
    (interactive)
    (setq +helm-swoop-next-input (minibuffer-contents-no-properties))
    (helm-run-after-exit
     (lambda ()
       (let ((helm-swoop-pre-input-function
              (lambda () +helm-swoop-next-input)))
         (call-interactively (if (and (fboundp 'projectile-project-p)
                                      (projectile-project-p))
                                 #'helm-multi-swoop-projectile
                               #'helm-multi-swoop-all))))))
  
  (defun +helm-multi-swoop-next ()
    "From '`helm-multi-swoop', calls `helm-do-grep-ag'.
Searches from either `projectile-project-root' or `default-directory'.
Preserves input from `helm-multi-swoop'."
    (interactive)
    (when (string= helm-buffer "*Helm Multi Swoop*")
      (setq +helm-swoop-next-input (minibuffer-contents-no-properties))
      (helm-run-after-exit
       (lambda ()
         (minibuffer-with-setup-hook (lambda () (insert +helm-swoop-next-input))
           (let* ((projectile-require-project-root nil)
                  (default-directory (projectile-project-root)))
             (call-interactively #'helm-do-grep-ag)))))))
  
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
   'magit-display-buffer-fullframe-status-v1)
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

(use-package git-gutter
  :disabled t
  :demand t
  :bind (:map leader-map ("gg" . hydra-git-gutter/body))
  :hook ((magit-post-refresh . git-gutter:update-all-windows)
         (focus-in . git-gutter:update-all-windows))
  :config
  (defhydra hydra-git-gutter (:columns 3 :exit nil :foreign-keys warn)
    "Git Gutter"
    ("s" git-gutter:stage-hunk "Stage Hunk")
    ("d" git-gutter:popup-hunk "Diff Hunk" :exit t)
    ("n" git-gutter:next-hunk "Next Hunk")
    ("p" git-gutter:previous-hunk "Previous Hunk")
    ("r" git-gutter:revert-hunk "Revert Hunk")
    ("c" magit-commit-popup "Commit" :color blue )
    ("q" nil "Cancel" :color blue))
  
  (defun +git-gutter:force-select-popup (&optional _diffinfo)
    (pop-to-buffer "*git-gutter:diff*"))

  (advice-add 'git-gutter:popup-hunk :after
              '+git-gutter:force-select-popup)

  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :disabled t
  :demand t
  :config
  ;; colored fringe "bars"
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224
         224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224
         224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

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

(use-package window-numbering
  :bind
  (("M-1" . select-window-1)
   ("M-2" . select-window-2)
   ("M-3" . select-window-3)
   ("M-4" . select-window-4)
   ("M-5" . select-window-5)
   :map leader-map
   ("ws" . split-and-balance-window-right)
   ("wd" . delete-window-and-balance))
  :config
  (defun split-and-balance-window-right ()
    "As 'SPLIT-WINDOW-RIGHT' followed by 'BALANCE-WINDOWS'"
    (interactive)
    (split-window-right)
    (balance-windows))
  
  (defun delete-window-and-balance ()
    "As 'DELETE-WINDOW' followed by 'BALNCE-WINDOWS'"
    (interactive)
    (delete-window)
    (balance-windows))
  
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
  :bind (:map leader-map
              ("tg" . +golden-ratio:toggle))
  :config
  (defun +golden-ratio:toggle ()
    "Toggles `golden-ratio-mode' and balances windows."
    (interactive)
    (if golden-ratio-mode
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (progn
        (golden-ratio-mode 1)
        (golden-ratio))))

  ;; Try to make golden-ratio play nice with other packages
  (defun +golden-ratio-helm-alive-p ()
    (when (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

  (defun +golden-ratio-in-ediff-p ()
    (when (boundp 'ediff-this-buffer-ediff-sessions)
      (symbol-value 'ediff-this-buffer-ediff-sessions)))

  (defun +golden-ratio-company-box-p ()
    (frame-parameter (selected-frame) 'company-box-buffer))
  
  (add-to-list 'golden-ratio-inhibit-functions #'+golden-ratio-helm-alive-p)
  (add-to-list 'golden-ratio-inhibit-functions #'+golden-ratio-in-ediff-p)
  (add-to-list 'golden-ratio-inhibit-functions #'+golden-ratio-company-box-p)
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
          `(("*Process List*" :select t :align ,left-or-below :size 0.4)
            ("*Apropos*" :select t :align ,left-or-below :size 0.4)
            ("Outline.*pdf" :regexp t :select t :align ,left-or-below :size 0.3)
            ("*Geiser documentation*" :select t :align t :size 0.4)
            ("*slime-description*" :select t :align t :size 0.4)
            ("\\`\\*\[h|H]elm.*?\\*\\'" :regexp t :align t :size 0.3)
            ("*Help*" :select t :align below :size 0.4 :popup t)
            ("^\\*helpful.*" :regexp t :select t :align ,left-or-below :size 0.4 :popup t)
            ("*Completions*" :select t :align t :size 0.4)
            ("*Compile-Log*" :select t :align ,left-or-below :size 0.4)
            ("*Man.*" :regexp t :select t :align ,left-or-below :size .5)
            ("*lispy-goto*" :align t :size 0.4)
            ("*git-gutter:diff*" :align ,left-or-below :size 0.4)
            ("*Diff*" :select t :align ,left-or-below :size 0.4)
            ("*Package Commit List*" :select t :align ,left-or-below :size 0.4))))

  (shackle-mode 1))

;;;; Languages
(use-package elisp-mode
  :init
  (when (package-installed-p 'company)
    (defvar emacs-lisp-mode-company-backends
      '(company-elisp company-capf company-semantic)
      "`emacs-lisp-mode' company-backends for use with `company-mode'.")

    (defun emacs-lisp-company-mode ()
      (interactive)
      (set (make-local-variable 'company-backends)
           emacs-lisp-mode-company-backends))

    (add-hook 'emacs-lisp-mode-hook #'emacs-lisp-company-mode)
    (add-hook 'emacs-lisp-mode-hook #'company-mode t)))

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
         "\\.djthml\\'")
  :config
  (define-prefix-command 'web-mode-leader-map)
  (set-keymap-parent web-mode-leader-map leader-map)
  (bind-keys :map web-mode-map ("M-m" . web-mode-leader-map))
  (bind-keys :map web-mode-leader-map
             ("rw" . web-mode-element-wrap)
             ("rc" . web-mode-element-clone)
             ("rr" . web-mode-element-rename)
             ("rk" . web-mode-element-kill)))

(use-package emmet-mode
  :commands (emmet-mode)
  :init
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'emmet-mode))
  (add-hook 'html-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode)
  :config
  (bind-keys :map emmet-mode-keymap
             ("C-j" . nil)))

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
