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
  (add-to-list 'default-frame-alist '(ns-appearance . 'nil))
  (customize-set-variable 'scroll-bar-mode nil)
  (customize-set-variable 'tool-bar-mode nil)
  (customize-set-variable 'frame-resize-pixelwise t)
  (customize-set-variable 'package-user-dir (concat user-emacs-directory "elpa"))
  (package-initialize))

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
(customize-set-variable 'confirm-kill-emacs #'yes-or-no-p)

(customize-set-variable 'blink-matching-paren nil)
(customize-set-variable 'visible-cursor nil)
(customize-set-variable 'cursor-in-non-selected-windows nil)
(customize-set-variable 'highlight-nonselected-windows nil)
(customize-set-variable 'indicate-buffer-boundaries nil)
(customize-set-variable 'indicate-empty-lines nil)
(customize-set-variable 'fringe-mode 4)

(customize-set-variable 'fit-window-to-buffer-horizontally t)
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

(eval-when-compile
  (require 'use-package))

;; Libraries
(use-package dash :demand t :config (dash-enable-font-lock))
(use-package s :demand t)
(use-package hydra :demand t)
(use-package seq)
(use-package map)
(use-package dash-functional)
(use-package request)
(use-package deferred)
(use-package f)

;; Keybinds
(use-package bind-key
  :demand t
  :config
  (define-prefix-command 'leader-map)
  (bind-keys ("M-u" . undo)
             ("C-x C-c" . nil)
             ("M-m" . leader-map)
             ("M-n" . next-buffer)
             ("M-p" . previous-buffer))
  (bind-keys :map leader-map
             ("tF" . toggle-frame-fullscreen)
             ("tl" . display-line-numbers-mode)
             ("bd" . kill-this-buffer)
             ("br" . rename-buffer)
             ("ws" . split-window-right)
             ("wd" . delete-window)
             ("wm" . delete-other-windows)
             ("wv" . split-window-below)
             ("qq" . save-buffers-kill-emacs)
             ("ad" . dired)))

;; Appearance and UI
(use-package frame
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :config
  (window-divider-mode t))

(use-package faces
  :init
  (defun m-font-lock-toggle-bold ()
    "For font-lock faces, toggles weight to either normal or bold."
    (interactive)
    (let* ((old-value (face-attribute 'font-lock-type-face :weight))
           (new-value (if (eq old-value 'normal) 'bold 'normal)))
      (cl-loop for face in (face-list)
               for name = (symbol-name face)
               when (string-match "^font-lock.*" name)
               do (set-face-attribute face nil :weight new-value)))))

(use-package solaire-mode
  :disabled t
  :init
  (defvar solaire-mode--themes-to-swap
    '(doom-one doom-spacegrey doom-solarized-light)
    "List of themes for which background colors must be changed by Solaire.")
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init 
  (load-theme 'spacemacs-dark t)
  (m-font-lock-toggle-bold))

(use-package face-remap
  :commands (text-scale-mode
             text-scale-set
             text-scale-mode-amount))

(use-package doom-modeline
  :demand t
  :load-path "site-lisp/doom-modeline")

(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode))

;; Packages
(use-package diminish
  :config
  (diminish 'visual-line-mode))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order-reverse)
  
  ;; Clear bindings on maps that block which-key paging command (C-h)
  (bind-keys :map help-map
             ("C-h" . nil))            ;help-for-help, also bound to ?
  
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
  (which-key-mode 1))

(use-package paradox
  :custom
  (package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("melpa-stable" . "https://stable.melpa.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("gnu" . "http://elpa.gnu.org/packages/")))
  (package-archive-priorities '(("melpa-stable" . 10)
                                ("melpa" . 5)
                                ("gnu" . 0)
                                ("marmalade" . -5)))
  (package-menu-hide-low-priority nil)
  (paradox-lines-per-entry 2)
  (paradox-use-homepage-buttons nil)
  :config
  (paradox-enable))

(use-package autorevert
  :demand t
  :config
  (setq auto-revert-verbose nil)
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
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode))

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :init
  (customize-set-variable
   'exec-path (eval-when-compile (exec-path-from-shell-initialize) exec-path))

  (setenv "PATH" (eval-when-compile (exec-path-from-shell-initialize) (getenv "PATH")))

  (when-let ((gls (executable-find "gls"))
             (ls (executable-find "ls")))
    (setq insert-directory-program gls
          dired-listing-switches "-aBhlp --group-directories-first")))

(use-package term
  :commands (term
             ansi-term)
  :init
  (bind-keys :map leader-map
             ("at" . ansi-term))
  :config
  (bind-keys :map term-raw-map
             ("M-x" . execute-extended-command)
             ("M-m" . nil))
  (with-eval-after-load 'helm
    (bind-keys :map term-raw-map
               ("M-x" . helm-M-x))))

(use-package eshell
  :commands (eshell)
  :init
  (setenv "NODE_NO_READLINE" "1")
  (bind-keys :map leader-map
             ("ae" . eshell))
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys :map eshell-mode-map
                         ("M-n" . nil)
                         ("M-p" . nil)))))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-reload-all
             yas-minor-mode
             yas-global-mode
             yas-expand)
  :init
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'yas-minor-mode))
  :config (yas-reload-all))

(use-package pdf-tools
  :disabled t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (bind-keys :map pdf-view-mode-map
             ("C-s" . pdf-occur)
             ("k" . nil)
             ("g" . pdf-view-goto-page)
             ("j" . pdf-view-next-line-or-next-page)
             ("k" . pdf-view-previous-line-or-previous-page))
  (bind-keys :map pdf-occur-buffer-mode-map
             ("v" . pdf-occur-view-occurrence)))

(use-package doc-view
  :config
  (bind-keys :map doc-view-mode-map
             ("k" . nil)
             ("n" . doc-view-next-page)
             ("p" . doc-view-previous-page)
             ("w" . doc-view-fit-width-to-window)
             ("h" . doc-view-fit-height-to-window)
             ("s" . doc-view-search)
             ("g" . doc-view-goto-page)))

(use-package eyebrowse
  :commands (eyebrowse-mode
             eyebrowse-switch-to-window-config
             eyebrowse-switch-to-window-config-1
             eyebrowse-switch-to-window-config-2
             eyebrowse-switch-to-window-config-3
             eyebrowse-switch-to-window-config-4
             eyebrowse-switch-to-window-config-5
             eyebrowse-switch-to-window-config-6
             eyebrowse-rename-window-config
             eyebrowse-close-window-config)
  :init
  (bind-keys* ("C-1" . eyebrowse-switch-to-window-config-1)
              ("C-2" . eyebrowse-switch-to-window-config-2)
              ("C-3" . eyebrowse-switch-to-window-config-3)
              ("C-4" . eyebrowse-switch-to-window-config-4)
              ("C-5" . eyebrowse-switch-to-window-config-5)
              ("C-6" . eyebrowse-switch-to-window-config-6))
  
  (bind-keys :map leader-map
             ("l1" . eyebrowse-switch-to-window-config-1)
             ("l2" . eyebrowse-switch-to-window-config-2)
             ("l3" . eyebrowse-switch-to-window-config-3)
             ("l4" . eyebrowse-switch-to-window-config-4)
             ("l5" . eyebrowse-switch-to-window-config-5)
             ("l6" . eyebrowse-switch-to-window-config-6)
             ("lS" . eyebrowse-switch-to-window-config)
             ("lr" . eyebrowse-rename-window-config)
             ("ld" . eyebrowse-close-window-config))
  :config
  ;; Persp-Mode Integration
  (with-eval-after-load 'persp-mode
    (defun spacemacs//get-persp-workspace (&optional persp frame)
      "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
      (let ((param-names (if (display-graphic-p frame)
                             '(gui-eyebrowse-window-configs
                               gui-eyebrowse-current-slot
                               gui-eyebrowse-last-slot)
                           '(term-eyebrowse-window-configs
                             term-eyebrowse-current-slot
                             term-eyebrowse-last-slot))))
        (--map (persp-parameter it persp) param-names)))

    (defun spacemacs//set-persp-workspace (workspace-params &optional persp frame)
      "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.

Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
      (let ((param-names (if (display-graphic-p frame)
                             '(gui-eyebrowse-window-configs
                               gui-eyebrowse-current-slot
                               gui-eyebrowse-last-slot)
                           '(term-eyebrowse-window-configs
                             term-eyebrowse-current-slot
                             term-eyebrowse-last-slot))))
        (--zip-with (set-persp-parameter it other persp)
                    param-names workspace-params)))

    (defun spacemacs/load-eyebrowse-for-perspective (type &optional frame)
      "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
      (when (eq type 'frame)
        (let* ((workspace-params (spacemacs//get-persp-workspace (get-frame-persp frame) frame))
               (window-configs (nth 0 workspace-params))
               (current-slot (nth 1 workspace-params))
               (last-slot (nth 2 workspace-params)))
          (if window-configs
              (progn
                (eyebrowse--set 'window-configs window-configs frame)
                (eyebrowse--set 'current-slot current-slot frame)
                (eyebrowse--set 'last-slot last-slot frame)
                (eyebrowse--load-window-config current-slot))
            (eyebrowse--set 'window-configs nil frame)
            (eyebrowse-init frame)
            (spacemacs/save-eyebrowse-for-perspective frame)))))

    (defun spacemacs/load-eyebrowse-after-loading-layout (_state-file _phash persp-names)
      "Bridge between `persp-after-load-state-functions' and
`spacemacs/load-eyebrowse-for-perspective'.

_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
      (let ((cur-persp (get-current-persp)))
        ;; load eyebrowse for current perspective only if it was one of the loaded
        ;; perspectives
        (when (member (or (and cur-persp (persp-name cur-persp))
                          persp-nil-name)
                      persp-names)
          (spacemacs/load-eyebrowse-for-perspective 'frame))))

    (defun spacemacs/update-eyebrowse-for-perspective (&rest _args)
      "Update and save current frame's eyebrowse workspace to its perspective."
      (let* ((current-slot (eyebrowse--get 'current-slot))
             (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
        (eyebrowse--update-window-config-element
         (eyebrowse--current-window-config current-slot current-tag)))
      (spacemacs/save-eyebrowse-for-perspective))

    (defun spacemacs/save-eyebrowse-for-perspective (&optional frame)
      "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
      (spacemacs//set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                            (eyebrowse--get 'current-slot frame)
                                            (eyebrowse--get 'last-slot frame))
                                      (get-frame-persp frame)
                                      frame))

    (add-hook 'persp-before-switch-functions
              #'spacemacs/update-eyebrowse-for-perspective)
    (add-hook 'eyebrowse-post-window-switch-hook
              #'spacemacs/save-eyebrowse-for-perspective)
    (add-hook 'persp-activated-functions
              #'spacemacs/load-eyebrowse-for-perspective)
    (add-hook 'persp-before-save-state-to-file-functions
              #'spacemacs/update-eyebrowse-for-perspective)
    (add-hook 'persp-after-load-state-functions
              #'spacemacs/load-eyebrowse-after-loading-layout))
  
  (eyebrowse-mode))

(use-package persp-mode
  :commands (persp-mode
             persp-switch
             persp-add-buffer
             persp-remove-buffer)
  :init
  (defvar persp-timed-auto-save-enable t
    "If t, persp-mode will save perspectives to file every
`persp-mode-timed-auto-save-interval seconds. Nil to disable.")
  (defvar persp-timed-auto-save-interval 600
    "Interval, in seconds, between persp-mode auto-saves if
`persp-timed-auto-save-enable is t.")
  (defvar persp--timed-auto-save-handler nil
    "Reference to handler for `persp-timed-auto-save")
  (bind-keys :map leader-map
             ("ll" . persp-mode)
             ("ls" . persp-switch)
             ("la" . persp-add-buffer)
             ("lr" . persp-remove-buffer))
  :config
  (defun persp-timed-auto-save ()
    "Timed auto-save for `persp-mode.
Saves persp-mode layouts every `persp-timed-auto-save-interval seconds.
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
  
  (persp-set-keymap-prefix (kbd "C-c l"))
  (setq persp-nil-name "Home"
        persp-add-buffer-on-find-file t
        persp-add-buffer-on-after-change-major-mode 'free
        persp-reset-windows-on-nil-window-conf t
        persp-restrict-buffers-to-if-foreign-buffer nil
        persp-save-dir (expand-file-name "cache/persp-confs/"
                                         user-emacs-directory)
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-switch-wrap t
        persp-auto-save-opt 2
        persp-autokill-buffer-on-remove nil)
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

(use-package helm-persp
  :load-path "site-lisp/helm-persp"
  :bind
  ("C-x C-l" . helm-persp-layouts))

(use-package osx-trash
  :defer 10
  :if (memq system-type '(osx darwin))
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
  
  (add-hook 'emacs-lisp-mode-hook #'setup--imenu-for-use-package)

  ;; Style packages as requires and imports in helm-imenu
  (with-eval-after-load 'helm-imenu
    (push
     (cons "^Packages$" 'font-lock-type-face)
     helm-imenu-type-faces)))

(use-package elisp-mode
  :init
  (define-prefix-command 'emacs-lisp-mode-leader-map)
  (bind-keys :map emacs-lisp-mode-map
             ("M-m m" . emacs-lisp-mode-leader-map))
  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "M-m m" "Emacs Lisp Mode")
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

(use-package smartparens
  :custom
  (sp-echo-match-when-invisible nil)
  :defines (sp-activate)
  :init
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  (add-hook 'html-mode-hook #'smartparens-mode)
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'smartparens-mode))
  (with-eval-after-load 'markdown-mode
    (add-hook 'markdown-mode-hook #'smartparens-mode))
  :config
  (use-package smartparens-config :demand t)
  (show-smartparens-global-mode))

(use-package expand-region
  :commands (er/expand-region)
  :init
  (bind-keys ("C-r" . er/expand-region)))

(use-package lispy
  :commands (lispy-mode
             lispy-mark-symbol
             lispy-move-beginning-of-line
             lispy-move-end-of-line)
  :init
  (add-hook 'lisp-mode-hook #'lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'scheme-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'racket-mode-hook #'lispy-mode)
  :config
  (bind-keys :map lispy-mode-map
             ("C-j" . avy-goto-char-timer)
             ("C-z" . avy-goto-line)
             ("C-0" . lispy-describe-inline)
             ("M-m" . nil)
             ("M-n" . nil)
             (":" . self-insert-command)
             ("[" . lispy-brackets)
             ("]" . self-insert-command)
             ("C-z" . lispy-ace-paren))

  ;; Until we find a better alternative, use i-menu for tag navigation
  (lispy-define-key lispy-mode-map "g" 'helm-imenu-in-all-buffers)
  (lispy-define-key lispy-mode-map "G" 'helm-imenu)
  (bind-keys :map leader-map
             ("M-m" . lispy-mark-symbol))

  ;; Do everything we can to prevent semantic from killing emacs
  (dolist (command '(lispy-goto
                     lispy-goto-recursive
                     lispy-goto-local
                     lispy-goto-elisp-commands
                     lispy-goto-projectile))
    (fset command #'ignore))
  :bind
  (("C-a" . lispy-move-beginning-of-line)
   ("C-e" . lispy-move-end-of-line)))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :bind
  (("C-c C-d" . elisp-slime-nav-describe-elisp-thing-at-point)))

(use-package clojure-mode)

(use-package clojure-semantic
  :load-path "site-lisp/clojure-semantic")

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-doc-mode))

(use-package intero
  :after (haskell-mode)
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package shm
  :commands (structured-haskell-mode))

(use-package cider
  :commands (cider-mode
             cider--display-interactive-eval-result)
  :init
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package racket-mode
  :commands (racket-mode racket-repl run-racket)
  :config
  (with-eval-after-load 'smartparens
    (add-to-list 'sp--lisp-modes 'racket-mode)
    (sp-local-pair 'racket-mode "'" nil :actions nil)
    (sp-local-pair 'racket-mode "`" nil :actions nil)))

(use-package geiser
  :commands (run-geiser))

(use-package slime
  :commands (slime-mode)
  :init
  (add-hook 'lisp-mode-hook #'slime-mode)
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package oz
  :load-path "/Applications/Mozart2.app/Contents/Resources/share/mozart/elisp"
  :mode (("\\.oz\\'" . oz-mode)
         ("\\.ozg\\'" . oz-gump-mode))
  :init
  (setenv "OZHOME" "/Applications/Mozart2.app/Contents/Resources"))

(use-package markdown-mode
  :mode ("\\.m[k]d" . markdown-mode)
  :config
  (with-eval-after-load 'smartparens
    (sp-local-pair 'markdown-mode "`" nil :actions nil)))

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
  :bind (("M-g d" . dumb-jump-go)
         ("M-g D" . dumb-jump-go-prompt))
  :custom
  (dumb-jump-selector 'helm))

(use-package rg
  :custom
  (rg-group-result t)
  (rg-keymap-prefix (kbd "M-s r"))
  :init
  (rg-enable-default-bindings))

(use-package ag
  :bind (("M-s a a" . ag-regexp)
         ("M-s a p" . ag-project-regexp)
         ("M-s a A" . ag)
         ("M-s a P" . ag-project)
         ("M-s a d" . ag-dired-regexp)
         ("M-s a D" . ag-dired))
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t)
  (ag-ignore-list '("archive-contents")))

(use-package helm
  :commands (helm-M-x
             helm-find-files
             helm-locate
             helm-mini
             helm-projectile
             helm-apropos
             helm-info
             helm-show-kill-ring
             helm-locate-library
             helm-describe-function
             helm-describe-variable
             helm-next-interesting-buffer
             helm-previous-interesting-buffer)
  :diminish helm-mode
  :init
  (defun +helm-show-resume (arg)
    (interactive "P")
    (helm-resume (not arg)))
  
  (bind-keys ("M-x" . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("C-x C-b" . helm-mini)
             ("C-h a" . helm-apropos)
             ("C-h i" . helm-info)
             ("C-h F" . find-function)
             ("M-n" . helm-next-interesting-buffer)
             ("M-p" . helm-previous-interesting-buffer))
  (bind-keys :map leader-map
             ("fl" . helm-locate)
             ("ff" . helm-find-files)
             ("bb" . helm-mini)
             ("hdf" . describe-function)
             ("hdv" . describe-variable)
             ("hk" . helm-show-kill-ring)
             ("hr" . +helm-show-resume)
             ("hll" . helm-locate-library)
             ("hb" . helm-filtered-bookmarks)
             ("hm" . helm-all-mark-rings))
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
  
  (defun helm-next-interesting-buffer ()
    "As `next-buffer' but respects `helm-boring-buffer-regexp-list'."
    (interactive)
    (helm--change-buffer 'next-buffer))
  
  (defun helm-previous-interesting-buffer ()
    "As `previous-buffer' but respects `helm-boring-buffer-regexp-list'."
    (interactive)
    (helm--change-buffer 'previous-buffer))

  (customize-set-variable 'helm-buffer-max-length nil)
  (setq helm-grep-ag-command
        "rg -M 256 --color=always --smart-case --no-heading --line-number %s %s %s"
        helm-M-x-fuzzy-match t
        helm-autoresize-max-height 30
        helm-display-header-line nil
        helm-boring-buffer-regexp-list '("\\` "
                                         "\\*helm"
                                         "\\*helm-mode"
                                         "\\*Echo Area"
                                         "\\*Minibuf"
                                         "\\magit"
                                         "\\*Diff*"
                                         "\\*lispy-goto*"
                                         "\\*Backtrace*")
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-buffer-not-found)
        helm-split-window-in-side-p t
        
        ;; Avoid slow tramp performance when using helm
        helm-buffer-skip-remote-checking t
        helm-ff-tramp-not-fancy t)

  ;; Helm Candidate Number Limit
  (customize-set-variable 'helm-candidate-number-limit 100)
  (with-eval-after-load 'helm-color
    (helm-attrset 'candidate-number-limit 9999 helm-source-colors)
    (helm-attrset 'candidate-number-limit 9999 helm-source-customize-face))
  
  ;; Mac specific config
  (when (equal system-type 'darwin)
    (setq helm-locate-fuzzy-match nil
          helm-locate-command "mdfind -name %s %s"))
  
  (use-package helm-config)
  (helm-mode 1)
  
  (bind-keys :map helm-map
             ("C-z" . helm-select-action)
             ("<tab>" . helm-execute-persistent-action)
             ("TAB" . helm-execute-persistent-action)
             ("C-M-n" . helm-scroll-other-window)
             ("C-M-p" . helm-scroll-other-window-down)))

(use-package helm-projectile
  :after (helm projectile)
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-switch-project
             helm-projectile)
  :init
  (bind-keys ("C-x C-p" . helm-projectile))
  (bind-keys :map leader-map
             ("ps" . helm-projectile-switch-project)
             ("pf" . helm-projectile-find-file)
             ("pp" . helm-projectile)
             ("pb" . helm-projectile-switch-to-buffer))
  :config
  (helm-projectile-on))

(use-package helm-ag
  :after (helm)
  :commands (helm-do-ag
             helm-do-ag-project-root)
  :init
  (bind-keys :map leader-map
             ("ss" . helm-do-ag)
             ("sp" . helm-do-ag-project-root)))

(use-package helm-swoop
  :after (helm)
  :bind
  (("C-s" . helm-swoop))
  :config
  (setq helm-swoop-candidate-number-limit 500
        ;; Bring helm-swoop under shackle control
        helm-swoop-split-window-function 'switch-to-buffer-other-window)
  :custom
  (helm-swoop-speed-or-color t)
  (helm-swoop-split-with-multiple-windows t)
  (helm-swoop-pre-input-function (lambda () "")))

(use-package helm-descbinds
  :after (helm)
  :config
  (setq helm-descbinds-window-style 'split-window)
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
  :commands (projectile-project-root)
  :config
  (projectile-mode))

(use-package stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo)))

(use-package magit
  :custom
  (magit-display-buffer-function
   'magit-display-buffer-fullframe-status-topleft-v1)
  :bind (("C-x g" . magit-status)
         ("C-x C-v" . magit-status)
         :map leader-map
         ("gs" . magit-status)
         ("gb" . magit-blame)
         ("gh" . magit-dispatch-popup)
         :map magit-process-mode-map
         ("M-n" . nil)
         ("M-p" . nil)
         :map magit-diff-mode-map
         ("M-n" . nil)
         ("M-p" . nil)
         :map magit-status-mode-map
         ("M-n" . nil)
         ("M-p" . nil))
  :commands (magit-commit-popup)
  :config
  (defun +magit|refresh-visible-vc-state ()
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (vc-refresh-state))))
  ;; TODO: Needs Testing
  (add-hook 'magit-post-refresh-hook #'+magit|refresh-visible-vc-state))

(use-package magithub
  :disabled t
  :custom
  (magithub-dir
   (concat user-emacs-directory "cache/magithub")))

(use-package magit-gh-pulls
  :after (magit)
  :commands (turn-on-magit-gh-pulls)
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package git-gutter
  :demand t
  :config
  (global-git-gutter-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook
              #'git-gutter:update-all-windows))
  (with-eval-after-load 'hydra
    (defhydra hydra-git-gutter (:columns 3 :exit nil :foreign-keys warn)
      "Git Gutter"
      ("s" git-gutter:stage-hunk "Stage Hunk")
      ("d" git-gutter:popup-hunk "Diff Hunk" :exit t)
      ("n" git-gutter:next-hunk "Next Hunk")
      ("p" git-gutter:previous-hunk "Previous Hunk")
      ("r" git-gutter:revert-hunk "Revert Hunk")
      ("c" magit-commit-popup "Commit" :color blue )
      ("q" nil "Cancel" :color blue))
    (bind-keys :map leader-map
               ("gg" . hydra-git-gutter/body)))
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)
  (defun +git-gutter:force-select-popup (&optional diffinfo)
    (pop-to-buffer "*git-gutter:diff*"))
  (advice-add 'git-gutter:popup-hunk :after
              '+git-gutter:force-select-popup))

(use-package git-gutter-fringe
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

(use-package gist
  :commands (gist-buffer
             gist-buffer-private
             gist-list
             gist-region
             gist-region-or-buffer
             gist-region-or-buffer-private)
  :init
  (bind-keys :map leader-map
             ("gGb" . gist-buffer)
             ("gGl" . gist-list)
             ("gGr" . gist-region)
             ("gGg" . gist-region-or-buffer)))

(use-package diff-hl
  :disabled t
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook
              'diff-hl-magit-post-refresh)))

(use-package company
  :diminish company-mode
  :config
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-c C-d" . company-show-doc-buffer)
             ("RET" . nil)
             ("<return>" . nil)
             ("<tab>" . company-complete-selection)
             ("TAB" . company-complete-selection)))

(use-package company-web
  :after (company))

(use-package company-tern
  :after (company)
  :commands (company-tern))

(use-package slime-company
  :after (company))

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
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

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
  :hook (js2-mode . tern-mode)
  :diminish tern-mode)

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
  :hook (ediff-quit-hook . winner-undo)
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
  
  (golden-ratio-mode t)
  :config
  (add-to-list 'golden-ratio-inhibit-functions #'+golden-ratio-helm-alive-p)
  (add-to-list 'golden-ratio-inhibit-functions #'+golden-ratio-in-ediff-p))


(use-package shackle
  :demand t
  :config
  (defun +shackle-maybe-split (consequent alternative)
    "Determines window split alignment based on frame-width.
Given a large enough frame, splits to consequent. Otherwise splits to alternative.
Valid alignments are `above', `below', `left', and `right'."
    (if (>= (frame-width) 160)
        consequent
      alternative))
  (setq shackle-select-reused-windows t)
  (let ((left-or-below (apply-partially #'+shackle-maybe-split 'left 'below)))
    (setq shackle-rules
          `(("*Process List*" :select t :align ,left-or-below :size 0.4)
            ("*Apropos*" :select t :align ,left-or-below :size 0.4)
            ("Outline.*pdf" :regexp t :select t :align ,left-or-below :size 0.3)
            ("*Geiser documentation*" :select t :align t :size 0.4)
            ("*slime-description*" :select t :align t :size 0.4)
            ("\\`\\*\[h|H]elm.*?\\*\\'" :regexp t :align t :size 0.3)
            ("*Help*" :select t :align ,left-or-below :size 0.4 :popup t)
            ("^\\*helpful.*" :regexp t :select t :align ,left-or-below :size 0.4 :popup t)
            ("*Completions*" :select t :align t :size 0.4)
            ("*Compile-Log*" :select t :align ,left-or-below :size 0.4)
            ("*Man.*" :regexp t :select t :align ,left-or-below :size .5)
            ("*lispy-goto*" :align t :size 0.4)
            ("*git-gutter:diff*" :align ,left-or-below :size 0.4)
            ("*Diff*" :select t :align ,left-or-below :size 0.4)
            ("*Package Commit List*" :select t :align ,left-or-below :size 0.4))))
  (shackle-mode 1))

;; End Emacs Initialization
;; Re-enable Garbage Collection
(setq gc-cons-threshold (* 1024 1024 16)
      gc-cons-percentage .1)

;;; init.el ends here. 
