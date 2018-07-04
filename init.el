;;; init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;;; Code:
;;** Emacs Settings and Customization
;; Suppress Garbage Collection
(setq gc-cons-threshold (* 1024 1024 64)
      gc-cons-percentage .6)

;; Load Custom file as early as possible so we can rewrite the values later
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t 'no-error)

;; Introductions
(customize-set-variable 'user-full-name "Christopher McCloud")
(customize-set-variable 'user-mail-address "mccloud.christopher@gmail.com")

;; Early Init: in Emacs 27+ this configuration is loaded prior to init.el
(when (version< emacs-version "27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory))
  (package-initialize))

;; NS Settings
(customize-set-variable 'ns-use-native-fullscreen nil)
(customize-set-variable 'ns-pop-up-frames 'fresh)
(customize-set-variable 'ns-command-modifier 'meta)
(customize-set-variable 'ns-option-modifier 'super)

(prefer-coding-system 'utf-8)

;; Files
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'confirm-kill-emacs #'y-or-n-p)
(customize-set-variable 'enable-recursive-minibuffers t)
(customize-set-variable 'enable-local-variables :safe)

;; Don't let find-file-at-point hang emacs with a bad ping attempt
(customize-set-variable 'ffap-machine-p-unknown 'reject)
(customize-set-variable 'ffap-machine-p-known 'reject)
(customize-set-variable 'ffap-machine-p-local 'reject)
;; Set the active region less eagerly. See GNU: #29889
(customize-set-variable 'select-active-regions 'only)

;; Emacs UI
(customize-set-variable 'visible-cursor nil)
(customize-set-variable 'cursor-in-non-selected-windows nil)
(customize-set-variable 'highlight-nonselected-windows nil)
(customize-set-variable 'indicate-buffer-boundaries nil)
(customize-set-variable 'indicate-empty-lines nil)
(customize-set-variable 'fringe-mode 6)

;; Font Locking
(customize-set-variable 'jit-lock-defer-time nil)
(customize-set-variable 'jit-lock-stealth-nice 0.1)
(customize-set-variable 'jit-lock-stealth-time 0.2)
(customize-set-variable 'jit-lock-stealth-verbose nil)

;; Line Numbers
(customize-set-variable 'display-line-numbers-type t)
(customize-set-variable 'display-line-numbers-current-absolute t)

;; Startup
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable
 'auto-save-list-file-prefix
 (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))
(customize-set-variable 'initial-major-mode 'fundamental-mode)
(customize-set-variable 'initial-scratch-message nil)

(advice-add 'yes-or-no-p :override #'y-or-n-p)

(setq disabled-command-function nil)

(setq-default frame-title-format nil
              fringes-outside-margins t
              bidi-display-reordering t
              inhibit-compacting-font-caches t)

;;*** Use-package Configuration
(customize-set-variable 'use-package-always-defer t)
(customize-set-variable 'use-package-verbose nil)
(customize-set-variable 'use-package-minimum-reported-time 0.01)
(customize-set-variable 'use-package-expand-minimally nil)
(eval-when-compile
  (require 'use-package))

;;** Libraries
(use-package dash
  :config
  (dash-enable-font-lock))
(use-package s)
(use-package hydra)
(use-package seq)
(use-package map)
(use-package dash-functional)
(use-package request)
(use-package deferred)
(use-package f)

;;** Core Packages
;;*** Keybinds
(use-package bind-key
  :demand t
  :custom
  (bind-key-describe-special-forms t)
  :bind
  (("M-m" . mnemonic-map)
   ("M-u" . undo)
   ("C-x C-c" . nil)
   :map mnemonic-map
   ("tF" . toggle-frame-fullscreen)
   ("tl" . display-line-numbers-mode)
   ("bd" . kill-this-buffer)
   ("br" . rename-buffer)
   ("qq" . save-buffers-kill-emacs)
   ("qf" . delete-frame)
   ("ad" . dired)
   ("sgg" . rgrep))
  :config
  (unless (bound-and-true-p mnemonic-map)
    (define-prefix-command 'mnemonic-map)))

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
    ;; Mnemonic Map
    "M-m b" "Buffers"
    "M-m f" "Files"
    "M-m a" "Applications"
    "M-m v" "Version Control"
    "M-m h" "Helm"
    "M-m c" "Customization"
    "M-m l" "Layouts"
    "M-m p" "Projects"
    "M-m q" "Quit"
    "M-m s" "Search"
    "M-m s r" "Ripgrep"
    "M-m s a" "Ag"
    "M-m s g" "Grep/Git-Grep"
    "M-m t" "Toggle"
    "M-m w" "Window")

  ;; These replacements appear only on dired-mode-map
  (which-key-add-major-mode-key-based-replacements 'dired-mode
    "*" "Mark"
    "%" "Regexp"
    ":" "Encryption"
    "C-t" "Image"
    "M-s" "Search"
    "C-x" "Ctrl-X-Map")
  
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;;*** Built In
(use-package frame
  :custom
  (window-divider-default-places 'right-only)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :config
  (window-divider-mode))

(use-package window
  :custom
  (window-combination-limit 'window-size)
  (window-combination-resize t)
  (fit-window-to-buffer-horizontally t)
  (split-width-threshold 80)
  (split-height-threshold 80)
  (split-window-preferred-function 'split-window-sensibly)
  :bind (("M-n" . next-file-buffer)
         ("M-p" . previous-file-buffer)
         :map mnemonic-map
         ("ws" . split-window-tree)
         ("wv" . split-window-below)
         ("wd" . delete-window)
         ("wm" . delete-other-windows)
         ("wb" . balance-windows)
         ("wt" . transpose-window))
  :init
  (defun split-window-tree ()
    "Splits windows such that each gets half as much space as the previous.

E.G. for a four window split, each would receive the following percentage of
the frame width: 1: 50% 2: 25% 3: 12.5% 4: 12.5%.

The end effect, is that even while using `golden-ratio-mode', the first
window will always have a relatively large portion of the screen, and so
is rarely obscured.

Also see `window-combination-limit'."
    (interactive)
    (cl-flet ((swap-window-buffers
               (window-1 window-2)
               (let ((save-buf (window-buffer window-1))
                     (save-start (window-start window-1)))
                 (set-window-buffer window-1 (window-buffer window-2) t)
                 (set-window-start window-1 (window-start window-2))
                 (set-window-buffer window-2 save-buf t)
                 (set-window-start window-2 save-start))))
      (let* ((window-combination-limit t)
             (target (selected-window))
             (root (frame-root-window target))
             (new-window (split-window root nil 'left))
             (windows (window-list-1 new-window)))
        (set-window-buffer new-window (window-buffer target) t)
        (set-window-start new-window (window-start target))
        (cl-loop for window in windows
                 until (equal window target)
                 do (swap-window-buffers window (cadr windows)))
        (other-window -1))))

  (defun display-buffer-in-working-window (buffer alist)
    (let ((windows (cl-remove-if
                    (lambda (w) (window-parameter w 'supporting-window))
                    (window-list-1 nil 'never)))
          (working-window-threshold (floor (/ (frame-width) 85))))
      (if (>= (length windows) working-window-threshold)
          (window--display-buffer buffer (car (last windows)) 'reuse alist)
        (display-buffer-pop-up-window buffer alist))))

  (defun transpose-window ()
    "Transforms the current window into the other 'working window'.

Attempts to display the current window as the 'other window'
relative to the previously selected window. In practice, this
function is used to display the contents of a veritically split
'minor window' in a full sized horizonal split."
    (interactive)
    (let ((buf (window-buffer)))
      (delete-window)
      (select-window (display-buffer-in-working-window buf nil))))

  (defun buffer-call-until (pred change-buffer)
    "Call CHANGE-BUFFER until PRED returns t on the current buffer."
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

(use-package faces
  :init
  (defun font-lock-set-weight (&optional new-value)
    "For font-lock faces, sets weight to NEW-VALUE.

If NEW-VALUE is not provided, then toggles between `bold' and `normal' weight."
    (interactive)
    (let* ((old-value (face-attribute 'font-lock-type-face :weight))
           (new-value (or new-value (if (eq old-value 'normal) 'bold 'normal))))
      (cl-loop for face in (face-list)
               for name = (symbol-name face)
               when (string-match "^font-lock.*" name)
               do (set-face-attribute face nil :weight new-value))))

  (defun set-font-size ()
    "Query user for new font size, which is then applied to `default'."
    (interactive)
    (let* ((old-size (/ (face-attribute 'default :height) 10))
           (new-size (read-string "Font Size: " (int-to-string old-size))))
      (set-face-attribute
       'default nil :height (* (string-to-number new-size) 10)))))

(use-package help-mode
  :config
  ;; Help buttons should respect our window management system
  (gv-define-simple-setter button-type-get button-type-put)
  (add-function
   :around
   (button-type-get 'help-function-def 'help-function)
   (lambda (old-fn fun &optional file type)
     (cl-letf (((symbol-function 'pop-to-buffer)
                (lambda (buffer-or-name &optional _action _no-record)
                  (select-window
                   (display-buffer-in-working-window buffer-or-name nil))))
               (window (selected-window)))
       (funcall old-fn fun file type)
       (unless (one-window-p)
         (delete-window window))))
   '((name . help-button-working-window))))

(use-package page-break-lines
  :hook ((emacs-lisp-mode . page-break-lines-mode)
         (lisp-mode . page-break-lines-mode)
         (scheme-mode . page-break-lines-mode)
         (compilation-mode . page-break-lines-mode)
         (outline-mode . page-break-lines-mode)
         (help-mode . page-break-lines-mode)))

(use-package autorevert
  :demand t
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package woman
  :custom
  (woman-cache-filename "~/.emacs.d/cache/.wmncache.el")
  (woman-cache-level 3)
  (woman-fill-frame t))

(use-package man
  :custom
  (Man-width 75))

(use-package recentf
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 10)
  (recentf-exclude '("~/.emacs.d/cache/.*"))
  (recentf-save-file "~/.emacs.d/cache/recentf")
  :init
  (recentf-mode))

(use-package re-builder
  :config
  (setq reb-auto-match-limit 500))

(use-package hl-line
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  :bind
  (:map mnemonic-map
        ("th" . hl-line-mode)
        ("tH" . global-hl-line-mode))
  :hook (prog-mode . hl-line-mode))

(use-package whitespace
  :custom
  (whitespace-style '(face lines-tail))
  (whitespace-line-column 80))

(use-package simple
  :custom
  (save-interprogram-paste-before-kill t)
  (blink-matching-paren nil)
  :init
  (global-visual-line-mode))

(use-package paren
  :init
  (show-paren-mode))

(use-package term
  :custom
  (term-suppress-hard-newline t)
  (term-buffer-maximum-size 1024)
  :bind
  (:map mnemonic-map
        ("at" . ansi-term)
        :map term-raw-map
        ("M-x" . helm-M-x)
        ("M-m" . nil)))

(use-package eshell
  :custom
  (eshell-directory-name
   (expand-file-name "cache/eshell/" user-emacs-directory))
  :bind (:map mnemonic-map ("ae" . eshell))
  :init (setenv "NODE_NO_READLINE" "1")
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys :map eshell-mode-map
                         ("M-n" . nil)
                         ("M-p" . nil)))))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-keep-variants t))

(use-package cus-edit
  :custom
  (compilation-scroll-output nil)
  (use-dialog-box nil)
  (ring-bell-function #'ignore)
  (visible-bell nil)
  (apropos-do-all t)
  (history-length 1000)
  :bind (:map mnemonic-map
              ("cg" . customize-group)
              ("cf" . customize-apropos-faces)))

;;*** Themes and UI
(use-package spacemacs-common
  :init
  (load-theme 'spacemacs-dark 'no-confirm)
  (font-lock-set-weight 'normal))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-solarized-light-brighter-comments t)
  (doom-solarized-light-comment-bg t)
  (doom-nord-brighter-comments t)
  (doom-nord-comment-bg t)
  (doom-nord-light-brighter-comments t)
  (doom-nord-light-comment-bg t)
  (doom-one-brighter-comments t)
  (doom-one-comment-bg t)
  (doom-one-light-brighter-comments t)
  (doom-one-light-comment-bg t)
  (doom-city-lights-brighter-comments t)
  (doom-city-lights-comment-bg t)
  (doom-dracula-brighter-comments t)
  (doom-dracula-comment-bg t)
  (doom-vibrant-brighter-comments t)
  (doom-vibrant-comment-bg t)
  (doom-peacock-brighter-comments t)
  (doom-peacock-comment-bg t)
  (doom-spacegrey-brighter-comments t)
  (doom-spacegrey-comment-bg t)
  (doom-opera-light-brighter-comments t)
  (doom-opera-light-comment-bg t)
  (doom-opera-brighter-comments t)
  (doom-opera-comment-bg t)
  (doom-molokai-brighter-comments t)
  :config
  ;; More prominent helm source headers
  (setq
   doom-themes-common-faces
   (cons
    '(helm-source-header :background variables :foreground base0 :inherit 'bold)
    (assoc-delete-all 'helm-source-header doom-themes-common-faces))))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1)
  :commands (all-the-icons-material
             all-the-icons-faicon
             all-the-icons-octicon
             all-the-icons-fileicon
             all-the-icons-wicon))

(use-package m-modeline
  :load-path "site-lisp/m-modeline"
  :bind (:map mnemonic-map
              ("tm" . m-modeline-mode))
  :init
  (m-modeline-mode))

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

(use-package delight
  :init
  (delight 'company-mode nil 'company)
  (delight 'helm-mode nil 'helm-mode)
  (delight 'golden-ratio-mode nil 'golden-ratio)
  (delight 'which-key-mode nil 'which-key)
  (delight 'page-break-lines-mode nil 'page-break-lines)
  (delight 'visual-line-mode nil 'simple)
  (delight 'lispy-mode nil 'lispy)
  (delight 'outline-minor-mode nil 'outline)
  (delight 'eldoc-mode nil 'eldoc)
  (delight 'magit-wip-before-change-mode " W" 'magit-wip)
  (delight 'magit-wip-after-apply-mode "I" 'magit-wip)
  (delight 'magit-wip-after-save-local-mode "P" 'magit-wip))

(use-package dimmer
  :load-path "site-lisp/dimmer"
  :custom
  (dimmer-exclusion-regexp-list
   '("^\\*[h|H]elm.*\\*"
     "^\\*Minibuf-[0-9]+\\*"
     "^.\\*which-key\\*$"))
  (dimmer-exclusion-predicates '(helm--alive-p window-minibuffer-p))
  (dimmer-fraction 0.4)
  :bind (:map mnemonic-map
              ("td" . dimmer-mode)))

;;*** Misc
(use-package xwidget
  :if (featurep 'xwidget-internal)
  :after helm-dash
  :custom
  (helm-dash-browser-func 'xwidget-webkit-display-url-buffer)
  :bind (:map xwidget-webkit-mode-map
              ("C-n" . xwidget-webkit-scroll-up-line)
              ("C-p" . xwidget-webkit-scroll-down-line)
              ([mouse-4] . xwidget-webkit-scroll-down-line)
              ([mouse-5] . xwidget-webkit-scroll-up-line)
              ("M-w" . xwidget-webkit-copy-selection-as-kill)
              ("C-c" . xwidget-webkit-copy-selection-as-kill))
  :init
  (defun xwidget-webkit-display-url-buffer (url &optional _session)
    "Browse url in existing webkit session using `display-buffer'."
    (interactive)
    (save-window-excursion
      (xwidget-webkit-browse-url url))
    (display-buffer xwidget-webkit-last-session-buffer)))

(use-package paradox
  :custom
  (paradox-lines-per-entry 1)
  (paradox-use-homepage-buttons nil)
  (paradox-column-width-package 30)
  (paradox-github-token t)
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package bookmark+
  :load-path "site-lisp/bookmark-plus")

(use-package dired
  :bind (:map dired-mode-map
         ("?" . which-key-show-major-mode)))

(use-package dired-async
  :hook (dired-mode . dired-async-mode))

(use-package auth-source
  :custom
  (auth-sources '(macos-keychain-internet macos-keychain-generic))
  (auth-source-cache-expiry 10800))

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

(use-package osx-trash
  :if (equal system-type 'darwin)
  :init
  (osx-trash-setup))

(use-package doc-view
  :bind (:map doc-view-mode-map
         ("k" . nil)
         ("n" . doc-view-next-page)
         ("p" . doc-view-previous-page)
         ("w" . doc-view-fit-width-to-window)
         ("h" . doc-view-fit-height-to-window)
         ("s" . doc-view-search)
         ("g" . doc-view-goto-page)))

;;*** Editing
(use-package yasnippet
  :config
  (yas-reload-all))

(use-package expand-region
  :bind ("C-r" . er/expand-region))

(use-package visual-regexp
  :custom
  (vr/default-replace-preview nil)
  :bind (("C-x /" . vr/query-replace)))

(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :custom
  (vr/engine 'pcre2el))

(use-package smartparens
  :custom
  (sp-echo-match-when-invisible nil)
  :hook ((js2-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package elec-pair
  :hook ((js2-mode . electric-pair-local-mode)))

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
   :map mnemonic-map
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

  ;; Lispy key definitions
  (lispy-define-key lispy-mode-map "q" 'lispy-ace-paren-unbounded)

  ;; Better helm support for lispy-tags
  (advice-add 'lispy--select-candidate :around
              (lambda (f candidates action)
                "Better helm support for lispy-goto"
                (if (eq lispy-completion-method 'helm)
                    (helm :sources
                          (helm-build-sync-source "semantic tags"
                            :candidates candidates
                            :action action
                            :candidate-number-limit 300)
                          :preselect (lispy--current-tag)
                          :buffer "*lispy-goto*")
                  (funcall f candidates action)))
              '((name . lispy--helm-select-candidate)))

  ;; Use helm for lispy visit
  (advice-add 'lispy-visit :override
              (lambda (_arg &rest _args)
                (interactive "p")
                (helm :sources 'helm-source-ls-git))
              '((name . lispy--visit-use-helm)))

  ;; More informative tags for searching by type
  (advice-add 'lispy--tag-name-elisp :around
              (lambda (f x &optional file)
                "Adds a few more categories to how lispy tags are displayed."
                (catch 'error (funcall f x file)
                       (cond ((eq (cadr x) 'function)
                              (if (semantic-tag-get-attribute x :user-visible-flag)
                                  (lispy--propertize-tag "defun" x :command)
                                (lispy--propertize-tag "defun" x :function)))
                             ((eq (cadr x) 'type)
                              (lispy--propertize-tag "defclass" x))
                             (t (funcall f x file)))))
              '((name . lispy--tag-name-elisp-extensions)))
  
  ;; Parse arguments to more forms
  (setcdr (assq 'emacs-lisp-mode lispy-tag-arity)
          (append (cdr (assq 'emacs-lisp-mode lispy-tag-arity))
                  '((cl-defmethod . 2)
                    (cl-defgeneric . 1)
                    (defclass . 1)))))

;;*** Window and Buffer Management
(use-package eyebrowse
  :custom
  (eyebrowse-new-workspace t)
  :bind*
  (:map mnemonic-map
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
  (persp-auto-save-opt 1)
  (persp-autokill-buffer-on-remove nil)
  (persp-keymap-prefix (kbd "C-c l"))
  (persp-auto-resume-time 0)
  (persp-init-frame-behaviour 'persp-ignore-wconf)
  :custom-face
  (persp-face-lighter-buffer-not-in-persp ((t (:inherit error))))
  :bind
  (:map mnemonic-map
        ("ll" . persp-mode)
        ("ls" . persp-switch)
        ("la" . persp-add-buffer)
        ("lr" . persp-remove-buffer)
        ("lw" . persp-save-state-to-file)
        ("lf" . persp-load-state-from-file))
  :hook ((persp-mode . persp-mode-setup-advice))
  :config
  (defvar persp-mode-functions-to-advise
    '(next-buffer
      previous-buffer
      helm-mini
      helm-imenu-in-all-buffers
      helm-multi-swoop
      helm-multi-swoop-all
      helm-multi-occur-all)
    "List of functions which need additional advising when using `persp-mode'.")

  (defun persp-mode-wrapper (wrapped-buffer-command &rest r)
    "Wrapper for commands which need advising for use with `persp-mode'.
Only for use with `advice-add'."
    (with-persp-buffer-list () (apply wrapped-buffer-command r)))

  (defun persp-mode-setup-advice ()
    "Adds or removes advice on functions in `persp-mode-functions-to-advise'."
    (cl-loop for func in persp-mode-functions-to-advise
             do (if persp-mode
                    (advice-add func :around #'persp-mode-wrapper)
                  (advice-remove func #'persp-mode-wrapper)))))

(use-package eyebrowse-persp-bridge
  :demand t
  :after (eyebrowse persp-mode)
  :load-path "site-lisp/eyebrowse-persp-bridge")

(use-package ace-window
  :custom
  (aw-keys '(49 50 51 52 53 54 55 56 57 48))
  :custom-face
  (aw-leading-char-face ((t (:inherit t :height 1.3))))
  :bind* (("M-o" . ace-window)
          :map mnemonic-map
          ("wS" . ace-swap-window)))

(use-package window-numbering
  :bind
  (("M-1" . select-window-1)
   ("M-2" . select-window-2)
   ("M-3" . select-window-3)
   ("M-4" . select-window-4)
   ("M-5" . select-window-5))
  :config
  (advice-add 'window-numbering-install-mode-line :override #'ignore)
  (advice-add 'window-numbering-clear-mode-line :override #'ignore)
  (window-numbering-mode))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :bind (:map mnemonic-map
              ("wu" . winner-undo)
              ("wr" . winner-redo))
  :hook (ediff-quit . winner-undo)
  :config
  (winner-mode))

(use-package golden-ratio
  :load-path "site-lisp/golden-ratio"
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-buffer-names '("*tide-documentation*"))
  :bind (:map mnemonic-map
              ("tg" . golden-ratio-mode)
              ("wg" . golden-ratio))
  :config
  (defun golden-ratio-helm-alive-p ()
    (ignore-errors (helm--alive-p)))

  (defun golden-ratio-in-ediff-p ()
    (ignore-errors (or ediff-this-buffer-ediff-sessions
                       (ediff-in-control-buffer-p))))

  (defun golden-ratio-company-box-p ()
    (frame-parameter (selected-frame) 'company-box-buffer))

  (defun golden-ratio-in-magit-p ()
    (string-match-p ".*magit.*" (symbol-name major-mode)))

  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-helm-alive-p)
  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-in-ediff-p)
  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-company-box-p)
  (add-to-list 'golden-ratio-inhibit-functions #'golden-ratio-in-magit-p))

(use-package shackle
  :demand t
  :custom
  (shackle-select-reused-windows t)
  :config
  (defun shackle-left-or-below () (if (> (frame-width) 160) 'left 'below))
  
  (defun shackle-display-helm-help (buffer alist plist)
    ;; When not in helm, we process the rule again without the custom
    ;; property.  If we are in helm, do nothing and let helm (or a
    ;; different matching rule) take care of how to display the help
    ;; buffer.
    (if (bound-and-true-p helm-alive-p)
        (let (shackle-rules)
          (display-buffer buffer))
      (shackle--display-buffer
       buffer
       alist
       (append
        '(:custom shackle-supporting-window)
        (cl-remove-if
         (lambda (elt) (or (functionp elt) (equal elt :custom))) plist)))))

  (defun shackle-supporting-window--delete-other-windows (window)
    (set-window-parameter window 'supporting-window nil)
    (set-window-parameter window 'delete-other-windows nil)
    (delete-other-windows window))
  
  (defun shackle-supporting-window (buffer alist plist)
    (shackle--display-buffer
     buffer
     (append alist
             `((window-parameters
                .
                ((supporting-window . t)
                 (delete-other-windows
                  . shackle-supporting-window--delete-other-windows)))))
     (cl-remove-if
      (lambda (elt) (or (functionp elt) (equal elt :custom))) plist)))

  (defun shackle-working-window (buffer alist _plist)
    (display-buffer-in-working-window buffer alist))
  
  (customize-set-variable
   'shackle-rules
   `(("*Process List*" :custom shackle-supporting-window
      :select t :align below :size 0.3)
     ("*Apropos*" :custom shackle-supporting-window
      :select t :align below :size 0.3)
     ("Outline.*pdf" :custom shackle-supporting-window
      :regexp t :select t :align below :size 0.3)
     ("*Geiser documentation*" :custom shackle-supporting-window
      :select t :align below :size 0.3)
     ("*slime-description*" :custom shackle-supporting-window
      :select t :align below :size 0.3)
     ("\\`\\*[h|H]elm.*\\*\\'" :custom shackle-supporting-window
      :regexp t :align t :size 0.3)
     ("*Help*" :custom shackle-display-helm-help
      :select t :align below :size 0.3)
     ("^\\*helpful.*" :custom shackle-supporting-window
      :regexp t :select t :align below :size 0.3)
     ("*Completions*" :custom shackle-supporting-window
      :select t :align below :size 0.3)
     ("*Compile-Log*" :custom shackle-supporting-window
      :select t :align below :size 0.3)
     ("*lispy-goto*" :custom shackle-supporting-window
      :align t :size 0.3)
     ("*tide-documentation*" :custom shackle-supporting-window
      :select t :align below :size 0.3 :popup t)
     ("*lispy-help*" :custom shackle-supporting-window
      :select t :align below :size 0.3 :popup t)
     ("magit-process:.*" :custom shackle-supporting-window
      :regexp t :select t :align below :size 0.3 :popup t)
     ("^\\*\\(Wo\\)?Man.*" :custom shackle-supporting-window
      :regexp t :select t :align below :size 0.3 :popup t)
     ("*git-gutter:diff*" :other t :size 0.3)
     ("^\\*xwidget.webkit:.*\\*" :regexp t :custom shackle-working-window)
     ("*Diff*" :select t :other t :size 0.3)
     ("*Package Commit List*" :select t :other t :align left)
     (helm-grep-mode :custom shackle-working-window)
     (ag-mode :custom shackle-working-window)
     (dired-mode :custom shackle-working-window)
     (helm-moccur-mode :custom shackle-working-window)
     (xref--xref-buffer-mode :custom shackle-working-window)))

  (shackle-mode))

;;*** Navigation/Code Navigation
(use-package xref
  :custom
  (xref-marker-ring-length 200)
  :config
  (defun xref-push-after (&rest _args)
    "Advises `push-mark' to conditionally push to the `xref--marker-ring'.

Pushes mark to the xref marker stack unless the current mark is
identical to the most recently added xref marker."
    (let* ((ring xref--marker-ring)
           (len (cadr ring))
           (markers (cddr ring))
           (last (and (> len 0) (aref markers (1- len)))))
      (unless (and last
                   (eq (current-buffer) (marker-buffer last))
                   (eq (point) (marker-position last)))
        (xref-push-marker-stack))))
  ;; Don't bother with global vs local vs xref markers, just track
  ;; them all in xref-marker-ring.
  (advice-add 'push-mark :after #'xref-push-after))

(use-package semantic
  :custom
  (semantic-elisp-store-documentation-in-tag nil)
  (semantic-edits-verbose-flag nil)
  (semantic-idle-scheduler-idle-time 10)
  (semantic-stickyfunc-indent-string " ")
  (semanticdb-default-save-directory
   (concat user-emacs-directory "cache/semanticdb"))
  (semantic-analyze-summary-function 'semantic-format-tag-short-doc)
  :hook (semantic-mode . semantic-imenu-cleanup)
  :config
  (defun semantic-imenu-cleanup ()
    (unless semantic-mode
      (eval `(setq-mode-local ,major-mode imenu-create-index-function
                              (default-value 'imenu-create-index-function)))))

  (semantic-default-elisp-setup)

  (setq-mode-local emacs-lisp-mode
                   semanticdb-find-default-throttle
                   (default-value 'semanticdb-find-default-throttle)
                   completion-at-point-functions
                   '(elisp-completion-at-point))

  (cl-loop for fun in
           '(semantic-analyze-completion-at-point-function
             semantic-analyze-notc-completion-at-point-function
             semantic-analyze-nolongprefix-completion-at-point-function)
           do (advice-add fun :override #'ignore)))

(use-package semantic-el-extensions
  :load-path "site-lisp/semantic-el-extensions"
  :demand t
  :after semantic)

(use-package mode-local
  :commands (mode-local-bind))

(use-package imenu
  :hook (emacs-lisp-mode . imenu-emacs-lisp-add-expressions)
  :init
  (defun imenu-emacs-lisp-add-expressions ()
    "Recognize `use-package' in imenu when in emacs-lisp-mode."
    (add-to-list
     'imenu-generic-expression
     '("Package" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2) t)))

(use-package elisp-slime-nav
  :bind (("C-c C-d" . elisp-slime-nav-describe-elisp-thing-at-point)))

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-background t)
  (avy-style 'at)
  :bind
  (("C-j" . avy-goto-char-timer)
   ("M-g c" . avy-goto-char-timer)
   ("M-g g" . avy-goto-line)
   ("M-g M-g" . avy-goto-line)))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'helm)
  :bind (("M-g d" . dumb-jump-go)
         ("M-g D" . dumb-jump-go-prompt)))

(use-package smart-jump
  :custom
  (smart-jump-bind-keys nil)
  :bind* (("M-." . smart-jump-go)
          ("M-," . smart-jump-back)
          ("M-?" . smart-jump-references))
  :config
  (smart-jump-setup-default-registers)
  ;; Use in js2-mode if available
  (require 'smart-jump-typescript-mode)
  (smart-jump-typescript-mode-register 'js2-mode))

;;*** Search (Grep/Git Grep/Ag/Rg)
(use-package rg
  :custom
  (rg-group-result t)
  :bind (:map mnemonic-map
         ("s r r" . rg)
         ("s r p" . rg-project)
         ("s r d" . rg-dwim)
         ("s r l" . rg-literal)
         ("s r s" . rg-list-searches)
         :map rg-mode-map
         ("n" . compilation-next-error)
         ("p" . compilation-previous-error)
         ("C-j" . compilation-display-error)
         ("e" . compilation-display-error)))

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t)
  (ag-ignore-list '("archive-contents"))
  :bind (:map mnemonic-map
              ("s a a" . ag-regexp)
              ("s a p" . ag-project-regexp)
              ("s a A" . ag)
              ("s a P" . ag-project)
              ("s a d" . ag-dired-regexp)
              ("s a D" . ag-dired)))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer nil)
  (wgrep-enable-key (kbd "C-c C-e")))

(use-package wgrep-ag)

(use-package wgrep-helm)

;;*** Helm
(use-package helm
  :custom
  (helm-candidate-number-limit 100)
  (helm-autoresize-max-height 30)
  (helm-display-header-line nil)
  (helm-split-window-inside-p t)
  (helm-follow-mode-persistent t)
  :custom-face
  (helm-match ((t (:inherit font-lock-keyword-face :weight bold))))
  (helm-candidate-number ((t (:inherit bold :background nil))))
  :bind (:map mnemonic-map
              ("hr" . helm-resume)
              ("hk" . helm-show-kill-ring)
              :map helm-map
              ("C-z" . helm-select-action)
              ("<tab>" . helm-execute-persistent-action)
              ("TAB" . helm-execute-persistent-action)
              ("C-M-n" . helm-scroll-other-window)
              ("C-M-p" . helm-scroll-other-window-down)
              ("C-s" . helm-into-next))
  :init
  (defvar helm-into-next-alist nil)
  :config
  (defun helm-into-next ()
    (interactive)
    (when-let ((input helm-input)
               (next-func (cdr (assoc helm-buffer helm-into-next-alist))))
      (with-helm-alive-p
        (helm-run-after-exit next-func input)))))

(use-package helm-config
  :demand t
  :after helm)

(use-package helm-mode
  :demand t
  :after helm
  :custom
  (helm-completion-in-region-fuzzy-match t)
  (helm-completing-read-handlers-alist
   '((xref-find-definitions . helm-completing-read-default-find-tag)
     (describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-default-find-tag)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (find-file-at-point . helm-completing-read-sync-default-handler)
     (ffap . helm-completing-read-sync-default-handler)
     (execute-extended-command)
     (dired-do-rename . helm-read-file-name-handler-1)
     (dired-do-copy . helm-read-file-name-handler-1)
     (dired-do-symlink . helm-read-file-name-handler-1)
     (dired-do-relsymlink . helm-read-file-name-handler-1)
     (dired-do-hardlink . helm-read-file-name-handler-1)))
  :config
  (helm-mode))

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
     "^magit.*:"
     "\\*Minibuf"
     "\\*Diff*"
     "\\*lispy-goto*"
     "\\*Backtrace*"))
  :bind (("C-x C-b" . helm-mini)
         :map mnemonic-map
         ("bb" . helm-mini))
  :config
  (defun helm-next-buffer ()
    (interactive)
    (buffer-call-until
     (lambda (b)
       (not (cl-some (lambda (rxp) (string-match-p rxp (buffer-name b)))
                     helm-boring-buffer-regexp-list)))
     'next-buffer))

  (defun helm-previous-buffer ()
    (interactive)
    (buffer-call-until
     (lambda (b)
       (not (cl-some (lambda (rxp) (string-match-p rxp (buffer-name b)))
                     helm-boring-buffer-regexp-list)))
     'previous)))

(use-package helm-files
  :custom
  (helm-ff-auto-update-initial-value nil)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-p" . helm-known-projects)
         :map mnemonic-map
         ("ff" . helm-find-files)
         ("hf" . helm-find))
  :config
  (defvar helm-source-files-known-projects
    (helm-build-sync-source "Known Project Repositories"
      :candidates #'magit-list-repos
      :action (helm-make-actions
               "Browse Project"
               (lambda (candidate)
                 (with-helm-default-directory candidate
                     (helm-browse-project nil)))
               "Git Status"
               (lambda (candidate)
                 (magit-status-internal candidate)))))

  (defun helm-known-projects ()
    "Navigation between projects know `magit-list-repos'."
    (interactive)
    (helm :buffer "*helm projects*"
          :sources helm-source-files-known-projects)))

(use-package helm-find
  :commands (helm-find-1))

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
  (cl-defmethod helm-setup-user-source ((source helm-source-multi-occur))
    (setf (slot-value source 'candidate-number-limit) 500))

  (defun helm-multi-occur-all (&optional input)
    "Runs `helm-occur' on all buffers visiting files."
    (interactive)
    (helm-multi-occur-1
     (cl-remove-if-not #'buffer-file-name (buffer-list))
     input))

  (helm-attrset 'candidate-number-limit 300 helm-source-regexp)
  (add-to-list 'helm-into-next-alist
               '("*helm occur*" . helm-multi-occur-all)))

(use-package helm-grep
  :custom
  (helm-grep-ag-command
   "rg -M 256 --color=always --smart-case --no-heading --line-number %s %s %s")
  :init
  (defun helm-grep-ag-dwim (&optional input)
    "Calls `helm-grep-ag' in git project root or default directory."
    (interactive)
    (minibuffer-with-setup-hook (lambda () (insert (or input "")))
      (helm-grep-ag-1 (or (expand-file-name
                           (locate-dominating-file default-directory ".git"))
                          default-directory))))
  (add-to-list 'helm-into-next-alist
               '("*helm multi occur*" . helm-grep-ag-dwim)))

(use-package helm-locate
  :bind (:map mnemonic-map
              ("fl" . helm-locate))
  :config
  (when (equal system-type 'darwin)
    (customize-set-variable 'helm-locate-fuzzy-match nil)
    (customize-set-variable 'helm-locate-command "mdfind -name %s %s")))

(use-package helm-elisp
  :bind (("C-h a" . helm-apropos)
         :map mnemonic-map
         ("hL" . helm-locate-library)))

(use-package helm-info
  :bind (("C-h i" . helm-info)))

(use-package helm-man
  :custom
  (helm-man-or-woman-function 'Man-getpage-in-background)
  :bind (:map mnemonic-map
              ("hm" . helm-man-woman)))

(use-package helm-color
  :bind (:map mnemonic-map
              ("hc" . helm-colors))
  :config
  (helm-attrset 'candidate-number-limit 9999 helm-source-colors)
  (helm-attrset 'candidate-number-limit 9999 helm-source-customize-face))

(use-package helm-imenu
  :custom
  (helm-imenu-fuzzy-match nil)
  (helm-imenu-delimiter ": ")
  :bind* (("C-x C-j" . helm-imenu-in-all-buffers))
  :config
  (cl-defmethod helm-setup-user-source ((source helm-imenu-source))
    (setf (slot-value source 'candidate-number-limit) 100))

  (add-to-list 'helm-imenu-type-faces
               '("^Package$" . font-lock-type-face)))

(use-package helm-semantic
  :custom
  (helm-semantic-display-style nil))

(use-package helm-ls-git
  :custom
  (helm-ls-git-status-command 'magit-status-internal)
  :custom-face
  (helm-ls-git-modified-not-staged-face ((t (:inherit diff-changed))))
  (helm-ls-git-added-copied-face ((t (:inherit diff-added))))
  (helm-ls-git-added-modified-face ((t (:inherit diff-added))))
  (helm-ls-git-conflict-face ((t (:inherit diff-refine-removed))))
  (helm-ls-git-untracked-face ((t (:inherit diff-removed))))
  :bind (("C-x C-d" . helm-browse-project)
         :map helm-ls-git-map
         ("C-s" . helm-ls-git-run-grep)
         ("C-c /" . helm-ls-git-run-find))
  :config
  (defun helm-ls-git-find (&optional _args)
    "As `helm-find', but using the current project root."
    (interactive)
    (let ((helm-ff-default-directory (helm-ls-git-root-dir)))
      (helm-find-1 (helm-ls-git-root-dir))))

  (defun helm-ls-git-run-find ()
    "Run find from helm-ls-git."
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-ls-git-find)))

  (cl-defmethod helm-setup-user-source ((source helm-ls-git-source))
    (setf (slot-value source 'action)
          (helm-append-at-nth
           (slot-value source 'action)
           (helm-make-actions "Find shell command in project `C-c /'"
                              'helm-ls-git-find)
           8)))

  (add-to-list 'helm-ls-git-default-sources
               'helm-source-files-known-projects t))

(use-package helm-persp
  :load-path "site-lisp/helm-persp"
  :bind ("C-x C-l" . helm-persp-layouts))

(use-package helm-descbinds
  :custom
  (helm-descbinds-window-style 'split-window)
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-themes
  :bind (:map mnemonic-map
         ("ht" . helm-themes))
  :config
  (defvar helm-themes-font-plist
    '(spacemacs-dark
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      spacemacs-light
      (default ((t (:height 120 :width normal :weight light :family "Input Mono"))))
      darktooth
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-one
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-one-light
      (default ((t (:height 120 :width normal :weight light :family "Input Mono"))))
      doom-peacock
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-vibrant
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-city-lights
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-spacegrey
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-tomorrow-night
      (default ((t (:height 120 :width ultra-condensed :family "Input"))))
      doom-tomorrow-day
      (default ((t (:height 120 :width normal :weight light :family "Input Mono"))))
      doom-solarized-light
      (default ((t (:height 120 :width normal :weight light :family "Input Mono"))))
      doom-opera-light
      (default ((t (:height 120 :width normal :weight light :family "Input Mono")))))
    "Mapping of themes to fonts used by `helm-themes'.

Switching to an included theme will additionally set the default font
as specified.")

  (advice-add 'helm-themes :after
              (lambda (&rest _args)
                (when-let* ((theme (car custom-enabled-themes))
                            (font (plist-get helm-themes-font-plist theme)))
                  (custom-set-faces font)))
              '((name . helm-themes-font-advice))))

(use-package helm-dash
  :custom
  (helm-dash-browser-func 'browse-url)
  :bind (:map mnemonic-map
              ("hd" . helm-dash-at-point))
  :hook ((emacs-lisp-mode . helm-dash-setup-docs)
         (js2-mode . helm-dash-setup-docs))
  :config
  (defvar helm-dash-docsets nil
    "Buffer local list of docsets to search by default.")

  (defvar helm-dash-docsets-for-mode
    `((emacs-lisp-mode . ("Emacs Lisp" "Emacs-CL"))
      (js2-mode . ("JavaScript" "NodeJS" "React")))
    "An association list of the form (MODE . (DOCSETS)).
MODE is a symbol and DOCSETS is a list of one or more strings.  When
calling `helm-dash-at-point' or `helm-dash', this list will be used to
set the active dash docsets based on the current major-mode.")

  (defun helm-dash-setup-docs ()
    (setq-local helm-dash-docsets
                (alist-get major-mode helm-dash-docsets-for-mode))))

(use-package helm-xref
  :load-path "site-lisp/helm-xref"
  :custom
  (xref-show-xrefs-function 'helm-xref-show-xrefs)
  :commands (helm-xref-show-xrefs))

;;*** Version Control and Project Management
(use-package magit
  :defer 3
  :custom
  (magit-process-popup-time 5)
  (magit-ediff-dwim-show-on-hunks nil)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-repository-directories
   '(("/Users/Macnube/Repos" . 1)
     ("/Users/Macnube/.emacs.d" . 1)
     ("/Users/Macnube/.emacs.d/site-lisp" . 1)))
  :bind
  (("C-x g" . magit-status)
   ("C-x C-v" . magit-status)
   :map mnemonic-map
   ("vs" . magit-status)
   ("vb" . magit-blame)
   ("vh" . magit-dispatch-popup)
   ("vc" . magit-commit-popup))
  :commands (magit-list-repos)
  :config
  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode))

(use-package vc
  :custom-face
  (vc-state-base ((t (:weight bold))))
  (vc-conflict-state ((t (:inherit (diff-changed vc-state-base)))))
  (vc-edited-state ((t (:inherit (diff-changed vc-state-base)))))
  (vc-locally-added-state ((t (:inherit (diff-changed vc-state-base)))))
  (vc-missing-state ((t (:inherit (diff-changed vc-state-base)))))
  (vc-needs-update-state ((t (:inherit (diff-changed vc-state-base)))))
  (vc-removed-state ((t (:inherit (diff-changed vc-state-base)))))
  (vc-up-to-date-state ((t (:inherit (diff-added vc-state-base))))))

(use-package projectile
  :custom
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  :bind (:map mnemonic-map
              ("sgp" . projectile-grep))
  :config
  (add-to-list 'projectile-globally-ignored-directories "semanticdb"))

(use-package magithub
  :custom
  (magithub-dir
   (concat user-emacs-directory "cache/magithub"))
  (magithub-clone-default-directory (expand-file-name "~/Repos/"))
  (magithub-datetime-format "%A %B %e%l:%M%p %Y")
  (magithub-dashboard-show-read-notifications nil)
  (magithub-preferred-remote-method 'clone_url)
  :commands (magithub-clone)
  :config
  (magithub-feature-autoinject
   '(completion status-checks-header commit-browse pull-request-merge)))

(use-package diff-hl
  :bind (:map diff-hl-command-map
         ("n" . hydra-diff-hl/body)
         :map mnemonic-map
         ("vn" . hydra-diff-hl/body))
  :hook ((prog-mode . diff-hl-mode)
         (prog-mode . diff-hl-flydiff-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (defhydra hydra-diff-hl (:columns 2 :exit nil :foreign-keys nil)
    "Navigate diffs"
    ("n" diff-hl-next-hunk "Next Hunk")
    ("p" diff-hl-previous-hunk "Previous Hunk")
    ("d" vc-diff "Diff" :exit t)
    ("c" magit-commit-popup "Commit" :exit t)
    ("q" nil "Quit" :exit t)))

;;*** Syntax Checking/Completion/Formatting
(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-error-list-minimum-level 'error)
  (flycheck-indication-mode 'right-fringe)
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
  (:map mnemonic-map
        ("tc" . flycheck-mode)))

(use-package company
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-limit 12)
  (company-require-match 'never)
  (company-tooltip-align-annotations t)
  :hook ((prog-mode . company-mode))
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
  :config
  (setq company-box-backends-colors nil
        company-box-doc-frame-parameters
        '((internal-border-width . 10)
          (font . "Input 11")))
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

;;** Languages and Language Extensions
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
  :after (web-mode company)
  :init
  (add-to-list 'company-backends 'company-web-html))

(use-package js
  :custom
  (js-chain-indent t)
  (js-indent-align-list-continuation t)
  (js-flat-functions t))

(use-package js2-mode
  :mode "\\.js$"
  :custom
  (js2-strict-missing-semi-warning nil)
  (js2-missing-semi-one-line-override t))

(use-package tide
  :bind (:map tide-mode-map
              ("C-c C-d" . tide-documentation-at-point))
  :hook ((js2-mode . tide-setup)
         (tide-mode . tide-eldoc-cleanup))
  :config
  (defun tide-eldoc-cleanup (&optional _arg)
    (when (and (not tide-mode)
               (commandp 'typescript-insert-and-indent))
      (eldoc-remove-command 'typescript-insert-and-indent))))

;; Re-enable Garbage Collection
(setq gc-cons-threshold (* 1024 1024 16)
      gc-cons-percentage .1)

;;; init.el ends here.
