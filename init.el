;; Supress Garbage Collection During Initialization
(setq gc-cons-threshold 100000000)
;; Begin Emacs Initialization
;; Load Customization Settings
(load-file (expand-file-name "custom.el" user-emacs-directory))

;; Extend Load Path
(eval-and-compile
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Appearance and Package Initialization
(package-initialize)

(load-theme 'sanityinc-solarized-light)

(require 'use-package)
(require 'bind-key)
(require 'diminish)

;; Key Bindings
(define-prefix-command 'mnemonic-map)
(bind-keys ("M-m" . mnemonic-map)
           ("M-u" . undo))
(define-prefix-command 'buffer-map)
(define-prefix-command 'toggle-map)
(bind-keys :map mnemonic-map
           ("t" . toggle-map)
           ("b" . buffer-map))
(bind-keys :map toggle-map
           ("F" . toggle-frame-fullscreen))
(bind-keys :map buffer-map
           ("d" . kill-this-buffer)
           ("r" . rename-buffer))

;; Packages
;; TODO: Manually Set Path on OSX
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package doc-view
  :defer t
  :config
  (bind-keys :map doc-view-mode-map
             ("k" . nil)
             ("n" . doc-view-next-page)
             ("p" . doc-view-previous-page)
             ("w" . doc-view-fit-width-to-window)
             ("h" . doc-view-fit-height-to-window)
             ("s" . doc-view-search)
             ("g" . doc-view-goto-page)))

(use-package osx-trash
  :if (memq system-type '(osx darwin))
  :init
  (osx-trash-setup))

(use-package recentf
  :config
  (recentf-mode)
  (recentf-track-opened-file))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode))

(use-package semantic
  :disabled t
  :defer 5
  :init
  (add-hook 'emacs-lisp-mode-hook #'semantic-mode)
  :config
  (semantic-default-elisp-setup))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (smartparens-global-strict-mode 1)
  (show-smartparens-global-mode 1))

(use-package lispy
  :defer 5
  :init
  (add-hook 'lisp-mode-hook #'lispy-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'scheme-mode-hook #'lispy-mode)
  (with-eval-after-load 'clojure-mode
    (add-hook 'clojure-mode-hook #'lispy-mode))
  (with-eval-after-load 'racket-mode
    (add-hook 'racket-mode-hook #'lispy-mode))
  :config
  (load (expand-file-name
         "site-lisp/clojure-semantic/clojure.el"
         user-emacs-directory))
  (bind-keys :map lispy-mode-map
             ("C-j" . avy-goto-word-or-subword-1)
             ("C-z" . avy-goto-line)
             ("M-m" . mnemonic-map))
  (bind-keys :map mnemonic-map
             ("M-m" . lispy-mark-symbol)))

(use-package clojure-mode :defer t)

(use-package racket-mode
  :defer t
  :config
  (with-eval-after-load 'smartparens
    (add-to-list 'sp--lisp-modes 'racket-mode)
    (sp-local-pair 'racket-mode "'" nil :actions nil)
    (sp-local-pair 'racket-mode "`" nil :actions nil)))

(use-package geiser
  :commands run-geiser)

(use-package slime
  :commands slime-mode
  :config
  (slime-setup '(slime-fancy slime-company))
  (setq inferior-lisp-program "sbcl"))

(use-package cider
  :commands (cider--display-interactive-eval-result)
  :defer t
  :init
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package avy
  :bind
  (("C-j" . avy-goto-word-or-subword-1)
   ("C-z" . avy-goto-line)))

(use-package helm
  :diminish helm-mode
  :init
  (use-package helm-ag :defer t)
  (use-package helm-descbinds :defer t)
  (use-package helm-themes :defer t)
  (use-package helm-swoop :bind (("C-s" . helm-swoop)))
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-switch-project
               helm-projectile))
  :config
  (require 'helm-config)
  (bind-keys :map helm-map
             ("C-z" . helm-select-action)
             ("<tab>" . helm-execute-persistent-action)
             ("TAB" . helm-execute-persistent-action))
  (bind-keys :map buffer-map
             ("f" . helm-buffers-list))
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-mini)
   ("C-x C-p" . helm-projectile)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info)))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :config
  (projectile-global-mode))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package company
  :diminish company-mode
  :demand t
  :init
  (use-package company-tern)
  (use-package slime-company)
  :config
  (global-company-mode 1)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("RET" . nil)
        ("<return>" . nil)
        ("<tab>" . company-complete-selection)
        ("TAB" . company-complete-selection)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package js2-mode
  :mode "\\.js$")

(use-package skewer-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook #'skewer-mode))

(use-package tern
  :diminish tern-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package window-numbering
  :init
  (progn
    (defun user--split-and-balance-window-right ()
      "As 'SPLIT-WINDOW-RIGHT' followed by 'BALANCE-WINDOWS'"
      (interactive)
      (split-window-right)
      (balance-windows)))
  :config
  (window-numbering-mode 1)
  (bind-keys :prefix-map window-management-map
             :prefix "M-m w"
             ("s" . user--split-and-balance-window-right)
             ("d" . delete-window)
             ("m" . delete-other-windows))
  (bind-keys ("M-1" . select-window-1)
             ("M-2" . select-window-2)
             ("M-3" . select-window-3)
             ("M-4" . select-window-4)))

(use-package shackle
  :config
  (shackle-mode 1))

;; End Emacs Initialization
;; Re-enable Garbage Collection
(setq gc-cons-threshold 800000)
