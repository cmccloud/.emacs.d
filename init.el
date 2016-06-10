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

(require 'use-package)
(require 'bind-key)
(require 'diminish)

(load-theme 'sanityinc-solarized-light)

;; Key Bindings
(define-prefix-command 'mnemonic-map)
(bind-keys ("M-m" . mnemonic-map)
           ("M-u" . undo)
           ("C-x C-c" . nil))
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


;; Libraries
(use-package s)
(use-package dash)

;; Packages
(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :init
  (setenv "PATH"
          (s-join
           ":"
           '("/usr/local/opt/coreutils/libexec/gnubin"
             "/usr/local/bin"
             "/usr/local/sbin"
             "/usr/bin"
             "/usr/sbin"
             "/bin"
             "/sbin"
             "/opt/X11/bin"
             "/usr/local/otp/nvm"
             "/usr/local/opt/nvm/bin"
             "/usr/local/opt/nvm/sbin"
             "/usr/local/opt/nvm/versions/node/v6.2.0/bin"
             "/usr/local/share/npm/bin"
             "/Library/TeX/texbin"
             "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9"
             "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9")))
  :config
  (exec-path-from-shell-initialize))

(use-package term
  :config
  (with-eval-after-load 'helm
    (bind-keys :map term-raw-map
               ("M-x" . helm-M-x))))

(use-package pdf-tools
  :defer t
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

(use-package eyebrowse
  :config
  (eyebrowse-mode))

(use-package osx-trash
  :if (memq system-type '(osx darwin))
  :init
  (osx-trash-setup))

(use-package recentf
  :functions
  (recentf-track-opened-file)
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

(use-package expand-region
  :config
  (bind-keys ("C-r" . er/expand-region)))

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
  (bind-keys :map lispy-mode-map
             ("C-j" . avy-goto-word-or-subword-1)
             ("C-z" . avy-goto-line)
             ("M-m" . mnemonic-map))
  (bind-keys :map mnemonic-map
             ("M-m" . lispy-mark-symbol)
             ("g" . lispy-goto))
  ;; Prevent semantic mode errors when using
  ;; lispy goto
  (advice-add 'special-lispy-goto
              :after
              (defun lispy--supress-semantic ()
                (semantic-mode -1)))
  :bind
  (("C-a" . lispy-move-beginning-of-line)))

(use-package clojure-mode
  :defer t
  :config
  (with-eval-after-load 'lispy
    (load (expand-file-name
           "site-lisp/clojure-semantic/clojure.el"
           user-emacs-directory))))

(use-package cider
  :commands (cider-mode
             cider--display-interactive-eval-result)
  :init
  (add-hook 'clojure-mode-hook #'cider-mode)
  :config
  ;; FIXME: Why doesn't this binding hold?
  (bind-keys :map cider-mode-map
             ("C-c C-w" . nil)))

(use-package racket-mode
  :functions (sp-local-pair)
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
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package oz
  :load-path "/Applications/Mozart2.app/Contents/Resources/share/mozart/elisp"
  :mode (("\\.oz\\'" . oz-mode)
         ("\\.ozg\\'" . oz-gump-mode))
  :init
  (setenv "OZHOME" "/Applications/Mozart2.app/Contents/Resources"))

(use-package avy
  :bind
  (("C-j" . avy-goto-word-or-subword-1)
   ("C-z" . avy-goto-line)))

(use-package helm
  :commands (helm-M-x)
  :diminish helm-mode
  :init
  (use-package helm-ag :defer t)
  (use-package helm-descbinds
    :bind
    (("C-h b" . helm-descbinds)))
  (use-package helm-themes :defer t)
  (use-package helm-swoop
    :bind
    (("C-s" . helm-swoop)))
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-switch-project
               helm-projectile))
  :config
  (use-package helm-config)
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
  :demand t
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
  :commands (global-company-mode company-mode)
  :init
  (use-package company-tern :commands (company-tern))
  (use-package slime-company :defer t)
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("RET" . nil)
             ("<return>" . nil)
             ("<tab>" . company-complete-selection)
             ("TAB" . company-complete-selection)))

(use-package flycheck
  :defer t
  :init
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
    :fringe-face 'flycheck-fringe-info))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package js2-mode
  :mode "\\.js$"
  :config
  (use-package nodejs-repl
    :config
    (defun nodejs-repl-eval-dwim ()
      (interactive)
      (if mark-active
          (nodejs-repl-send-region (region-beginning)
                                   (region-end))
        (nodejs-repl-send-last-sexp)))
    (defun chrome-refresh-current-tab ()
      (interactive)
      (do-applescript
       "tell application \"Google Chrome\" to reload active tab of window 1"))
    (bind-keys :map js2-mode-map
               ("C-x C-e" . nodejs-repl-eval-dwim))
    (bind-keys :map mnemonic-map
               ("<f5>" . chrome-refresh-current-tab))))

(use-package skewer-mode
  :defer t
  :commands (skewer-mode))

(use-package tern
  :diminish tern-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package window-numbering
  :init
  (defun user--split-and-balance-window-right ()
    "As 'SPLIT-WINDOW-RIGHT' followed by 'BALANCE-WINDOWS'"
    (interactive)
    (split-window-right)
    (balance-windows))
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

(use-package golden-ratio
  :defer t
  :commands (golden-ratio-mode))

(use-package shackle
  :config
  (shackle-mode 1))

;; End Emacs Initialization
;; Re-enable Garbage Collection
(setq gc-cons-threshold 800000)

