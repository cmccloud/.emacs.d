;; Supress Garbage Collection During Initialization
(setq gc-cons-threshold 100000000)
;; Begin Emacs Initialization
;; Load Customization Settings
(load (expand-file-name "custom.el" user-emacs-directory) nil t)

;; Extend Load Path
(eval-and-compile
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Package Initialization
(package-initialize)
(require 'use-package)

;; Libraries
(use-package dash)
(use-package s)
(use-package seq)
(use-package map :defer t)
(use-package dash-functional :defer t)
(use-package request :defer t)
(use-package deferred :defer t)
(use-package f :defer t)
(use-package hydra)

;; Appearance and UI
(setq doom-one-brighter-comments t)
(load-theme 'spacemacs-dark)
(setq frame-title-format nil)

(use-package page-break-lines
  :diminish 'page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(use-package spaceline-config
  :disabled t
  :config
  (spaceline-helm-mode 1)
  (spaceline-info-mode 1))

(use-package all-the-icons
  :after spaceline)

(use-package spaceline-all-the-icons
  ;; Serious performance issues
  :after spaceline
  :config
  (setq spaceline-all-the-icons-icon-set-flycheck-slim 'dots
        spaceline-all-the-icons-icon-set-git-ahead 'commit
        spaceline-all-the-icons-icon-set-window-numbering 'square
        spaceline-all-the-icons-flycheck-alternate t
        spaceline-all-the-icons-highlight-file-name t
        spaceline-all-the-icons-separator-type 'none)
  (spaceline-all-the-icons-theme))

;; Packages
(use-package bind-key
  :config
  (define-prefix-command 'leader-map)
  (bind-keys ("M-u" . undo)
             ("C-x C-c" . nil)
             ("M-m" . leader-map)
             ("M-n" . next-buffer)
             ("M-p" . previous-buffer))
  (bind-keys :map leader-map
             ("tF" . toggle-frame-fullscreen)
             ("bd" . kill-this-buffer)
             ("br" . rename-buffer)
             ("ws" . split-window-right)
             ("wd" . delete-window)
             ("wm" . delete-other-windows)
             ("wv" . split-window-below)
             ("qq" . save-buffers-kill-emacs)
             ("ad" . dired)))

(use-package diminish
  :defer t
  :config
  (diminish 'visual-line-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-add-key-based-replacements
    "M-m b" "Buffers" 
    "M-m f" "Files"
    "M-m a" "Applications"
    "M-m g" "Git"
    "M-m gg" "Gist"
    "M-m h" "Helm"
    "M-m hd" "Describe"
    "M-m hl" "Locate"
    "M-m l" "Layouts"
    "M-m p" "Projects"
    "M-m q" "Quit"
    "M-m s" "Search"
    "M-m t" "Toggle"
    "M-m w" "Window")
  (which-key-mode 1))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package hl-line
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'hl-line-mode)
  :bind
  (:map leader-map
        ("th" . hl-line-mode)
        ("tH" . global-hl-line-mode)))

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
  :defer t
  :commands (term
             ansi-term)
  :init
  (bind-keys :map leader-map
             ("at" . ansi-term))
  :config
  (bind-keys :map term-raw-map
             ("M-x" . 'execute-extended-command)
             ("M-m" . nil))
  (with-eval-after-load 'helm
    (bind-keys :map term-raw-map
               ("M-x" . helm-M-x))))

(use-package eshell
  :defer t
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
  :defer t
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
  :defer t
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
  (eyebrowse-mode))

(use-package persp-mode
  :init
  (defvar persp-timed-auto-save-enable t
    "If t, persp-mode will save perspectives to file every
`persp-mode-timed-auto-save-interval seconds. Nil to disable.")
  (defvar persp-timed-auto-save-interval 600
    "Interval, in seconds, between persp-mode auto-saves if
`persp-timed-auto-save-enable is t.")
  (defvar persp--timed-auto-save-handler nil
    "Reference to handler for `persp-timed-auto-save")
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
  
  (setq persp-nil-name "@Nil"
        persp-add-buffer-on-find-file t
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

  ;; Integrations with other buffer management tools
  (advice-add 'next-buffer :around
              (lambda (next-buffer)
                (with-persp-buffer-list () (funcall next-buffer))))
  (advice-add 'previous-buffer :around
              (lambda (previous-buffer)
                (with-persp-buffer-list () (funcall previous-buffer))))
  
  (with-eval-after-load 'helm-buffers
    (defvar helm-persp-current-buffers-cache nil)
    (defvar helm-persp-filtered-buffers-cache nil)

    (defclass helm-persp-current-buffers-source (helm-source-buffers)
      ((candidates
        :initform #'(lambda ()
                      (if helm-persp-current-buffers-cache
                          helm-persp-current-buffers-cache
                        (setq helm-persp-current-buffers-cache
                              (mapcar #'buffer-name
                                      (persp-buffer-list-restricted nil 0))))))
       (cleanup
        :initform #'(lambda () (setq helm-persp-current-buffers-cache nil)))))
    (defclass helm-persp-filtered-buffers-source (helm-source-buffers)
      ((candidates
        :initform #'(lambda ()
                      (if helm-persp-filtered-buffers-cache
                          helm-persp-filtered-buffers-cache
                        (setq helm-persp-filtered-buffers-cache
                              (mapcar #'buffer-name
                                      (persp-buffer-list-restricted nil 1))))))
       (cleanup
        :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil)))))

    (defvar helm-source-persp-current-buffers
      (helm-make-source "Current buffers"
          'helm-persp-current-buffers-source
        :fuzzy-match t))
    (defvar helm-source-persp-filtered-buffers
      (helm-make-source "Other buffers"
          'helm-persp-filtered-buffers-source
        :fuzzy-match t))

    (helm-add-action-to-source "Remove buffer(s) from current perspective."
                               (lambda (candidate)
                                 (mapcar 'persp-remove-buffer
                                         (helm-marked-candidates)))
                               helm-source-persp-current-buffers 0) 
    (helm-add-action-to-source "Add buffer(s) to current perspective."
                               (lambda (candidate)
                                 (mapcar 'persp-add-buffer
                                         (helm-marked-candidates)))
                               helm-source-persp-filtered-buffers 0)

    (defun helm-persp-add-buffers-to-perspective ()
      (interactive)
      (helm
       :buffer "*helm perspectives add buffer*"
       :sources '(helm-source-persp-filtered-buffers)))

    (defun helm-persp-remove-buffers-from-perspective ()
      (interactive)
      (helm
       :buffer "*helm perspectives remove buffer*"
       :sources '(helm-source-persp-current-buffers)))

    (defun persp--helm-mini (helm-mini)
      "Wrapper for helm-mini for use with `persp-mode'.
Only for use with `advice-add'."
      (with-persp-buffer-list () (funcall helm-mini)))

    (advice-add 'helm-mini :around
                #'persp--helm-mini)
    
    (bind-keys :map leader-map
               ("la" . helm-persp-add-buffers-to-perspective)
               ("lr" . helm-persp-remove-buffers-from-perspective)))

  (bind-keys :map leader-map
             ("tp" . persp-mode)
             ("ls" . persp-switch)
             ("la" . persp-add-buffer)
             ("lr" . persp-remove-buffer)))

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
  :disabled t
  :defer 5
  :init
  (add-hook 'emacs-lisp-mode-hook #'semantic-mode)
  :config
  (semantic-default-elisp-setup))

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
  :defer t
  :commands (smartparens-mode
             smartparens-strict-mode
             show-smartparens-mode
             smartparens-global-mode
             smartparens-global-strict-mode
             show-smartparens-global-mode)
  :defines (sp-activate)
  :init
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  (add-hook 'html-mode-hook #'smartparens-mode)
  (with-eval-after-load 'web-mode
    (add-hook 'web-mode-hook #'smartparens-mode))
  (with-eval-after-load 'markdown-mode
    (add-hook 'markdown-mode-hook #'smartparens-mode))
  :config
  (use-package smartparens-config)
  (show-smartparens-global-mode))

(use-package expand-region
  :defer t
  :commands (er/expand-region)
  :init
  (bind-keys ("C-r" . er/expand-region)))

(use-package lispy
  :defer t
  :commands (lispy-mode
             lispy-mark-symbol
             lispy-move-beginning-of-line
             lispy-move-end-of-line)
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
             ("C-0" . lispy-describe-inline)
             ("M-m" . nil)
             ("M-n" . nil)
             (":" . self-insert-command)
             ("[" . self-insert-command)
             ("]" . self-insert-command))
  (bind-keys :map leader-map
             ("M-m" . lispy-mark-symbol))
  ;; Prevent semantic mode errors when using
  ;; lispy goto
  (advice-add 'special-lispy-goto
              :after
              (defun lispy--supress-semantic ()
                (semantic-mode -1)))
  :bind
  (("C-a" . lispy-move-beginning-of-line)
   ("C-e" . lispy-move-end-of-line)))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :bind
  (("C-c C-d" . elisp-slime-nav-describe-elisp-thing-at-point)))

(use-package clojure-mode
  :defer t
  :config
  (with-eval-after-load 'lispy
    (load (expand-file-name
           "site-lisp/clojure-semantic/clojure.el"
           user-emacs-directory))))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'haskell-doc-mode))

(use-package intero
  :defer t
  :after (haskell-mode)
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package shm
  :defer t
  :commands (structured-haskell-mode))

(use-package cider
  :defer t
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
  :defer t
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
  :defer t
  :commands (avy-goto-word-or-subword-1
             avy-goto-line
             avy-goto-char
             avy-goto-char-2)
  :init
  (bind-keys :map leader-map
             ("j" . avy-goto-word-or-subword-1)
             ("z" . avy-goto-line))
  :bind
  (("C-j" . avy-goto-word-or-subword-1)
   ("C-z" . avy-goto-line)))

(use-package helm
  :defer t
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
             ("hll" . helm-locate-library))
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
  
  (setq helm-M-x-fuzzy-match nil
        helm-autoresize-max-height 30
        helm-boring-buffer-regexp-list '("\\` "
                                         "\\*helm"
                                         "\\*helm-mode"
                                         "\\*Echo Area"
                                         "\\*Minibuf"
                                         "\\*magit"
                                         "\\*lispy-goto*"
                                         "\\*Backtrace*")
        helm-ff-tramp-not-fancy nil
        helm-locate-command "mdfind -name %s %s"
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-buffer-not-found)
        helm-split-window-in-side-p t
        helm-swoop-speed-or-color t
        helm-swoop-split-with-multiple-windows t)
  (use-package helm-config)
  (helm-mode 1)
  
  (bind-keys :map helm-map
             ("C-z" . helm-select-action)
             ("<tab>" . helm-execute-persistent-action)
             ("TAB" . helm-execute-persistent-action)
             ("C-M-n" . helm-scroll-other-window)
             ("C-M-p" . helm-scroll-other-window-down)))

(use-package helm-projectile
  :defer t
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
             ("pb" . helm-projectile-switch-to-buffer)))

(use-package helm-ag
  :after (helm)
  :defer t
  :commands (helm-do-ag
             helm-do-ag-project-root)
  :init
  (bind-keys :map leader-map
             ("ss" . helm-do-ag)
             ("sp" . helm-do-ag-project-root)))

(use-package helm-swoop
  :after (helm)
  :defer t
  :bind
  (("C-s" . helm-swoop)))

(use-package helm-descbinds
  :after (helm)
  :defer t
  :config
  (setq helm-descbinds-window-style 'split-window)
  :bind
  (("C-h b" . helm-descbinds)))

(use-package helm-themes
  :after (helm)
  :defer t)

(use-package helm-dash
  :disabled t
  :defer t
  :commands (helm-dash)
  :after (helm)
  :config
  (bind-keys :map leader-map
             ("hdd" . helm-dash)))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :commands (projectile-mode
             projectile-ack
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
  :defer t
  :commands (magit-mode
             magit-status
             magit-commit-popup
             magit-stage-file
             magit-unstage-file
             magit-push-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-commit)
  :init
  (bind-keys :map leader-map
             ("gs" . magit-status))
  :config
  (bind-keys :map magit-process-mode-map
             ("M-n" . nil)
             ("M-p" . nil))
  (bind-keys :map magit-diff-mode-map
             ("M-n" . nil)
             ("M-p" . nil))
  (bind-keys :map magit-status-mode-map
             ("M-n" . nil)
             ("M-p" . nil)))

(use-package magit-gh-pulls
  :defer t
  :after (magit)
  :commands (turn-on-magit-gh-pulls)
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package git-gutter
  :diminish 'git-gutter-mode 
  :config
  (global-git-gutter-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook
              #'git-gutter:update-all-windows))
  (with-eval-after-load 'hydra
    (defhydra hydra-git-gutter (:columns 3 :exit nil :foreign-keys warn)
      "Git Gutter"
      ("s" git-gutter:stage-hunk "Stage Hunk")
      ("n" git-gutter:next-hunk "Next Hunk")
      ("p" git-gutter:previous-hunk "Previous Hunk")
      ("r" git-gutter:revert-hunk "Revert Hunk")
      ("c" magit-commit-popup "Commit" :color blue )
      ("q" nil "Cancel" :color blue))
    (bind-keys :map leader-map
               ("gg" . hydra-git-gutter/body)))
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))

(use-package git-gutter-fringe
  :after git-gutter
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

(use-package fringe-helper
  :disabled t)

(use-package gist
  :defer t
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
  :defer t
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
  :after (company)
  :defer t)

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
  :defer t
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package web-mode
  :defer t
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
  :defer t
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
    (add-hook 'js2-mode-hook #'company-mode t))

  (defun chrome-refresh-current-tab ()
    (interactive)
    (do-applescript
     "tell application \"Google Chrome\" to reload active tab of window 1"))
  (bind-keys :map leader-map
             ("<f5>" . chrome-refresh-current-tab)))

(use-package nodejs-repl
  :defer t
  :after (js2-mode)
  :commands (nodejs-repl-mode
             nodejs-repl-send-last-sexp
             nodejs-repl-send-region
             nodejs-repl-send-buffer)
  :init
  (defun nodejs-repl-eval-dwim ()
    (interactive)
    (if mark-active
        (nodejs-repl-send-region (region-beginning)
                                 (region-end))
      (nodejs-repl-send-last-sexp)))
  (with-eval-after-load 'js2-mode
    (bind-keys :map js2-mode-map
               ("C-x C-e" . nodejs-repl-eval-dwim)
               ("C-c C-k" . nodejs-repl-send-buffer))))

(use-package skewer-mode
  :defer t
  :commands (skewer-mode
             skewer-html-mode
             skewer-css-mode)
  :defines (skewer-html-mode-map)
  :init
  (with-eval-after-load 'skewer-html
    (bind-keys :map skewer-html-mode-map
               ("C-x C-e" . skewer-html-eval-tag))))

(use-package impatient-mode
  :defer t
  :init
  (add-hook 'html-mode-hook #'impatient-mode)
  (add-hook 'web-mode-hook #'impatient-mode)
  (add-hook 'css-mode-hook #'impatient-mode))

(use-package tern
  :diminish tern-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package window-numbering
  :commands (select-window-1
             select-window-2
             select-window-3
             select-window-4
             select-window-5)
  :init
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
  (bind-keys :map leader-map
             ("ws" . split-and-balance-window-right)
             ("wd" . delete-window-and-balance))
  :config
  (window-numbering-mode 1)
  :bind
  (("M-1" . select-window-1)
   ("M-2" . select-window-2)
   ("M-3" . select-window-3)
   ("M-4" . select-window-4)
   ("M-5" . select-window-5)))

(use-package winner
  :defer t
  :commands (winner-undo
             winner-redo)
  :init
  (bind-keys :map leader-map
             ("wu" . winner-undo)
             ("wr" . winner-redo))
  :config
  (winner-mode))

(use-package golden-ratio
  :defer t
  :diminish golden-ratio-mode
  :commands (golden-ratio-mode)
  :init
  (defun toggle-golden-ratio ()
    "Toggles `golden-ratio-mode' and balances windows."
    (interactive)
    (if golden-ratio-mode
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode 1)))
  (bind-keys :map leader-map
             ("tg" . toggle-golden-ratio)))

(use-package writeroom
  :commands (writeroom-mode
             global-writeroom-mode)
  :init
  (bind-keys :map leader-map
             ("td" . writeroom-mode)
             ("tD" . global-writeroom-mode))
  (defun writeroom-scale-text ()
    (if (or writeroom-mode global-writeroom-mode)
        (text-scale-set 2)
      (text-scale-mode -1)))
  (add-hook 'writeroom-mode-hook #'writeroom-scale-text)
  (add-hook 'global-writeroom-mode-hook #'writeroom-scale-text))

(use-package shackle
  :config
  (defun shackle-release-help (func &rest args)
    "Runs func in shackle context with *Help* buffers left free."
    (let ((shackle-rules
           (--remove-first (s-equals-p (car it) "*Help*")
                           shackle-rules)))
      (apply func args)))
  (shackle-mode 1)
  (setq shackle-rules
        '(("*Process List*" :select t :align t :size 0.4)
          ("*Apropos*" :select t :align t :size 0.4)
          ("Outline.*pdf" :regexp t :select t :align (quote left) :size 0.3)
          ("*Geiser documentation*" :select t :align t :size 0.4)
          ("*slime-description*" :select t :align t :size 0.4)
          ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
          ("*Help*" :select t :align t :size 0.4)
          ("*Completions*" :select t :align t :size 0.4)
          ("*Compile-Log*" :select t :align t :size 0.4)
          ("*Man.*" :regexp t :select t :align t :size 0.4)
          ("*lispy-goto*" :align (quote below) :size 0.4)))

  (with-eval-after-load 'helm
    (advice-add 'helm-execute-persistent-action
                :around
                #'shackle-release-help)))

;; End Emacs Initialization
;; Re-enable Garbage Collection
(setq gc-cons-threshold 800000)
