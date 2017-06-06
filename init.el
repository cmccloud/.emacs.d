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
(setq use-package-always-defer t
      use-package-verbose t)
(add-hook 'after-init-hook
          (lambda () (message (concat "Emacs started in: " (emacs-init-time)))))

;; Libraries
(use-package dash :demand t)
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
             ("bd" . kill-this-buffer)
             ("br" . rename-buffer)
             ("ws" . split-window-right)
             ("wd" . delete-window)
             ("wm" . delete-other-windows)
             ("wv" . split-window-below)
             ("qq" . save-buffers-kill-emacs)
             ("ad" . dired)))

;; Appearance and UI
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-one-brighter-comments nil
        doom-one-brighter-modeline nil)
  
  (load-theme 'doom-one t)
  (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
  (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
  (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
  (with-eval-after-load 'neotree
    (doom-themes-neotree-config))
  (with-eval-after-load 'nlinum
    (doom-themes-nlinum-config)))

(use-package linum
  :commands linum-mode
  :preface (defvar linum-format "%4d ")
  :init
  (bind-keys :map leader-map
             ("tl" . linum-mode))
  :config
  (use-package hlinum :demand t)
  (hlinum-activate))

(use-package face-remap
  :commands (text-scale-mode
             text-scale-set
             text-scale-mode-amount))

;;; ### FROM DOOM UI CORE  AND CORE###
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     
(prefer-coding-system        'utf-8)   
(set-terminal-coding-system  'utf-8)   
(set-keyboard-coding-system  'utf-8)   
(set-selection-coding-system 'utf-8)   
(setq locale-coding-system   'utf-8)   
(setq-default buffer-file-coding-system 'utf-8)

(setq-default frame-title-format nil
              fringes-outside-margins t
              bidi-display-reordering nil
              blink-matching-paren nil
              cursor-in-non-selected-windows nil
              highlight-nonselected-windows nil
              image-animate-loop t
              indicate-buffer-boundaries nil
              indicate-empty-lines nil
              save-interprogram-paste-before-kill t
              jit-lock-defer-time nil
              jit-lock-stealth-nice 0.1
              jit-lock-stealth-time 0.2
              jit-lock-stealth-verbose nil
              compilation-scroll-output t 
              use-dialog-box nil
              visible-cursor nil
              ring-bell-function #'ignore
              visible-bell nil
              apropos-do-all t
              auto-save-default nil
              create-lockfiles nil
              history-length 1000
              make-backup-files nil)

;; Window Divider
(setq-default window-divider-default-places 'right-only
              window-divider-default-bottom-width 1
              window-divider-default-right-width 3)
(window-divider-mode 1)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(fset #'yes-or-no-p #'y-or-n-p)

;; A minor mode for toggling the mode-line
(defvar-local doom--modeline-format nil
  "The modeline format to use when `doom-hide-modeline-mode' is active. Don't
set this directly. Bind it in `let' instead.")
(defvar-local doom--old-modeline-format nil
  "The old modeline format, so `doom-hide-modeline-mode' can revert when it's
disabled.")
(define-minor-mode doom-hide-modeline-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if doom-hide-modeline-mode
      (setq doom--old-modeline-format mode-line-format
            mode-line-format doom--modeline-format)
    (setq mode-line-format doom--old-modeline-format
          doom--old-modeline-format nil))
  (force-mode-line-update))

;; Ensure major mode or theme changes wont overwrite these variables
(put 'doom--modeline-format 'permanent-local t)
(put 'doom--old-modeline-format 'permanent-local t)
(put 'doom-hide-modeline-mode 'permanent-local t)

(defun doom|hide-modeline-mode-reset ()
  "Sometimes, a major-mode is activated after `doom-hide-modeline-mode' is
activated, thus disabling it (because changing major modes invokes
`kill-all-local-variables' and specifically seems to kill `mode-line-format's
local value, whether or not it's permanent-local. Therefore, we cycle
`doom-hide-modeline-mode' to fix this."
  (when doom-hide-modeline-mode
    (doom-hide-modeline-mode -1)
    (doom-hide-modeline-mode +1)))
(add-hook 'after-change-major-mode-hook #'doom|hide-modeline-mode-reset)

;; no modeline in completion popups
(add-hook 'completion-list-mode-hook #'doom-hide-modeline-mode)

;; Bootstrap
(when (display-graphic-p)
  (push (cons 'left-fringe  4) default-frame-alist)
  (push (cons 'right-fringe 4) default-frame-alist)
  (add-hook 'emacs-startup-hook
            (lambda () (set-window-fringes (minibuffer-window) 0 0 nil)))
  (add-hook 'minibuffer-setup-hook
            (lambda () (set-window-fringes (minibuffer-window) 0 0 nil))))

;;
;; Modeline
;;

(defmacro def-modeline-segment! (name &rest forms)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst doom--prepare-modeline-segments (segments)
  (let (segs)
    (dolist (seg segments (nreverse segs))
      (push (if (stringp seg)
                seg
              (list (intern (format "doom-modeline-segment--%s" (symbol-name seg)))))
            segs))))

(defmacro def-modeline! (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it. NAME is a symbol to identify
it (used by `doom-modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `def-modeline-segment!'.

Example:
  (def-modeline! minimal
    (bar matches \" \" buffer-info)
    (media-info major-mode))
  (doom-set-modeline 'minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom--prepare-modeline-segments lhs))
        (rhs-forms (doom--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (list lhs
                 (propertize
                  " " 'display
                  `((space :align-to (- (+ right right-fringe right-margin)
                                        ,(+ 1 (string-width (format-mode-line rhs)))))))
                 rhs)))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun doom-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun doom-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (let ((modeline (doom-modeline key)))
    (when modeline
      (setf (if default
                (default-value 'mode-line-format)
              (buffer-local-value 'mode-line-format (current-buffer)))
            modeline))))

;;; ### FROM DOOM-MODELINE CONFIG ###
(line-number-mode -1)

;; all-the-icons doesn't work in the terminal, so we "disable" it.
(unless (display-graphic-p)
  (defalias 'all-the-icons-octicon    #'ignore)
  (defalias 'all-the-icons-faicon     #'ignore)
  (defalias 'all-the-icons-fileicon   #'ignore)
  (defalias 'all-the-icons-wicon      #'ignore)
  (defalias 'all-the-icons-alltheicon #'ignore))

(use-package all-the-icons
  :demand t
  :when (display-graphic-p))

(use-package eldoc-eval
  :demand t
  :config
  ;; Show eldoc in the mode-line with `eval-expression'
  (defun +doom-modeline--show-eldoc (input)
    "Display string STR in the mode-line next to minibuffer."
    (with-current-buffer (eldoc-current-buffer)
      (let* ((str              (and (stringp input) input))
             (mode-line-format (or (and str (or (doom-modeline 'eldoc) str))
                                   mode-line-format))
             mode-line-in-non-selected-windows)
        (force-mode-line-update)
        (sit-for eldoc-show-in-mode-line-delay))))

  (setq eldoc-in-minibuffer-show-fn #'+doom-modeline--show-eldoc)
  (eldoc-in-minibuffer-mode +1))

;;; Flash the mode-line on error
;; TODO More flexible colors (only suits dark themes)
;; FIXME fast key-repeat can make the mode-line bg get stuck (rare)
(defvar doom--visual-bell-old-bg nil)
(defun doom-visual-bell ()
  "Blink the mode-line red briefly."
  (unless doom--visual-bell-old-bg
    (setq doom--visual-bell-old-bg (face-background 'mode-line)))
  (set-face-background 'mode-line "#54252C")
  (run-with-timer
   0.1 nil
   (lambda () (set-face-background 'mode-line doom--visual-bell-old-bg))))
(setq ring-bell-function #'doom-visual-bell
      visible-bell nil)

;; Keep `+doom-modeline-current-window' up-to-date
(defvar +doom-modeline-current-window (frame-selected-window))
(defun +doom-modeline|set-selected-window (&rest _)
  "Sets `+doom-modeline-current-window' appropriately"
  (let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +doom-modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'+doom-modeline|set-selected-window)
(add-hook 'focus-in-hook #'+doom-modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'+doom-modeline|set-selected-window)
(advice-add #'select-window :after #'+doom-modeline|set-selected-window)


;;
;; Variables
;;

(defvar +doom-modeline-height 29
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +doom-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)


;;
;; Custom faces
;;

(defgroup +doom-modeline nil
  ""
  :group 'doom)

(defface doom-modeline-buffer-path
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the dirname part of the buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-file
  '((t (:inherit mode-line-buffer-id)))
  "Face used for the filename part of the mode-line buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-modified
  '((t (:inherit error :background nil :bold t)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the major-mode segment in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-persp-name
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the `persp-mode' perspective name and icon in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-persp-free-buffer
  '((t (:inherit error :background nil :bold t)))
  "Face used for the `persp-mode' name and icon when visiting a buffer not
added to the current perspective."
  :group '+doom-modeline)

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+doom-modeline--anzu', `+doom-modeline--evil-substitute' and
`iedit'"
  :group '+doom-modeline)

(defface doom-modeline-info
  `((t (:inherit success :bold t)))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+doom-modeline)

(defface doom-modeline-warning
  `((t (:inherit warning :bold t)))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defface doom-modeline-urgent
  `((t (:inherit error :bold t)))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+doom-modeline)

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group '+doom-modeline)

(defface doom-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+doom-modeline)

;;
;; Bootstrap
;;

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local +doom-modeline-env-version nil)
(defvar-local +doom-modeline-env-command nil)
(defun +doom-modeline|update-env ()
  (when +doom-modeline-env-command
    (let* ((default-directory (doom-project-root))
           (s (shell-command-to-string +doom-modeline-env-command)))
      (setq +doom-modeline-env-version (if (string-match "[ \t\n\r]+\\'" s)
                                           (replace-match "" t t s)
                                         s)))))
(add-hook 'focus-in-hook #'+doom-modeline|update-env)
(add-hook 'find-file-hook #'+doom-modeline|update-env)

;; Only support python and ruby for now
(add-hook 'python-mode-hook
          (lambda () (setq +doom-modeline-env-command "python --version 2>&1 | cut -d' ' -f2")))
(add-hook 'ruby-mode-hook
          (lambda () (setq +doom-modeline-env-command "ruby   --version 2>&1 | cut -d' ' -f2")))


;;
;; Modeline helpers
;;

(defsubst active ()
  (eq (selected-window) +doom-modeline-current-window))


(defvar doom-memoized-table (make-hash-table :test 'equal :size 10)
  "A lookup table containing memoized functions. The keys are argument lists,
and the value is the function's return value.")


(defun doom-memoize (name)
  "Memoizes an existing function. NAME is a symbol."
  (let ((func (symbol-function name)))
    (put name 'function-documentation
         (concat (documentation func) " (memoized)"))
    (fset name
          `(lambda (&rest args)
             (let ((key (cons ',name args)))
               (or (gethash key doom-memoized-table)
                   (puthash key (apply ',func args)
                            doom-memoized-table)))))))


(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  `(progn
     (defun ,name ,arglist ,@body)
     (doom-memoize ',name)))

(def-memoized! +doom-modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (when (display-graphic-p)
    (propertize
     " " 'display
     (let ((data (make-list height (make-list width 1)))
           (i 0)
           (color (or color "None")))
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (let ((len (length data))
               (idx 0))
           (apply #'concat
                  (mapcar #'(lambda (dl)
                              (setq idx (+ idx 1))
                              (concat
                               "\""
                               (concat
                                (mapcar #'(lambda (d)
                                            (if (eq d 0)
                                                (string-to-char " ")
                                              (string-to-char ".")))
                                        dl))
                               (if (eq idx len) "\"};" "\",\n")))
                          data))))
        'xpm t :ascent 'center)))))

(defun +doom-modeline--buffer-file ()
  "Display the base of the current buffer's filename."
  (if buffer-file-name
      (file-name-nondirectory (or buffer-file-truename (file-truename buffer-file-name)))
    "%b"))

(defun doom-project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let ((projectile-require-project-root strict-p))
    (ignore-errors (projectile-project-root))))

(defun +doom-modeline--buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename. See `doom-buffer-name' for that."
  (when buffer-file-name
    (let ((buffer-path
           (file-relative-name (file-name-directory
                                (or buffer-file-truename (file-truename buffer-file-name)))
                               (doom-project-root))))
      (unless (equal buffer-path "./")
        (let ((max-length (truncate (* (window-body-width) 0.4))))
          (if (> (length buffer-path) max-length)
              (let ((path (nreverse (split-string buffer-path "/" t)))
                    (output ""))
                (when (and path (equal "" (car path)))
                  (setq path (cdr path)))
                (while (and path (<= (length output) (- max-length 4)))
                  (setq output (concat (car path) "/" output)
                        path (cdr path)))
                (when path
                  (setq output (concat "../" output)))
                (unless (string-suffix-p "/" output)
                  (setq output (concat output "/")))
                output)
            buffer-path))))))

;;
;; Segments
;;

(def-modeline-segment! buffer-project
  "Displays `doom-project-root'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (active) 'doom-modeline-buffer-path)))
    (concat (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name (doom-project-root)))
                        'face face))))


(def-modeline-segment! buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (let* ((all-the-icons-scale-factor 1.2)
         (modified-p (buffer-modified-p))
         (active (active))
         (faces (if modified-p 'doom-modeline-buffer-modified)))
    (concat (if buffer-read-only
                (concat (all-the-icons-octicon
                         "lock"
                         :face 'doom-modeline-warning
                         :v-adjust -0.05)
                        " ")
              (when modified-p
                (concat
                 (all-the-icons-faicon "floppy-o"
                                       :face 'doom-modeline-buffer-modified
                                       :v-adjust -0.1)
                 " ")))
            (when (and buffer-file-name (not (file-exists-p buffer-file-name)))
              (concat (all-the-icons-octicon
                       "circle-slash"
                       :face 'doom-modeline-urgent
                       :v-adjust -0.05)
                      " "))
            (when-let (dir-path (+doom-modeline--buffer-path))
              (if-let (faces (or faces (if active 'doom-modeline-buffer-path)))
                  (propertize dir-path 'face `(:inherit ,faces))
                dir-path))
            (when-let (file-path (+doom-modeline--buffer-file))
              (if-let (faces (or faces (if active 'doom-modeline-buffer-file)))
                  (propertize file-path 'face `(:inherit ,faces))
                file-path)))))

(def-modeline-segment! eyebrowse-workspace
  (if (and (bound-and-true-p eyebrowse-mode)
           (s-contains-p "eyebrowse-switch" (symbol-name last-command) t))
      (let ((segment (concat (when (bound-and-true-p persp-mode) ":")
                             (int-to-string (eyebrowse--get 'current-slot))
                             ""))
            (faces (if (and (bound-and-true-p persp-mode)
                            (not (persp-contain-buffer-p)))
                       'doom-modeline-persp-free-buffer
                     'doom-modeline-persp-name)))
        (if (active)
            (propertize segment
                        'face `(:inherit ,faces))
          segment))
    ""))

(def-modeline-segment! persp-name
  "Displays the current perspective if `persp-mode' is active.
Signals whether current-buffer is a part of the current perspective."
  (when (bound-and-true-p persp-mode)
    (let* ((current (get-current-persp))
           (persp-name (if current (persp-name current) persp-nil-name))
           (active-face (if (persp-contain-buffer-p)
                            'doom-modeline-persp-name
                          'doom-modeline-persp-free-buffer))
           (segment persp-name))
      (concat (if (active)
                  (propertize segment 'face `(:inherit ,active-face))
                segment)))))

(def-modeline-segment! persp-icon
  "Displays an icon for `persp-mode' on the modeline."
  (when (bound-and-true-p persp-mode)
    (let ((active-face (if (persp-contain-buffer-p)
                           'doom-modeline-persp-name
                         'doom-modeline-persp-free-buffer))
          (icon (all-the-icons-faicon "clone"
                                      :v-adjust 0.04)))
      (concat " " (if (active)
           (propertize icon 'face `(:inherit ,active-face
                                             :height 1.2))
           (propertize icon 'face `(:height 1.2)))))))

(def-modeline-segment! buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
            (cond ((eq eol-type 0) "LF  ")
                  ((eq eol-type 1) "CRLF  ")
                  ((eq eol-type 2) "CR  ")))
          (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (sys-name (plist-get sys :name))
                 (sys-cat (plist-get sys :category)))
            (cond ((memq sys-cat '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name sys-name)))))
          "  "))


(def-modeline-segment! major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (if (stringp mode-line-process) mode-line-process)
           (if +doom-modeline-env-version (concat " " +doom-modeline-env-version))
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'doom-modeline-buffer-major-mode)))

(def-modeline-segment! major-mode-with-icons
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (all-the-icons-icon-for-mode major-mode
                                        :height 1.2
                                        :v-adjust 0.07)
           " "
           (format-mode-line mode-name)
           (if (stringp mode-line-process) mode-line-process)
           (if +doom-modeline-env-version (concat " " +doom-modeline-env-version))
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'doom-modeline-buffer-major-mode)))


(def-modeline-segment! vcs
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend (vc-backend buffer-file-name))
          (state   (vc-state buffer-file-name))
          (face    'mode-line-inactive)
          (active  (active))
          (all-the-icons-scale-factor 1.0)
          (all-the-icons-default-adjust -0.1))
      (concat +doom-modeline-vspc
              (cond ((memq state '(edited added))
                     (if active (setq face 'doom-modeline-info))
                     (all-the-icons-octicon
                      "git-branch"
                      :face face
                      :height 1.2
                      :v-adjust -0.05))
                    ((eq state 'needs-merge)
                     (if active (setq face 'doom-modeline-info))
                     (all-the-icons-octicon "git-merge" :face face))
                    ((eq state 'needs-update)
                     (if active (setq face 'doom-modeline-warning))
                     (all-the-icons-octicon "arrow-down" :face face))
                    ((memq state '(removed conflict unregistered))
                     (if active (setq face 'doom-modeline-urgent))
                     (all-the-icons-octicon "alert" :face face))
                    (t
                     (if active (setq face 'font-lock-doc-face))
                     (all-the-icons-octicon
                      "git-branch"
                      :face face
                      :height 1.2
                      :v-adjust -0.05)))
              " "
              (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                          'face (if active face))
              "  "
              +doom-modeline-vspc))))


(def-memoized! +doom-ml-icon (icon &optional text face)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat
   "  "
   (when icon
     (concat
      (all-the-icons-octicon icon :face face :height 1.0 :v-adjust 0)
      (if text " ")))
   (when text
     (propertize text 'face face))))

(def-modeline-segment! flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (+doom-ml-icon "circle-slash"
                                        (number-to-string sum)
                                        (if .error 'doom-modeline-urgent 'doom-modeline-warning))))
                   (concat
                    (+doom-ml-icon "check" nil 'doom-modeline-info) " ")))
      ('running     (+doom-ml-icon "ellipsis" "Running" 'font-lock-doc-face))
      ('no-checker  (+doom-ml-icon "alert" "-" 'font-lock-doc-face))
      ('errored     (+doom-ml-icon "alert" "Error" 'doom-modeline-urgent))
      ('interrupted (+doom-ml-icon "x" "Interrupted" 'font-lock-doc-face))
      ;; ('suspicious  "")
      )))


(defsubst doom-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(def-modeline-segment! selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (active) (or mark-active (eq evil-state 'visual)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (eq 'block evil-visual-selection))
                (let ((cols (abs (- (doom-column reg-end)
                                    (doom-column reg-beg)))))
                  (format "%dx%dB" lines cols)))
               ((eq 'line evil-visual-selection)
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL" (- (1+ reg-end) reg-beg) lines))
               (t
                (format "%dC" (- (1+ reg-end) reg-beg)))))
       'face 'doom-modeline-highlight))))

(defun +doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'doom-modeline-panel
                                     :v-adjust -0.05)
              sep))))

(defsubst +doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (cdr
                               (memq this-oc (sort (append iedit-occurrences-overlays (list))
                                                   (lambda (x y) (< (overlay-start x) (overlay-start y))))))))
                 "-")
               length))
     'face (if (active) 'doom-modeline-panel))))

(def-modeline-segment! matches
  "Displays: 1. the currently recording macro, and/or 2. the current number
of `iedit' regions."
  (let ((meta (concat (+doom-modeline--macro-recording)
                      (+doom-modeline--iedit))))
    (or (and (not (string= meta "")) meta)
        (if buffer-file-name " %I "))))

;; TODO Include other information
(def-modeline-segment! media-info
  "Metadata regarding the current file, such as dimensions for images."
  (cond ((eq major-mode 'image-mode)
         (let ((size (image-size (image-get-display-property) :pixels)))
           (format "  %dx%d  " (car size) (cdr size))))))


(def-modeline-segment! eldoc
  "Display eldoc documentation in the mode-line while using the minibuffer (e.g.
`eval-expression')."
  (bound-and-true-p str))

;; These bars regulate the height of the mode-line in GUI Emacs.
(def-modeline-segment! bar
  (+doom-modeline--make-xpm
   (face-background (if (active)
                        'doom-modeline-bar
                      'doom-modeline-inactive-bar)
                    nil t)
   +doom-modeline-height
   +doom-modeline-bar-width))

(def-modeline-segment! eldoc-bar
  "A differently colored bar, to signify an eldoc display."
  (+doom-modeline--make-xpm
   (face-background 'doom-modeline-eldoc-bar nil t)
   +doom-modeline-height
   +doom-modeline-bar-width))

(def-modeline-segment! helm-name
  (propertize
   (buffer-name (current-buffer))
   'face `(:inherit doom-modeline-buffer-file)))

(def-modeline-segment! helm-candidate-number-at-point
  (format "L%-3d" (helm-candidate-number-at-point)))

(def-modeline-segment! helm-follow
  (when (or (helm-follow-mode-p)
            (and helm-follow-mode-persistent
                 (member (assoc-default 'name (helm-get-current-source))
                         helm-source-names-using-follow)))
    (propertize "#Follow"
                'face `(:inherit doom-modeline-buffer-file))))

(def-modeline-segment! helm-marked
  (when-let
      (marked
       (and helm-marked-candidates
            (cl-loop with cur-name = (assoc-default 'name (helm-get-current-source))
                     for c in helm-marked-candidates
                     for name = (assoc-default 'name (car c))
                     when (string= name cur-name)
                     collect c)))
    (propertize
     (format "[M%d]" (length marked))
     'face `(:inherit doom-modeline-panel))))

(def-modeline-segment! helm-candidate-number
  (concat (propertize (with-helm-buffer
                        (helm-show-candidate-number
                         (car-safe helm-mode-line-string)))
                      'face '(:inherit doom-modeline-buffer-file))
          " "))
(def-modeline-segment! helm-modeline-string-segment
  helm--mode-line-string-real)

;;
;; Mode lines
;;

(def-modeline! main
  (bar matches persp-icon " " persp-name eyebrowse-workspace " " buffer-info "  %l:%c %p  " selection-info)
  (buffer-encoding vcs major-mode flycheck))

(def-modeline! eldoc
  (eldoc-bar " " eldoc)
  (media-info major-mode))

(def-modeline! minimal
  (bar matches persp-icon " " persp-name eyebrowse-workspace " " buffer-info)
  (media-info major-mode))

(def-modeline! special
  (bar matches persp-icon " " persp-name eyebrowse-workspace " " " %b   %l:%c %p  " selection-info)
  (buffer-encoding major-mode flycheck))

(def-modeline! project
  (bar persp-icon " " persp-name eyebrowse-workspace " " buffer-project)
  (major-mode))

(def-modeline! media
  (bar " %b  ")
  (media-info major-mode))

(def-modeline! helm
  (bar
   helm-marked
   " "
   helm-name
   " "
   helm-candidate-number-at-point
   helm-follow
   helm-candidate-number
   " ")
  (helm-modeline-string-segment))

;;
(doom-set-modeline 'main t)

;; helm modeline integration
;; TODO:: THIS IS HACKY
(with-eval-after-load 'helm
  (defun doom--helm-display-mode-line (source &optional force)
    (let ((force nil))
      (cond (helm-echo-input-in-header-line
             (setq force t)
             (helm--set-header-line))
            (helm-display-header-line
             (let ((hlstr (helm-interpret-value
                           (and (listp source)
                                (assoc-default 'header-line source))
                           source))
                   (endstr (make-string (window-width) ? )))
               (setq header-line-format
                     (propertize (concat " " hlstr endstr)
                                 'face 'helm-header))))))
    (doom-set-modeline 'helm)
    (when force (force-mode-line-update)))

  (advice-add 'helm-display-mode-line :after
              'doom--helm-display-mode-line))

;; Window numbering screws up modeline on load
(with-eval-after-load 'window-numbering
  (add-hook 'window-numbering-mode-hook
            (lambda () (doom-set-modeline 'main t))))

;; This scratch buffer is already created, and doesn't get a modeline. For the
;; love of Emacs, someone give the man a modeline!
(with-current-buffer "*scratch*"
  (doom-set-modeline 'main))


;;
;; Hooks
;;

(defun +doom-modeline|set-special-modeline ()
  (doom-set-modeline 'special))

(defun +doom-modeline|set-media-modeline ()
  (doom-set-modeline 'media))

(add-hook 'org-src-mode-hook #'+doom-modeline|set-special-modeline)
(add-hook 'image-mode-hook #'+doom-modeline|set-media-modeline)


(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode))

;; Packages
(use-package diminish
  :defer t
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
    "C-x h" "Helm"
    "C-x n" "Narrowing"
    "C-x r" "Registers"
    "C-x v" "Version Control"
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
    "M-m t" "Toggle"
    "M-m w" "Window")
  (which-key-mode 1))

(use-package paradox
  :commands (paradox-list-packages)
  :config
  (setq package-archive-priorities '(("melpa-stable" . 10)
                                     ("melpa" . 5)
                                     ("gnu" . 0)
                                     ("marmalade" . -5)))
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (setq paradox-lines-per-entry 1)
  (paradox-enable))

(use-package autorevert
  :demand t
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode))

(use-package hl-line
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
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

  ;; Mac Specific Config
  (when (equal system-type 'darwin)
    (when-let ((gls (executable-find "gls"))
               (ls (executable-find "ls")))
      (setq insert-directory-program gls
            dired-listing-switches "-aBhlp --group-directories-first")))
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

  (with-eval-after-load 'persp-mode
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
             ("tp" . persp-mode)
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

    (cl-defmethod helm-setup-user-source ((source helm-persp-current-buffers-source))
      (setf (slot-value source 'action)
            (append (helm-make-actions "Remove buffer(s) from current perspective."
                                       (lambda (candidate)
                                         (mapcar 'persp-remove-buffer
                                                 (helm-marked-candidates))))
                    helm-type-buffer-actions)))
    
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

    (cl-defmethod helm-setup-user-source ((source helm-persp-filtered-buffers-source))
      (setf (slot-value source 'action)
            (append (helm-make-actions "Add buffer(s) to current perspective."
                                       (lambda (candidate)
                                         (mapcar 'persp-add-buffer
                                                 (helm-marked-candidates))))
                    helm-type-buffer-actions)))

    (defvar helm-source-persp-current-buffers
      (helm-make-source "Current buffers"
          'helm-persp-current-buffers-source
        :fuzzy-match t))
    
    (defvar helm-source-persp-filtered-buffers
      (helm-make-source "Other buffers"
          'helm-persp-filtered-buffers-source
        :fuzzy-match t))

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

    ;; WIP
    (defun +helm-layouts ()
      (interactive)
      (helm
       :buffer "*helm layouts*"
       :sources '(helm-source-persp-filtered-buffers
                  helm-source-persp-current-buffers)))

    (defun persp--helm-mini (helm-mini)
      "Wrapper for helm-mini for use with `persp-mode'.
Only for use with `advice-add'."
      (with-persp-buffer-list () (funcall helm-mini)))

    (advice-add 'helm-mini :around
                #'persp--helm-mini)
    
    (bind-keys :map leader-map
               ("la" . helm-persp-add-buffers-to-perspective)
               ("lr" . helm-persp-remove-buffers-from-perspective))))

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
  (use-package smartparens-config :demand t)
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

(use-package dumb-jump
  :commands (dumb-jump-mode
             dumb-jump-go
             dumb-jump-back
             dumb-jump-quick-look)
  :config
  (setq dumb-jump-force-searcher 'ag
        dumb-jump-selector 'helm))

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
  
  (setq helm-grep-ag-command
        "rg --color=always --smart-case --no-heading --line-number %s %s %s"
        helm-buffer-max-length nil
        helm-M-x-fuzzy-match t
        helm-autoresize-max-height 30
        helm-boring-buffer-regexp-list '("\\` "
                                         "\\*helm"
                                         "\\*helm-mode"
                                         "\\*Echo Area"
                                         "\\*Minibuf"
                                         "\\*magit"
                                         "\\*lispy-goto*"
                                         "\\*Backtrace*")
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-buffer-not-found)
        helm-split-window-in-side-p t
        helm-swoop-speed-or-color t
        helm-swoop-split-with-multiple-windows t
        
        ;; Avoid slow tramp performance when using helm
        helm-buffer-skip-remote-checking t
        helm-ff-tramp-not-fancy t)

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
  (("C-s" . helm-swoop))
  :config
  (setq helm-swoop-candidate-number-limit 500))

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

(use-package helm-unicode
  :after helm
  :commands helm-unicode
  :init
  (bind-keys :map leader-map
             ("hu" . helm-unicode))
  :config
  (bind-keys :map helm-command-prefix
             ("u" . helm-unicode)))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :functions (projectile-project-root)
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
  (projectile-mode))

(use-package neotree)

(use-package stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(use-package sublimity
  :commands sublimity-mode
  :load-path "/site-lisp/sublimity"
  :init
  (with-eval-after-load 'hydra

    (defhydra hydra-minimap-scrolling
      (:foreign-keys nil :exit nil :pre (sublimity-mode +1) :post (sublimity-mode -1))
      "Page Scrolling: "
      ("n" scroll-up-command "Scroll Forwards")
      ("p" scroll-down-command "Scroll Backwards")
      ("q" nil "Quit"))

    (bind-keys ("C-v" . hydra-minimap-scrolling/scroll-up-command)
               ("M-v" .  hydra-minimap-scrolling/scroll-down-command)))
  :config
  (use-package sublimity-map :demand t
    :config
    (sublimity-map-set-delay nil)))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo)))

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

(use-package writeroom-mode
  :commands (writeroom-mode
             global-writeroom-mode)
  :init
  (bind-keys :map leader-map
             ("td" . writeroom-mode)
             ("tD" . global-writeroom-mode))
  :config
  (defun writeroom-scale-text ()
    (if (or writeroom-mode global-writeroom-mode)
        (text-scale-set 2)
      (text-scale-mode -1)
      (text-scale-set 0)))
  (add-hook 'writeroom-mode-hook #'writeroom-scale-text)
  (add-hook 'global-writeroom-mode-hook #'writeroom-scale-text))

(use-package shackle
  :demand t
  :config
  (defun shackle-release-help (func &rest args)
    "Runs func in shackle context with *Help* buffers left free."
    (let ((shackle-rules
           (--remove-first (s-equals-p (car it) "*Help*")
                           shackle-rules)))
      (apply func args)))
  (setq shackle-select-reused-windows t)
  (shackle-mode 1)
  (setq shackle-rules
        '(("*Process List*" :select t :align t :size 0.4)
          ("*Apropos*" :select t :align t :size 0.4)
          ("Outline.*pdf" :regexp t :select t :align left :size 0.3)
          ("*Geiser documentation*" :select t :align t :size 0.4)
          ("*slime-description*" :select t :align t :size 0.4)
          ("\\`\\*\[h|H]elm.*?\\*\\'" :regexp t :align t :size 0.3)
          ("*Help*" :select t :align t :size 0.4)
          ("*Completions*" :select t :align t :size 0.4)
          ("*Compile-Log*" :select t :align t :size 0.4)
          ("*Man.*" :regexp t :select t :align t :size 0.4)
          ("*lispy-goto*" :align t :size 0.4)
          ("*git-gutter:diff*" :align bottom :size 0.3)))

  (with-eval-after-load 'helm
    (advice-add 'helm-execute-persistent-action
                :around
                #'shackle-release-help)))

;; End Emacs Initialization
;; Re-enable Garbage Collection
(setq gc-cons-threshold 800000)
