;;; ui/doom-modeline/doom-modeline.el -*- lexical-binding: t; -*-
(require 'subr-x)
(when (display-graphic-p)
  (require 'all-the-icons))

(unless (display-graphic-p)
  (defalias 'all-the-icons-octicon    #'ignore)
  (defalias 'all-the-icons-faicon     #'ignore)
  (defalias 'all-the-icons-fileicon   #'ignore)
  (defalias 'all-the-icons-wicon      #'ignore)
  (defalias 'all-the-icons-alltheicon #'ignore))

;; A minor mode for toggling the mode-line
(defvar-local doom--modeline-format nil
  "The modeline format to use when `doom-hide-modeline-mode' is active. Don't
set this directly. Bind it in `let' instead.")
(defvar-local doom--old-modeline-format nil
  "The old modeline format, so `doom-hide-modeline-mode' can revert when it's
disabled.")

(defvar doom-saved-mode-line-format nil
  "The value of `mode-line-format' prior to enabling `doom-modeline-mode'.
Restored on exiting mode.")

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


;; (use-package eldoc-eval
;;   :demand t
;;   :config
;;   ;; Show eldoc in the mode-line with `eval-expression'
;;   (defun +doom-modeline--show-eldoc (input)
;;     "Display string STR in the mode-line next to minibuffer."
;;     (with-current-buffer (eldoc-current-buffer)
;;       (let* ((str              (and (stringp input) input))
;;              (mode-line-format (or (and str (or (doom-modeline 'eldoc) str))
;;                                    mode-line-format))
;;              mode-line-in-non-selected-windows)
;;         (force-mode-line-update)
;;         (sit-for eldoc-show-in-mode-line-delay))))

;;   (setq eldoc-in-minibuffer-show-fn #'+doom-modeline--show-eldoc)
;;   (eldoc-in-minibuffer-mode +1))

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
    (unless (or (minibuffer-window-active-p win)
                (bound-and-true-p helm-alive-p))
      (setq +doom-modeline-current-window win))))




;;
;; Variables
;;

(defvar +doom-modeline-height (round (* (default-line-height) 1.4))
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

(defface doom-modeline-helm-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active helm window."
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
  (ignore-errors (locate-dominating-file default-directory ".git")))

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
                         (+doom-ml-icon
                          "circle-slash"
                          (number-to-string sum)
                          (if .error
                              'doom-modeline-urgent
                            'doom-modeline-warning))))
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

(def-modeline-segment! helm-bar
  (+doom-modeline--make-xpm
   (face-background 'doom-modeline-helm-bar nil t)
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
            (cl-loop with cur-name = (assoc-default 'name
                                                    (helm-get-current-source))
                     for c in helm-marked-candidates
                     for name = (assoc-default 'name (car c))
                     when (string= name cur-name)
                     collect c)))
    (propertize
     (format "[M%d]" (length marked))
     'face `(:inherit doom-modeline-panel))))

(def-modeline-segment! helm-candidate-number
  (when-let (candidate (with-current-buffer (helm-buffer-get)
                       (helm-show-candidate-number
                        (car-safe helm-mode-line-string))))
      (concat
       (propertize candidate 'face '(:inherit doom-modeline-buffer-file))
       " ")))

(def-modeline-segment! helm-modeline-string-segment
  helm--mode-line-string-real)

(def-modeline-segment! helm-header-line-segment
  (when-let ((source (helm-get-current-source)))
    (propertize (helm-interpret-value (and (listp source)
                                           (assoc-default 'header-line source))
                                      source))))

;;
;; Mode lines
;;

(def-modeline! main
  (bar
   matches
   persp-icon
   " "
   persp-name
   eyebrowse-workspace
   " "
   buffer-info
   "  %l:%c %p  "
   selection-info)
  (buffer-encoding vcs major-mode flycheck))

(def-modeline! eldoc
  (eldoc-bar " " eldoc)
  (media-info major-mode))

(def-modeline! minimal
  (bar matches persp-icon " " persp-name eyebrowse-workspace " " buffer-info)
  (media-info major-mode))

(def-modeline! special
  (bar
   matches
   persp-icon
   " "
   persp-name
   eyebrowse-workspace
   " "
   " %b   %l:%c %p  "
   selection-info)
  (buffer-encoding major-mode flycheck))

(def-modeline! project
  (bar persp-icon " " persp-name eyebrowse-workspace " " buffer-project)
  (major-mode))

(def-modeline! media
  (bar " %b  ")
  (media-info major-mode))

(def-modeline! helm
  (helm-bar
   helm-marked
   " "
   helm-name
   " "
   helm-candidate-number-at-point
   helm-follow
   helm-candidate-number
   " ")
  (helm-header-line-segment))
(defun +doom-modeline|set-main-modeline ()
  (doom-set-modeline 'main t))

(defun +doom-modeline|set-special-modeline ()
  (doom-set-modeline 'special))

(defun +doom-modeline|set-media-modeline ()
  (doom-set-modeline 'media))

;;
;; Hooks and Initialization
;;
(defvar doom-modeline-show-helm-modeline t
  "Whether doom-modeline-mode should display the modeline in helm buffers.")

(defun doom--helm-display-mode-line (&optional _source _force)
  (if doom-modeline-mode
      (with-helm-buffer
        (if doom-modeline-show-helm-modeline
            (doom-set-modeline 'helm)
          (setq mode-line-format nil)))))

;;;###autoload
(define-minor-mode doom-modeline-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  (if doom-modeline-mode
      (progn
        ;; Save current Modeline
        (setq doom-saved-mode-line-format mode-line-format)
        ;; Recalculate height based on font
        (setq +doom-modeline-height (round (* (default-line-height) 1.4)))
        ;; Maintains selected window
        (add-hook 'window-configuration-change-hook #'+doom-modeline|set-selected-window)
        (add-hook 'focus-in-hook #'+doom-modeline|set-selected-window)
        (advice-add #'handle-switch-frame :after #'+doom-modeline|set-selected-window)
        (advice-add #'select-window :after #'+doom-modeline|set-selected-window)
        
        ;; Keep the Environment up to date
        (add-hook 'focus-in-hook #'+doom-modeline|update-env)
        (add-hook 'find-file-hook #'+doom-modeline|update-env)
        ;; Hacks for various modes
        ;; Both add-hook and advice-add work on uninitialized variables.
        (add-hook 'org-src-mode-hook #'+doom-modeline|set-special-modeline)
        (add-hook 'image-mode-hook #'+doom-modeline|set-media-modeline)
        (add-hook 'window-numbering-mode-hook #'+doom-modeline|set-main-modeline)
        (add-hook 'helm-after-initialize-hook #'doom--helm-display-mode-line)
        (advice-add 'helm-display-mode-line :after #'doom--helm-display-mode-line)
        ;; Finally set the modeline
        (doom-set-modeline 'main t)
        ;; For Scratch buffer too, in we want doom-modeline-mode enabled at startup.
        (when-let ((scratch (get-buffer "*scratch*")))
          (with-current-buffer scratch
            (doom-set-modeline 'main)))
        (force-mode-line-update))
    ;;
    (remove-hook 'window-configuration-change-hook #'+doom-modeline|set-selected-window)
    (remove-hook 'focus-in-hook #'+doom-modeline|set-selected-window)
    (advice-remove #'handle-switch-frame #'+doom-modeline|set-selected-window)
    (advice-remove #'select-window #'+doom-modeline|set-selected-window)
    (remove-hook 'focus-in-hook #'+doom-modeline|update-env)
    (remove-hook 'find-file-hook #'+doom-modeline|update-env)
    (remove-hook 'org-src-mode-hook #'+doom-modeline|set-special-modeline)
    (remove-hook 'image-mode-hook #'+doom-modeline|set-media-modeline)
    (remove-hook 'window-numbering-mode-hook #'+doom-modeline|set-main-modeline)
    (remove-hook 'helm-after-initialize-hook #'doom--helm-display-mode-line)
    (advice-remove 'helm-display-mode-line #'doom--helm-display-mode-line)
    (setq-default mode-line-format doom-saved-mode-line-format)
    (when-let ((scratch (get-buffer "*scratch*")))
          (with-current-buffer scratch
            (setq mode-line-format doom-saved-mode-line-format)))
    (force-mode-line-update)))


(provide 'doom-modeline)

;;; END DOOM MODELINE 
