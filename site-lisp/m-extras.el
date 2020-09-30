;;; m-extras.el --- Personal extensions for emacs packages -*- lexical-binding: t -*-

;; Copyright (C) 2020 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs
;;; Commentary:
;;;; Code:
;; Desktop and one off instances of emacs
(require 'cl-seq)

(defun m-extras-desktop-not-loaded ()
  "Handles initial behavior of second+ emacs instance.
Added to `desktop-after-not-loaded-hook'."
  (desktop-save-mode-off)
  (setq fit-window-to-buffer-horizontally t
	fit-frame-to-buffer-margins '(300 100 300 100))
  (run-with-timer
   0 nil (lambda ()
	   (fit-frame-to-buffer)
	   (set-frame-position (selected-frame) 2800 100))))

;; Help Mode
;; Alters the behavior of how the various help mode buttons function
(gv-define-simple-setter button-type-get button-type-put)

(defun help-mode-button-advice (oldfun &rest args)
  (cl-letf* ((window (selected-window))
             ((symbol-function 'pop-to-buffer)
              (lambda (buffer-or-name &optional _action _no-record)
		(kill-buffer (current-buffer))
                (select-window
                 (display-buffer-same-window buffer-or-name nil)))))
    (apply oldfun args)))

  (dolist (type '(help-function-def help-variable-def help-face-def))
    (add-function
     :around
     (button-type-get type 'help-function)
     #'help-mode-button-advice))

(with-eval-after-load 'cl-extra
  (add-function
   :around
   (button-type-get 'cl-type-definition 'help-function)
   #'help-mode-button-advice))

;; Imenu
(defun m-extras-imenu-elisp-extras ()
  "Recognize `use-package' in imenu when in emacs-lisp-mode."
  (add-to-list
   'imenu-generic-expression
   '("Package" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2) t))

;; Helm Imenu
(with-eval-after-load 'helm-imenu
  (add-to-list 'helm-imenu-type-faces '("^Package$" . font-lock-type-face)))

;; Helm
;; FIX: Helm Buffer List allows duplicates (as of when?)
(define-advice helm-buffers-get-visible-buffers
    (:override (&rest r) remove-duplicates)
  (let (result)
    (walk-windows
     (lambda (x)
       (cl-pushnew (buffer-name (window-buffer x)) result))
     nil 'visible)
    result))

(provide 'm-extras.el)

;;; m-extras.el ends here. 
