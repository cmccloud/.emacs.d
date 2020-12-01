;;; m-extras.el --- Personal extensions for emacs packages -*- lexical-binding: t -*-

;; Copyright (C) 2020 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs
;;; Commentary:
;;;; Code:
(require 'cl-seq)
;; Desktop and one off instances of emacs
(defun m-extras-desktop-not-loaded ()
  "Handles initial behavior of second+ emacs instance.
Added to `desktop-after-not-loaded-hook'."
  (desktop-save-mode-off)
  (setq fit-window-to-buffer-horizontally t
	fit-frame-to-buffer-margins '(300 100 300 100))
  (run-with-timer
   0 nil (lambda ()
	   (fit-frame-to-buffer)
	   ;; Ubuntu doesn't correctly open applications on the primary display
	   ;; so we offset the x-position by the width of the secondary display
	   (set-frame-position (selected-frame) (+ (/ (display-pixel-width) 2) 240) 100))))

;; Help Mode
;; Alters the behavior of how the various help mode buttons function:
;; Use display-buffer rather than pop-to-buffer
;; Kill help buffer on button press
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

(provide 'm-extras.el)

;;; m-extras.el ends here. 
