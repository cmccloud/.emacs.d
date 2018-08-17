;;; helm-persp.el --- Helm source for persp-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Christopher McCloud

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-source)
(require 'helm-types)
(require 'helm-buffers)
(require 'persp-mode)

(defvar helm-persp--current-buffers-cache)
(defvar helm-persp--filtered-buffers-cache)
(defvar helm-source-persp-current-buffers)
(defvar helm-source-persp-filtered-buffers)

(defvar helm-persp-perspectives-source
  (helm-build-sync-source "Perspectives"
    :candidates #'persp-names
    :action (helm-make-actions
              "Switch to Perspective"
              (lambda (c) (persp-switch c))
              "Kill Perspective"
              (lambda (c) (persp-kill c))
              "Create New Perspective"
              (lambda (_c) (persp-switch
                           (persp-name
                            (call-interactively 'persp-add-new)))))))

(defvar helm-persp-new-persp-source
  (helm-build-dummy-source "Create Perspective"
    :action (helm-make-actions
             "Create New Perspective"
             (lambda (c) (persp-switch
                          (persp-name
                           (persp-add-new c)))))))

(defun helm-persp-disjunct-buffer (buffer)
  "Removes or adds BUFFER from/to current perspective.

If BUFFER is a member of `get-current-persp', removes BUFFER from perspective,
otherwise BUFFER is added to the current perspective."
  (when (bufferp buffer)
    (if (persp-contain-buffer-p buffer)
        (persp-remove-buffer buffer)
      (persp-add-buffer buffer))))

(defun helm-persp--buffers-init ()
  (setq helm-persp--current-buffers-cache
        (helm-skip-boring-buffers
         (mapcar #'buffer-name (persp-buffer-list-restricted nil 0))
         nil)
        helm-persp--filtered-buffers-cache
        (helm-skip-boring-buffers
         (mapcar #'buffer-name (persp-buffer-list-restricted nil 1))
         nil))
  (let ((result (cl-loop for b in (append helm-persp--current-buffers-cache
                                          helm-persp--filtered-buffers-cache)
                         maximize (length b) into len-buf
                         maximize (length (with-current-buffer b
                                            (symbol-name major-mode)))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value  'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defclass helm-persp-current-buffers-source (helm-source-sync helm-type-buffer)
  ((init
    :initform #'helm-persp--buffers-init)
   (candidates
    :initform helm-persp--current-buffers-cache)
   (matchplugin :initform nil)
   (match :initform #'helm-buffers-match-function)
   (persistent-action :initform #'helm-buffers-list-persistent-action)
   (volatile :initform t)
   (keymap :initform helm-buffer-map)))

(cl-defmethod helm--setup-source ((source helm-persp-current-buffers-source))
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (append (helm-make-actions "Remove buffer(s) from current Perspective."
                                   (lambda (_c)
                                     (mapc #'helm-persp-disjunct-buffer
                                           (helm-marked-candidates :all-sources t))))
                (symbol-value (slot-value source 'action)))))

(defclass helm-persp-filtered-buffers-source (helm-source-sync helm-type-buffer)
  ((init
    :initform #'helm-persp--buffers-init)
   (candidates
    :initform helm-persp--filtered-buffers-cache)
   (matchplugin :initform nil)
   (match :initform #'helm-buffers-match-function)
   (persistent-action :initform #'helm-buffers-list-persistent-action)
   (volatile :initform t)
   (keymap :initform helm-buffer-map)))

(cl-defmethod helm--setup-source ((source helm-persp-filtered-buffers-source))
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (append (helm-make-actions "Add buffer(s) to current Perspective."
                                   (lambda (_c)
                                     (mapc #'helm-persp-disjunct-buffer
                                           (helm-marked-candidates :all-sources t))))
                (symbol-value (slot-value source 'action)))))

;;;###autoload
(defun helm-persp-layouts ()
  (interactive)
  (if (not persp-mode)
      (message "Persp-mode not enabled. Use M-x `persp-mode'.")
    (unless (bound-and-true-p helm-source-persp-current-buffers)
      (setq helm-source-persp-current-buffers
            (helm-make-source "Current Buffers"
                helm-persp-current-buffers-source)))

    (unless (bound-and-true-p helm-source-persp-filtered-buffers)
      (setq helm-source-persp-filtered-buffers
            (helm-make-source "Other Buffers"
                helm-persp-filtered-buffers-source)))
    (helm
     :buffer "*helm layouts*"
     :sources '(helm-persp-perspectives-source
                helm-source-persp-current-buffers
                helm-source-persp-filtered-buffers
                helm-persp-new-persp-source))))

;; helm-persp.el ends here.
