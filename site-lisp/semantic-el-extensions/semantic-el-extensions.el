;; semantic-el-extensions.el --- Extends bovine/el -*- lexical-binding: t -*-

;; Copyright (C) 2018 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;;; Code:
(require 'semantic/bovine/el)

(semantic-elisp-setup-form-parser
    (lambda (form _start _end)
      (let ((name (nth 1 form)))
        (semantic-tag-new-include
         (symbol-name (if (eq (car-safe name) 'quote)
                          (nth 1 name)
                        name))
         nil
         '(:directory (when-let ((path (plist-get form :load-path)))
                        (expand-file-name path))))))
  use-package)

(semantic-elisp-setup-form-parser
    (lambda (form _start _end)
      (semantic-tag-new-function
       (symbol-name (nth 1 form))
       "Minor Mode"
       nil))
  define-minor-mode)

(semantic-elisp-setup-form-parser
    (lambda (form _start _end)
      (let ((name (nth 1 form))
            (doc (semantic-elisp-form-to-doc-string (nth 3 form))))
        (semantic-tag-new-variable
         (symbol-name (if (eq (car-safe name) 'quote)
                          (nth 1 name)
                        name))
         nil
         (nth 2 form)
         :user-visible-flag (and doc
                                 (> (length doc) 0)
                                 (= (aref doc 0) ?*))
         :documentation (semantic-elisp-do-doc doc))))
  customize-set-variable)

(semantic-elisp-reuse-form-parser defun cl-defun)

(semantic-elisp-reuse-form-parser defmethod cl-defmethod cl-defgeneric)

(provide 'semantic-el-extensions)

;;; semantic-el-extensions.el ends here
