;; project-treemacs.el --- project.el for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Christopher McCloud

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple treemacs backend for project.el:
;; Treemacs Project -> project.el Project
;; Treemacs Workspace -> project.el External Roots
;; 
;; `project-find-file', `project-switch-to-buffer', `project-find-regexp' et al
;; operate on the current treemacs project.
;; 
;; `project-or-external-find-file', and `project-or-external-find-regexp'
;; operate on the current treemacs workspace.
;; 
;; TODO: Add support for project.el based ignores

;;; Code:
(require 'seq)
(require 'treemacs)
(require 'project)

(defun project-treemacs-try (dir)
  (treemacs--find-project-for-path dir))

(cl-defmethod project-root ((project treemacs-project))
  (treemacs-project->path project))

(cl-defmethod project-files ((project treemacs-project) &optional dirs)
  (seq-mapcat (lambda (d) (directory-files-recursively d ".*" nil t nil))
	      (append (list (project-root project)) dirs)))

(cl-defmethod project-external-roots ((project treemacs-project))
  (remove (project-root project)
	  (mapcar #'treemacs-project->path
		  (treemacs-workspace->projects
		   (treemacs-current-workspace)))))

(add-to-list 'project-find-functions #'project-treemacs-try)

(provide 'project-treemacs)

;; project-treemacs.el ends here
