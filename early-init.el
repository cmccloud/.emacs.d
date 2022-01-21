;;; early-init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;; Commentary:
;; Emacs 27+ introduced early-init.el as a result of changes to the built in
;; package management system. From Emacs News:
;; 
;;   ** Emacs can now be configured using an early init file.
;;   The file is called 'early-init.el', in 'user-emacs-directory'.  It is
;;   loaded very early in the startup process: before graphical elements
;;   such as the tool bar are initialized, and before the package manager
;;   is initialized.  The primary purpose is to allow customizing how the
;;   package system is initialized given that initialization now happens
;;   before loading the regular init file (see below).
;; 
;; For the time being, configuration in early-init will be limited to:
;; 1. Graphical elements such as frame parameters
;; 2. Package system configuration
;; 3. Native Compilation Settings
;;
;; Configuration in this file occurs *before* loading `custom-file' so care 
;; should be taken to ensure that the values shown here are not later overridden
;; by customize. 

;;; Code:

;;;; Graphical Elements Settings
(setq-default frame-title-format "GNU Emacs")
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(custom-set-faces
 (pcase (system-name)
   ("M-laptop-w11" '(default ((t (:height 140 :family "Input")))))
   ("M-laptop-arch" '(default ((t (:height 140 :weight medium :family "Input Mono Condensed")))))
   (other-system '(default ((t (:height 120 :family "Input")))))))
(setq ns-use-proxy-icon nil)

;;;; Package Settings
(customize-set-variable
 'package-quickstart-file
 (expand-file-name "var/package-quickstart.el" user-emacs-directory))
(customize-set-variable
 'package-archives '(("melpa" . "https://melpa.org/packages/")
                     ("melpa-stable" . "https://stable.melpa.org/packages/")
		     ;; Tree-Sitter
		     ("ublt" . "https://elpa.ubolonton.org/packages/")
                     ("org" . "http://orgmode.org/elpa/")
                     ("gnu" . "http://elpa.gnu.org/packages/")
		     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable
 'package-archive-priorities '(("melpa" . 10)
                               ("ublt" . 10)
			       ("nongnu" . 5)
                               ("melpa-stable" . 0)
                               ("gnu" . 0)))
(customize-set-variable
 'package-user-dir (expand-file-name "elpa" user-emacs-directory))
(customize-set-variable 'package-menu-hide-low-priority t)
(customize-set-variable 'package-quickstart t)

;;;; Native Compilation Settings
(customize-set-variable 'native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
