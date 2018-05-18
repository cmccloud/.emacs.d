;;; early-init.el --- Personal Emacs Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Christopher McCloud

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
;; 2. Package system configuration.
;;
;; Configuration in this file occurs *before* loading `custom-file' so care 
;; should be taken to ensure that the values shown here are not later overridden
;; by `customize'. 

;;; Code: 
;;;; Graphical Elements Settings
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'nil))
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'frame-resize-pixelwise t)
(setq ns-use-proxy-icon nil)

;;;; Package Settings
(customize-set-variable 'package-user-dir (concat user-emacs-directory "elpa"))
(customize-set-variable 'package-quickstart t)

;;; early-init.el ends here
