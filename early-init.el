;;; early-init.el -*- lexical-binding: t -*-

;; Graphical Elements Settings
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'nil))
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'frame-resize-pixelwise t)

;; Package Settings
(customize-set-variable 'package-quickstart t)
