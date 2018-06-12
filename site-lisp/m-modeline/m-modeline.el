;;; m-modeline.el --- Personal mode-line configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;;; Code: 
(defvar mode-line-default-format
  (get 'mode-line-format 'standard-value))

(defvar mode-line-padding
  '(:eval (let* ((mlf (remq mode-line-padding mode-line-format))
                 (len (length (format-mode-line mlf))))
            (make-string (- (window-total-width) len)
                         (string-to-char " ")))))

(defvar mode-line-file-modified
  '(:eval (if (buffer-modified-p)
       (propertize "[!]" 'face '(:inherit warning :weight bold))
       (propertize "[-]" 'face '(:weight bold)))))

;;;###autoload
(define-minor-mode m-modeline-mode
  "Personal mode line configuration"
  :init-value nil
  :global t
  (if m-modeline-mode
      (setq-default mode-line-format
                    `("%e"
                      mode-line-front-space
                      "%I "
                      mode-line-client
                      ,mode-line-file-modified
                      mode-line-frame-identification
                      mode-line-buffer-identification
                      "   "
                      (:eval (propertize "%l:%c" 'face '(:weight bold)))
                      ,mode-line-padding
                      (format-mode-line (vc-mode vc-mode))
                      "  "
                      (:eval (propertize (format-mode-line mode-line-modes) 'face '(:weight bold)))
                      mode-line-misc-info
                      mode-line-end-spaces))
    (setq-default mode-line-format
                  (cdar (get 'mode-line-format 'standard-value)))))

(provide 'm-modeline)

;;; m-modeline.el ends here 
