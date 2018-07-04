;;; m-modeline.el --- Personal modeline settings. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;;; Code:
(defvar m-modeline-persp-lighter
  '(:eval
    (when (bound-and-true-p persp-mode)
      (let ((persp (get-current-persp)))
        (concat "#" (safe-persp-name persp) " ")))))

(put 'm-modeline-persp-lighter 'risky-local-variable t)

(defvar mode-line-padding
  '(:eval
    (let ((mode-line-padding ""))
      (make-string (max (- (window-total-width)
                           (string-width (format-mode-line mode-line-format)))
                        0)
                   (string-to-char " ")))))

(put 'mode-line-padding 'risky-local-variable t)

(defvar mode-line-file-modified
  '(:eval (if (buffer-modified-p)
              (propertize "!" 'face '(:inherit warning :weight bold))
            (propertize "-" 'face '(:weight bold)))))

(put 'mode-line-file-modified 'risky-local-variable t)

(defvar mode-line-file-size
  '(:eval (propertize "%I " 'face 'bold)))

(put 'mode-line-file-size 'risky-local-variable t)

(defvar mode-line-cursor-position
  '(:eval (propertize "%l:%c" 'face 'bold)))

(put 'mode-line-cursor-position 'risky-local-variable t)

;;;###autoload
(define-minor-mode m-modeline-mode
  "Personal mode line configuration"
  :init-value nil
  :global t
  (if m-modeline-mode
      (setq-default mode-line-format
                    `("%e"
                      mode-line-front-space
                      m-modeline-persp-lighter
                      mode-line-file-size
                      mode-line-client
                      mode-line-file-modified
                      " "
                      mode-line-misc-info
                      " "
                      mode-line-buffer-identification
                      " "
                      mode-line-cursor-position
                      mode-line-padding
                      (vc-mode vc-mode)
                      " "
                      mode-line-modes
                      mode-line-end-spaces))
    (setq-default mode-line-format
                  (cdar (get 'mode-line-format 'standard-value)))))

(provide 'm-modeline)

;;; m-modeline.el ends here 
