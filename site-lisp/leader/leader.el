;;; leader.el

(defvar leader--default-map (make-sparse-keymap)
  "Keymap used for mode-independent leader bindings.")

(defvar leader--current-map leader--default-map
  "Keymap used for major-mode leader bindings.")

(defvar leader--mode-maps nil
  "Alist of mode-local leader bindings, shadows mode-independent bindings.")

(defvar leader--minor-mode-maps nil
  "Alist of minor-mode leader bindings, shadows mode-independent bindings
and major mode bindings.")

;;; customization
(defgroup leader nil
  "<leader> support"
  :prefix 'leader/)

(defcustom leader/leader "M-p"
  "String"
  :type 'string
  :group 'leader)

;;;###autoload
(define-minor-mode leader-mode
  :init-value nil
  :global t
  :keymap nil
  (let* ((mode-map (cdr (assoc major-mode leader--mode-maps)))
         (map (or mode-map leader--default-map))
         (leader-key (read-kbd-macro leader/leader)))
    (if leader-mode
        (progn
          (setq leader--current-map map)
          (define-key global-map leader-key leader--current-map)
          (mapcar (lambda (cell)
                    (let ((minor-mode-map (cdr cell)))
                      (set-keymap-parent minor-mode-map leader--current-map)))
                  leader--minor-mode-maps))
      (progn
        (define-key global-map leader-key nil)
        (mapcar (lambda (cell)
                  (let ((minor-mode-map (cdr cell)))
                    (define-key minor-mode-map leader-key nil)))
                leader--minor-mode-maps)))))
;;;###autoload
(define-globalized-minor-mode global-leader-mode leader-mode
  (lambda () (leader-mode 1)))

;;;###autoload
(defun leader/set-key (key def &rest bindings)
  (interactive "kKey: \noCommand: ")
  (leader--def-keys leader--default-map key def bindings))
(put 'leader/set-key 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/set-key-for-mode (mode key def &rest bindings)
  (interactive "Smode: \nkKey: \noCommand: ")
  (let ((mode-map (cdr (assoc mode leader--mode-maps))))
    (unless mode-map
      (setq mode-map (make-sparse-keymap))
      (set-keymap-parent mode-map leader--default-map)
      (push (cons mode mode-map) leader--mode-maps))
    (leader--def-keys mode-map key def bindings)))
(put 'leader/set-key-for-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun leader/set-key-on-minor-mode-map (mode map key def &rest bindings)
  (interactive "Smode: \nkKEy: \noCommand: ")
  (let ((mmode-map (cdr (assoc mode leader--minor-mode-maps))))
    (unless mmode-map
      (setq mmode-map (make-sparse-keymap))
      (set-keymap-parent mmode-map leader--current-map)
      (push (cons mode mmode-map) leader--minor-mode-maps)
      (leader--def-keys mmode-map key def bindings)
      (define-key map (read-kbd-macro leader/leader) mmode-map))))
(put 'leader/set-key-on-minor-mode-map 'lisp-indent-function 'defun)

(defun leader--def-keys (map key def bindings)
  (while key
    (define-key map (read-kbd-macro key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(provide 'leader)
;;; leader.el ends here