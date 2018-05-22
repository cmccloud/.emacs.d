;;; eyebrowse-persp-bridge.el --- Taken from Spacemacs. -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;;; Commentary:
;; Code below taken from Spacemacs Layouts layer.

;;; Code: 
(require 'eyebrowse)
(require 'persp-mode)
(require 'dash)

(defun spacemacs//get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (persp-parameter it persp) param-names)))

(defun spacemacs//set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.

Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (set-persp-parameter it other persp)
                param-names workspace-params)))

(defun spacemacs/load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((workspace-params (spacemacs//get-persp-workspace (get-frame-persp frame) frame))
           (window-configs (nth 0 workspace-params))
           (current-slot (nth 1 workspace-params))
           (last-slot (nth 2 workspace-params)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (spacemacs/save-eyebrowse-for-perspective frame)))))

(defun spacemacs/load-eyebrowse-after-loading-layout (_state-file _phash persp-names)
  "Bridge between `persp-after-load-state-functions' and
`spacemacs/load-eyebrowse-for-perspective'.

_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
  (let ((cur-persp (get-current-persp)))
    ;; load eyebrowse for current perspective only if it was one of the loaded
    ;; perspectives
    (when (member (or (and cur-persp (persp-name cur-persp))
                      persp-nil-name)
                  persp-names)
      (spacemacs/load-eyebrowse-for-perspective 'frame))))

(defun spacemacs/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (spacemacs/save-eyebrowse-for-perspective))

(defun spacemacs/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (spacemacs//set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                        (eyebrowse--get 'current-slot frame)
                                        (eyebrowse--get 'last-slot frame))
                                  (get-frame-persp frame)
                                  frame))

(add-hook 'persp-before-switch-functions
          #'spacemacs/update-eyebrowse-for-perspective)
(add-hook 'eyebrowse-post-window-switch-hook
          #'spacemacs/save-eyebrowse-for-perspective)
(add-hook 'persp-activated-functions
          #'spacemacs/load-eyebrowse-for-perspective)
(add-hook 'persp-before-save-state-to-file-functions
          #'spacemacs/update-eyebrowse-for-perspective)
(add-hook 'persp-after-load-state-functions
          #'spacemacs/load-eyebrowse-after-loading-layout)


(provide 'eyebrowse-persp-bridge)

;; eyebrowse-persp-bridge.el ends here.
