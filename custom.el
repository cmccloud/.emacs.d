(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action (quote accept))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(avy-background t)
 '(avy-style (quote at-full))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/cache/backups/"))))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete-chars (quote (32 41 46)))
 '(company-backends
   (quote
    (company-tern company-bbdb company-nxml company-css company-eclim company-clang company-xcode company-ropemacs company-elisp company-semantic company-cmake
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse company-files company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-gtags-executable nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(compilation-message-face (quote default))
 '(cursor-type t)
 '(custom-buffer-done-kill t)
 '(custom-enabled-themes nil)
 '(custom-file "/Users/Macnube/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(echo-keystrokes 0.2)
 '(exec-path
   (quote
    ("/usr/local/bin/" "/usr/local/sbin/" "/usr/local/bin/" "/usr/local/sbin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" "/opt/X11/bin/" "/usr/local/opt/nvm/sbin/" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9/" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9/" "/usr/local/share/npm/bin/" "/usr/local/otp/nvm/" "/usr/local/opt/coreutils/libexec/gnubin/" "/Applications/Emacs.app/Contents/MacOS/libexec/")))
 '(fancy-splash-image nil)
 '(fci-rule-color "#eee8d5")
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-hl-line-mode nil)
 '(global-semantic-idle-scheduler-mode nil)
 '(global-semanticdb-minor-mode t)
 '(helm-M-x-fuzzy-match nil)
 '(helm-autoresize-max-height 30)
 '(helm-dash-browser-func (quote eww))
 '(helm-locate-command "mdfind -name %s %s")
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found)))
 '(helm-mode t)
 '(helm-split-window-in-side-p t)
 '(helm-swoop-speed-or-color t)
 '(helm-swoop-split-with-multiple-windows t)
 '(help-window-select t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(js-expr-indent-offset 0)
 '(js-indent-level 2)
 '(lispy-compat (quote (edebug cider)))
 '(lispy-completion-method (quote helm))
 '(lispy-eval-display-style (quote overlay))
 '(lispy-no-permanent-semantic t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(package-selected-packages
   (quote
    (monokai-theme material-theme arjen-grey-theme idea-darkula-theme white-sand-theme paper-theme js2-refactor skewer-mode js-doc helm-dash multiple-cursors racket-mode geiser osx-trash helm-gtags helm-projectile projectile ggtags smooth-scrolling elisp-slime-nav lispy zenburn-theme window-numbering which-key use-package smartparens shackle magit js2-mode helm-themes helm-swoop helm-descbinds helm-ag exec-path-from-shell company-tern color-theme-sanityinc-solarized cider avy)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-cache-file "/Users/Macnube/.emacs.d/cache/projectile.cache")
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(projectile-known-projects-file "/Users/Macnube/.emacs.d/cache/projectile-bookmarks.eld")
 '(projectile-sort-order (quote recentf))
 '(projectile-switch-project-action (quote helm-projectile))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("~/.emacs.d/cache/.*")))
 '(recentf-max-menu-items 10)
 '(recentf-max-saved-items 1000)
 '(recentf-save-file "~/.emacs.d/cache/recentf")
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(semantic-mode t)
 '(semantic-stickyfunc-indent-string " ")
 '(semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")
 '(shackle-mode t)
 '(shackle-rules
   (quote
    (("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
     ("*Help*" :size 0.4 :align bottom)
     ("*Complie-Log*" :select t :align t :size 0.4)
     ("*Man.*" :regexp t :select t :align t :size 0.4)
     ("*lispy-goto*" :align
      (quote below)
      :size 0.4))))
 '(shell-file-name "/bin/zsh")
 '(show-paren-mode nil)
 '(show-smartparens-global-mode t)
 '(sp-show-pair-delay 0.1)
 '(sp-show-pair-from-inside t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(visible-cursor nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :width ultra-condensed :foundry "nil" :family "Input"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :underline nil :weight bold))))
 '(cursor ((t (:background "SkyBlue3" :foreground "gray99"))))
 '(show-paren-match ((t nil))))
