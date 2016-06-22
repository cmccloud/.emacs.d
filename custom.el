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
 '(auto-save-file-name-transforms
   (quote
    ((".*" "/var/folders/f4/0p9hr1155ngckx9pjfsct6th0000gn/T/\\2" t))))
 '(avy-all-windows nil)
 '(avy-background t)
 '(avy-style (quote at))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/cache/backups/"))))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete-chars (quote (32 41 46)))
 '(company-backends
   (quote
    (company-tern company-bbdb company-nxml company-css company-eclim company-clang company-xcode company-capf company-ropemacs company-semantic company-elisp company-cmake
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-gtags-executable nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(compilation-message-face (quote default))
 '(create-lockfiles nil)
 '(cursor-type t)
 '(custom-buffer-done-kill t)
 '(custom-enabled-themes nil)
 '(custom-file "/Users/Macnube/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(diff-hl-margin-mode nil)
 '(diff-hl-side (quote right))
 '(echo-keystrokes 0.2)
 '(exec-path
   (quote
    ("/usr/local/bin/" "/usr/local/sbin/" "/usr/local/bin/" "/usr/local/sbin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" "/opt/X11/bin/" "/usr/local/opt/nvm/sbin/" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9/" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9/" "/usr/local/share/npm/bin/" "/usr/local/otp/nvm/" "/usr/local/opt/coreutils/libexec/gnubin/" "/Applications/Emacs.app/Contents/MacOS/libexec/")))
 '(explicit-shell-file-name "/bin/zsh")
 '(fancy-splash-image nil)
 '(fci-rule-color "#eee8d5")
 '(flycheck-highlighting-mode (quote sexps))
 '(fringe-mode nil nil (fringe))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(global-auto-revert-mode t)
 '(global-company-mode nil)
 '(global-diff-hl-mode nil)
 '(global-git-gutter-mode t)
 '(global-hl-line-mode t)
 '(global-semantic-idle-scheduler-mode nil)
 '(global-semanticdb-minor-mode t)
 '(golden-ratio-auto-scale t)
 '(golden-ratio-extra-commands
   (quote
    (windmove-left windmove-right windmove-down windmove-up select-window-1 select-window-2 select-window-3 select-window-4 select-window-5)))
 '(golden-ratio-mode nil)
 '(haskell-completing-read-function (quote helm--completing-read-default))
 '(haskell-tags-on-save t)
 '(helm-M-x-fuzzy-match nil)
 '(helm-autoresize-max-height 30)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*magit" "\\*lispy-goto*" "\\*Backtrace*")))
 '(helm-dash-browser-func (quote eww))
 '(helm-ff-tramp-not-fancy nil)
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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist
   (quote
    ((top . 30)
     (left . 50)
     (width . 111)
     (height . 57))))
 '(initial-major-mode (quote fundamental-mode))
 '(js-expr-indent-offset 0)
 '(js-indent-level 2)
 '(large-file-warning-threshold 200000000)
 '(leader-mode nil)
 '(leader/leader "M-m")
 '(lispy-avy-style-char (quote at))
 '(lispy-avy-style-symbol (quote at))
 '(lispy-compat (quote (edebug cider)))
 '(lispy-completion-method (quote helm))
 '(lispy-eval-display-style (quote overlay))
 '(lispy-no-permanent-semantic t)
 '(magit-diff-use-overlays nil)
 '(markdown-command "marked")
 '(markdown-command-needs-filename t)
 '(markdown-gfm-additional-languages (quote ("scheme" "javascript")))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(package-selected-packages
   (quote
    (use-package shm intero haskell-mode diff-hl gh-md markdown-mode vmd-mode labburn-theme expand-region nodejs-repl pdf-tools auctex eyebrowse golden-ratio flycheck slime-company slime color-theme-sanityinc-tomorrow monokai-theme material-theme arjen-grey-theme idea-darkula-theme white-sand-theme paper-theme js2-refactor skewer-mode js-doc helm-dash multiple-cursors racket-mode geiser osx-trash helm-gtags helm-projectile projectile ggtags smooth-scrolling elisp-slime-nav lispy zenburn-theme window-numbering which-key smartparens shackle magit js2-mode helm-themes helm-swoop helm-descbinds helm-ag exec-path-from-shell company-tern color-theme-sanityinc-solarized cider avy)))
 '(pdf-view-display-size (quote fit-height))
 '(pdf-view-use-imagemagick t)
 '(pdf-view-use-scaling t)
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
 '(semantic-edits-verbose-flag t)
 '(semantic-idle-scheduler-idle-time 10)
 '(semantic-mode nil)
 '(semantic-stickyfunc-indent-string " ")
 '(semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")
 '(shackle-mode t)
 '(shackle-rules
   (quote
    (("*Apropos*" :select t :align t :size 0.4)
     ("Outline.*pdf" :regexp t :align
      (quote left)
      :size 0.3)
     ("*Geiser documentation*" :select t :align t :size 0.4)
     ("*slime-description*" :select t :align t :size 0.4)
     ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
     ("*Help*" :select t :size 0.4 :align bottom)
     ("*Completions*" :select t :align t :size 0.4)
     ("*Compile-Log*" :select t :align t :size 0.4)
     ("*Man.*" :regexp t :select t :align t :size 0.4)
     ("*lispy-goto*" :align
      (quote below)
      :size 0.4))))
 '(shell-file-name "/bin/zsh")
 '(show-paren-mode nil)
 '(show-smartparens-global-mode t)
 '(slime-kill-without-query-p t)
 '(sp-show-pair-delay 0.1)
 '(sp-show-pair-from-inside nil)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/cache/auto-saves/")
 '(tramp-backup-directory-alist (quote ((".*" . "~/.emacs.d/cache/backups/"))))
 '(tramp-persistency-file-name "/Users/Macnube/.emacs.d/cache/tramp")
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
 '(diff-hl-change ((t (:background "SteelBlue3"))))
 '(diff-hl-delete ((t (:background "IndianRed2"))))
 '(diff-hl-insert ((t (:background "light green"))))
 '(git-gutter:added ((t (:background "PaleGreen2" :foreground "#859900" :weight bold))))
 '(git-gutter:deleted ((t (:background "IndianRed2" :weight bold))))
 '(git-gutter:modified ((t (:background "RoyalBlue2" :weight bold))))
 '(show-paren-match ((t nil))))
