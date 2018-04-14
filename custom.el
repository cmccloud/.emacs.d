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
 '(ansi-term-color-vector
   [unspecified "#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"] t)
 '(auto-save-file-name-transforms
   (quote
    ((".*" "/var/folders/f4/0p9hr1155ngckx9pjfsct6th0000gn/T/\\2" t))))
 '(avy-all-windows nil)
 '(avy-background t)
 '(avy-style (quote at))
 '(background-color "#202020")
 '(background-mode dark)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/cache/backups/"))))
 '(blink-cursor-mode nil)
 '(clojure-align-forms-automatically t)
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete-chars (quote (32 41 46)))
 '(company-backends
   (quote
    (company-tern company-bbdb company-nxml company-css company-eclim company-clang company-xcode company-capf company-semantic company-elisp company-cmake
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
 '(cursor-color "#cccccc")
 '(cursor-type t)
 '(custom-buffer-done-kill t)
 '(custom-enabled-themes nil)
 '(custom-file "/Users/Macnube/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("2a1b4531f353ec68f2afd51b396375ac2547c078d035f51242ba907ad8ca19da" "2af26301bded15f5f9111d3a161b6bfb3f4b93ec34ffa95e42815396da9cb560" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "f03e2076bb6ba9f1178ef83f54b395c48c70dd160a34e37c80c876c925701b5a" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "e1e94aaecf0d3ed7bc6cdcebb02aafbf2a0236e711f4a78783337f8b0d32c842" "413a1712dfb0929cca961863482094057217bcb13475b4b813b35ea81da8e2e6" "5fea012429d3e720b7adc598295157befc00af4f3d433255087b407bf3a9ab71" "c31b688a76507c3b0458ae9d3848e3196346aeeadde457df5f828c7dc3e59fe3" "b40f4579788b1e990c553c163a4d294ac9d5a2a4615b1d1a757812ea427f8d50" "2f5dd0ac7dffdc0acf0aa15c9b7a5b1f86c37b9e11800325160b89c1b8a6fefe" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(echo-keystrokes 0.2)
 '(emmet-indentation 2)
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/local/sbin" "/usr/local/opt/nvm/versions/node/v6.2.0/bin" "/usr/local/bin" "/usr/local/sbin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/X11/bin" "/usr/local/opt/nvm/sbin" "/Library/TeX/texbin" "/usr/local/opt/coreutils/libexec/gnubin" "/usr/local/otp/nvm" "/usr/local/opt/nvm/bin" "/usr/local/share/npm/bin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/usr/local/share/npm/bin" "/usr/local/otp/nvm" "/usr/local/opt/coreutils/libexec/gnubin" "/Applications/Emacs.app/Contents/MacOS/libexec")))
 '(explicit-shell-file-name "/bin/zsh")
 '(fancy-splash-image nil)
 '(fci-rule-color "#eee8d5")
 '(flycheck-highlighting-mode (quote sexps))
 '(foreground-color "#cccccc")
 '(fringe-mode nil nil (fringe))
 '(git-gutter:added-sign " ")
 '(git-gutter:ask-p nil)
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(global-visual-line-mode t)
 '(golden-ratio-auto-scale t)
 '(golden-ratio-extra-commands
   (quote
    (windmove-left windmove-right windmove-down windmove-up select-window-1 select-window-2 select-window-3 select-window-4 select-window-5)))
 '(golden-ratio-mode nil)
 '(haskell-completing-read-function (quote helm--completing-read-default))
 '(haskell-tags-on-save t)
 '(helm-M-x-fuzzy-match nil)
 '(helm-ag-always-set-extra-option t)
 '(helm-autoresize-max-height 30)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*magit" "\\*lispy-goto*" "\\*Backtrace*")))
 '(helm-dash-browser-func (quote browse-url))
 '(helm-dash-docsets-path "/Users/Macnube/Library/Application Support/Dash/DocSets/")
 '(helm-ff-tramp-not-fancy nil)
 '(helm-locate-command "mdfind -name %s %s")
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found)))
 '(helm-mode t)
 '(helm-source-names-using-follow (quote ("Buffers")))
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
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#efebe9")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((top . 30) (left . 50) (width . 81) (height . 51))))
 '(initial-major-mode (quote fundamental-mode))
 '(jdee-db-active-breakpoint-face-colors ("#1B2229" . "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors ("#1B2229" . "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors ("#1B2229" . "#3B3F46"))
 '(js-expr-indent-offset 0)
 '(js-indent-level 2)
 '(js2-include-node-externs t)
 '(large-file-warning-threshold 200000000)
 '(leader-mode t t)
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
 '(mouse-wheel-progressive-speed t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(org-agenda-files (quote ("~/.emacs.d/emacs-todo.org")))
 '(org-ellipsis "  ")
 '(package-selected-packages
   (quote
    (doom-themes
     company
     solaire-mode
     helm-xref
     helm-unicode
     stripe-buffer
     hlinum
     nlinum
     eldoc-eval
     company-quickhelp
     dumb-jump
     git-commit
     lispy
     use-package
     persp-mode
     helm
     helm-ag
     helm-core
     helm-descbinds
     helm-describe-modes
     helm-projectile
     helm-swoop
     helm-themes
     bind-key
     hydra
     git-gutter-fringe
     git-gutter
     spacemacs-theme
     page-break-lines
     all-the-icons
     writeroom-mode
     visual-fill-column
     magit-gh-pulls
     impatient-mode
     company-web
     rainbow-mode
     company-tern
     clojure-mode
     paradox
     pdf-tools
     multiple-cursors
     expand-region
     web-mode
     emmet-mode
     gist
     f
     request-deferred
     deferred
     request
     shm
     intero
     haskell-mode
     diff-hl
     gh-md
     markdown-mode
     vmd-mode
     nodejs-repl
     auctex
     eyebrowse
     golden-ratio
     flycheck
     slime-company
     slime
     skewer-mode
     js-doc
     racket-mode
     geiser
     osx-trash
     projectile
     ggtags
     smooth-scrolling
     elisp-slime-nav
     window-numbering
     smartparens
     shackle
     magit
     js2-mode
     exec-path-from-shell
     cider
     avy)))
 '(paradox-column-width-package 30)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(pdf-view-display-size (quote fit-height))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pdf-view-use-imagemagick t)
 '(pdf-view-use-scaling t)
 '(pos-tip-background-color "#A6E22E" t)
 '(pos-tip-foreground-color "#272822" t)
 '(projectile-cache-file "/Users/Macnube/.emacs.d/cache/projectile.cache")
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(projectile-known-projects-file "/Users/Macnube/.emacs.d/cache/projectile-bookmarks.eld")
 '(projectile-sort-order (quote recentf))
 '(projectile-switch-project-action (quote helm-projectile))
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude (quote ("~/.emacs.d/cache/.*")))
 '(recentf-max-menu-items 10)
 '(recentf-max-saved-items 1000)
 '(recentf-save-file "~/.emacs.d/cache/recentf")
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values (quote ((eval progn (pp-buffer) (indent-buffer)))))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-margin 10)
 '(scroll-preserve-screen-position t)
 '(semantic-edits-verbose-flag t)
 '(semantic-idle-scheduler-idle-time 10)
 '(semantic-stickyfunc-indent-string " ")
 '(semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")
 '(shackle-mode t)
 '(shell-file-name "/bin/zsh")
 '(show-paren-delay 0.05)
 '(show-paren-mode t)
 '(slime-kill-without-query-p t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(sp-show-pair-delay 0.01)
 '(sp-show-pair-from-inside nil)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/cache/auto-saves/" nil (tramp))
 '(tramp-backup-directory-alist (quote ((".*" . "~/.emacs.d/cache/backups/"))) nil (tramp))
 '(tramp-persistency-file-name "/Users/Macnube/.emacs.d/cache/tramp")
 '(use-package-minimum-reported-time 0.05)
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
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(which-key-mode t)
 '(which-key-sort-order (quote which-key-prefix-then-key-order-reverse))
 '(writeroom-global-effects
   (quote
    (writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :width normal :foundry "nil" :family "Menlo"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :underline nil :weight bold))))
 '(cursor ((t (:background "SkyBlue3" :foreground "gray99"))))
 '(diff-hl-change ((t (:background "SteelBlue3"))))
 '(diff-hl-delete ((t (:background "IndianRed2"))))
 '(diff-hl-insert ((t (:background "light green"))))
 '(git-gutter-fr:modified ((t (:foreground "keyboardFocusIndicatorColor"))))
 '(git-gutter:added ((t (:background "PaleGreen2" :foreground "#859900" :weight bold))))
 '(git-gutter:deleted ((t (:background "IndianRed2" :weight bold))))
 '(git-gutter:modified ((t (:background "RoyalBlue2" :weight bold))))
 '(helm-candidate-number ((t nil)))
 '(helm-visible-mark ((t (:background "steel blue" :foreground "black"))))
 '(hl-line ((t (:inherit helm-selection))))
 '(region ((t (:background "slate grey" :foreground "black"))))
 '(show-paren-match ((t nil)))
 '(which-key-group-description-face ((t (:foreground "#a9a1e1" :weight semi-bold)))))
