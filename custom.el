;;; custom.el --- User custom-set settings. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Christopher McCloud

;; Author: Christopher McCloud <mccloud.christopher@gmail.com>

;; This file is not part of GNU Emacs

;;; Code: 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 75)
 '(ad-redefinition-action 'accept)
 '(ag-highlight-search t)
 '(ag-ignore-list '("archive-contents"))
 '(ag-reuse-window t)
 '(all-the-icons-scale-factor 1)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(apropos-do-all t)
 '(auth-source-cache-expiry 10800)
 '(auth-sources '(macos-keychain-internet macos-keychain-generic))
 '(auto-revert-verbose nil)
 '(auto-save-default nil)
 '(auto-save-list-file-prefix "/Users/Macnube/.emacs.d/cache/auto-save-list/.saves-")
 '(avy-all-windows nil)
 '(avy-background t)
 '(avy-style 'at)
 '(aw-keys '(49 50 51 52 53 54 55 56 57 48))
 '(background-color "#202020")
 '(background-mode dark)
 '(backup-directory-alist '(("." . "~/.emacs.d/cache/backups/")))
 '(bind-key-describe-special-forms t)
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(clojure-align-forms-automatically t)
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete-chars '(32 41 46))
 '(company-box-max-candidates 500)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-gtags-executable nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-require-match 'never)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 12)
 '(compilation-message-face 'default)
 '(compilation-scroll-output nil)
 '(completion-ignored-extensions
   '(".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".DS_Store" ".hi" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"))
 '(confirm-kill-emacs 'y-or-n-p)
 '(create-lockfiles nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#cccccc")
 '(cursor-in-non-selected-windows nil)
 '(cursor-type t)
 '(custom-buffer-done-kill t)
 '(custom-enabled-themes nil)
 '(custom-file "/Users/Macnube/.emacs.d/custom.el")
 '(custom-safe-themes t)
 '(delete-by-moving-to-trash t)
 '(dimmer-exclusion-predicates '(helm--alive-p window-minibuffer-p))
 '(dimmer-exclusion-regexp-list
   '("^\\*[h|H]elm.*\\*" "^\\*Minibuf-[0-9]+\\*" "^.\\*which-key\\*$"))
 '(dimmer-fraction 0.4)
 '(display-line-numbers-current-absolute t)
 '(display-line-numbers-type t t)
 '(doom-city-lights-brighter-comments t)
 '(doom-city-lights-comment-bg t)
 '(doom-dracula-brighter-comments t)
 '(doom-dracula-brighter-modeline nil)
 '(doom-dracula-comment-bg t)
 '(doom-one-brighter-comments t)
 '(doom-one-comment-bg t)
 '(doom-solarized-light-brighter-comments t)
 '(doom-solarized-light-comment-bg t)
 '(doom-themes-enable-bold nil)
 '(doom-themes-enable-italic nil)
 '(dumb-jump-selector 'helm)
 '(echo-keystrokes 0.2)
 '(ediff-keep-variants t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(emmet-indentation 2)
 '(enable-local-variables :safe)
 '(enable-recursive-minibuffers t)
 '(eshell-directory-name "/Users/Macnube/.emacs.d/cache/eshell/")
 '(exec-path
   '("/usr/local/opt/coreutils/libexec/gnubin/" "/usr/local/bin/" "/usr/local/sbin/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" "/opt/X11/bin/" "/usr/local/MacGPG2/bin/" "/usr/local/opt/nvm/" "/Library/TeX/texbin/" "/usr/local/opt/coreutils/libexec/gnubin/" "/Users/Macnube/.nvm/versions/node/v8.11.2/bin/" "/usr/local/opt/nvm/versions/node/v8.11.2/bin/" "/usr/local/share/npm/bin/" "/usr/local/otp/nvm/" "/usr/local/Cellar/emacs/HEAD-8f2a815/libexec/emacs/27.0.50/x86_64-apple-darwin16.7.0/"))
 '(explicit-shell-file-name "/bin/zsh")
 '(eyebrowse-new-workspace t)
 '(fancy-splash-image nil)
 '(fci-rule-color "#eee8d5")
 '(ffap-machine-p-known 'reject)
 '(ffap-machine-p-local 'reject)
 '(ffap-machine-p-unknown 'reject)
 '(fit-window-to-buffer-horizontally t)
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-error-list-minimum-level 'error)
 '(flycheck-highlighting-mode 'sexps)
 '(flycheck-indication-mode 'right-fringe)
 '(foreground-color "#cccccc")
 '(frame-resize-pixelwise t)
 '(fringe-mode 6 nil (fringe))
 '(git-gutter:added-sign " ")
 '(git-gutter:ask-p nil)
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(global-auto-revert-mode t)
 '(global-hl-line-sticky-flag nil)
 '(global-visual-line-mode t)
 '(golden-ratio-auto-scale t)
 '(golden-ratio-exclude-buffer-names '("*tide-documentation*"))
 '(haskell-completing-read-function 'helm--completing-read-default)
 '(haskell-tags-on-save t)
 '(helm-M-x-fuzzy-match t)
 '(helm-adaptive-history-file "~/.emacs.d/cache/helm-adaptive-history")
 '(helm-ag-always-set-extra-option t)
 '(helm-autoresize-max-height 30)
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "^magit.*:" "\\*Minibuf" "\\*Diff*" "\\*lispy-goto*" "\\*Backtrace*"))
 '(helm-buffer-max-length nil)
 '(helm-buffer-skip-remote-checking t)
 '(helm-candidate-number-limit 100)
 '(helm-completing-read-handlers-alist
   '((xref-find-definitions . helm-completing-read-default-find-tag)
     (describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-default-find-tag)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (find-file-at-point . helm-completing-read-sync-default-handler)
     (ffap . helm-completing-read-sync-default-handler)
     (execute-extended-command)
     (dired-do-rename . helm-read-file-name-handler-1)
     (dired-do-copy . helm-read-file-name-handler-1)
     (dired-do-symlink . helm-read-file-name-handler-1)
     (dired-do-relsymlink . helm-read-file-name-handler-1)
     (dired-do-hardlink . helm-read-file-name-handler-1)))
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-dash-browser-func 'browse-url)
 '(helm-dash-docsets-path "/Users/Macnube/Library/Application Support/Dash/DocSets/")
 '(helm-descbinds-window-style 'split-window)
 '(helm-display-header-line nil)
 '(helm-ff-auto-update-initial-value nil)
 '(helm-follow-mode-persistent t)
 '(helm-grep-ag-command
   "rg -M 256 --color=always --smart-case --no-heading --line-number %s %s %s")
 '(helm-imenu-delimiter ": ")
 '(helm-imenu-fuzzy-match nil)
 '(helm-locate-command "mdfind -name %s %s")
 '(helm-locate-fuzzy-match nil)
 '(helm-ls-git-status-command 'magit-status-internal)
 '(helm-man-or-woman-function 'Man-getpage-in-background)
 '(helm-mini-default-sources
   '(helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found))
 '(helm-moccur-always-search-in-current t)
 '(helm-moccur-auto-update-on-resume 'noask)
 '(helm-moccur-show-buffer-fontification nil)
 '(helm-moccur-use-ioccur-style-keys nil)
 '(helm-rg-default-directory 'default t)
 '(helm-semantic-display-style nil)
 '(helm-source-names-using-follow nil)
 '(helm-split-window-inside-p t)
 '(helm-window-prefer-horizontal-split t)
 '(help-window-select t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-nonselected-windows nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100)))
 '(history-length 1000)
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-line-sticky-flag nil)
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11"))
 '(hl-sexp-background-color "#efebe9")
 '(icomplete-mode nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines nil)
 '(inhibit-startup-message t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((top . 30) (left . 50) (width . 90) (height . 58)))
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(jdee-db-active-breakpoint-face-colors ("#1B2229" . "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors ("#1B2229" . "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors ("#1B2229" . "#3B3F46"))
 '(jit-lock-defer-time nil)
 '(jit-lock-stealth-nice 0.1)
 '(jit-lock-stealth-time 0.2)
 '(jit-lock-stealth-verbose nil)
 '(js-chain-indent t)
 '(js-expr-indent-offset 0)
 '(js-flat-functions t)
 '(js-indent-align-list-continuation t)
 '(js-indent-level 2)
 '(js2-include-node-externs t)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(large-file-warning-threshold 200000000)
 '(lispy-avy-style-char 'at)
 '(lispy-avy-style-symbol 'at)
 '(lispy-compat '(edebug cider))
 '(lispy-completion-method 'helm)
 '(lispy-eval-display-style 'overlay)
 '(lispy-no-permanent-semantic t)
 '(lispy-occur-backend 'helm)
 '(lispy-safe-copy t)
 '(lispy-safe-delete t)
 '(lispy-safe-paste t)
 '(load-prefer-newer t)
 '(magit-diff-use-overlays nil)
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 '(magit-ediff-dwim-show-on-hunks nil t)
 '(magit-process-popup-time 5)
 '(magit-repository-directories
   '(("/Users/Macnube/Repos" . 1)
     ("/Users/Macnube/.emacs.d" . 1)
     ("/Users/Macnube/.emacs.d/site-lisp" . 1)))
 '(magithub-clone-default-directory "/Users/Macnube/Repos/")
 '(magithub-dashboard-show-read-notifications nil)
 '(magithub-datetime-format "%A %B %e%l:%M%p %Y")
 '(magithub-dir "~/.emacs.d/cache/magithub")
 '(magithub-preferred-remote-method 'clone_url)
 '(make-backup-files nil)
 '(markdown-command "marked")
 '(markdown-command-needs-filename t)
 '(markdown-gfm-additional-languages '("scheme" "javascript"))
 '(mouse-wheel-progressive-speed t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-option-modifier 'super)
 '(ns-pop-up-frames 'fresh)
 '(ns-use-native-fullscreen nil)
 '(org-agenda-files '("~/.emacs.d/emacs-todo.org"))
 '(org-ellipsis "  ")
 '(package-archive-priorities
   '(("melpa" . 10)
     ("melpa-stable" . 5)
     ("gnu" . 0)
     ("marmalade" . -5)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")))
 '(package-menu-hide-low-priority t)
 '(package-quickstart t)
 '(package-quickstart-file "/Users/Macnube/.emacs.d/cache/package-quickstart.el")
 '(package-selected-packages
   '(darktooth-theme spacemacs-theme cider clojure-mode delight helm-dash pcre2el tide js2-mode intero ace-window wgrep which-key company-box magit zenburn-theme doom-themes helm lispy magithub slime smartparens yasnippet geiser osx-trash wgrep-ag wgrep-helm smart-jump helm-ls-git slime-company visual-regexp-steroids visual-regexp ag rg company company-quickhelp dumb-jump git-commit use-package persp-mode helm-descbinds helm-describe-modes helm-themes bind-key hydra page-break-lines all-the-icons visual-fill-column company-web rainbow-mode paradox expand-region web-mode f request-deferred deferred request shm haskell-mode diff-hl markdown-mode vmd-mode eyebrowse golden-ratio flycheck js-doc racket-mode projectile smooth-scrolling elisp-slime-nav window-numbering shackle exec-path-from-shell avy))
 '(package-user-dir "~/.emacs.d/elpa")
 '(paradox-column-width-package 30)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-lines-per-entry 1)
 '(paradox-use-homepage-buttons nil)
 '(pdf-view-display-size 'fit-height)
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pdf-view-use-imagemagick t)
 '(pdf-view-use-scaling t)
 '(persp-add-buffer-on-after-change-major-mode t)
 '(persp-auto-resume-time 0)
 '(persp-auto-save-opt 1)
 '(persp-autokill-buffer-on-remove nil)
 '(persp-init-frame-behaviour 'persp-ignore-wconf)
 '(persp-keymap-prefix "l")
 '(persp-nil-name "Home")
 '(persp-reset-windows-on-nil-window-conf t)
 '(persp-restrict-buffers-to-if-foreign-buffer nil)
 '(persp-save-dir "/Users/Macnube/.emacs.d/cache/persp-confs/")
 '(persp-set-last-persp-for-new-frames t)
 '(persp-switch-to-added-buffer nil)
 '(persp-switch-wrap t)
 '(pos-tip-background-color "#A6E22E" t)
 '(pos-tip-foreground-color "#272822" t)
 '(projectile-cache-file "/Users/Macnube/.emacs.d/cache/projectile.cache")
 '(projectile-completion-system 'helm)
 '(projectile-enable-caching t)
 '(projectile-indexing-method 'alien)
 '(projectile-known-projects-file "/Users/Macnube/.emacs.d/cache/projectile-bookmarks.eld")
 '(projectile-sort-order 'recentf)
 '(projectile-switch-project-action 'helm-projectile)
 '(rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(recentf-auto-cleanup 'never)
 '(recentf-exclude '("~/.emacs.d/cache/.*"))
 '(recentf-max-menu-items 10)
 '(recentf-max-saved-items 1000)
 '(recentf-save-file "~/.emacs.d/cache/recentf")
 '(rg-group-result t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((eval progn (pp-buffer) (indent-buffer))))
 '(save-interprogram-paste-before-kill t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-margin 5)
 '(scroll-preserve-screen-position t)
 '(select-active-regions 'only)
 '(semantic-analyze-summary-function 'semantic-format-tag-short-doc t)
 '(semantic-edits-verbose-flag nil)
 '(semantic-elisp-store-documentation-in-tag nil)
 '(semantic-idle-scheduler-idle-time 10)
 '(semantic-stickyfunc-indent-string " ")
 '(semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")
 '(shackle-mode t)
 '(shackle-rules
   '(("*Process List*" :custom shackle-set-no-other-window :select t :align below :size 0.3)
     ("*Apropos*" :custom shackle-set-no-other-window :select t :align below :size 0.3)
     ("Outline.*pdf" :custom shackle-set-no-other-window :regexp t :select t :align below :size 0.3)
     ("*Geiser documentation*" :custom shackle-set-no-other-window :select t :align below :size 0.3)
     ("*slime-description*" :custom shackle-set-no-other-window :select t :align below :size 0.3)
     ("\\`\\*[h|H]elm.*\\*\\'" :custom shackle-set-no-other-window :regexp t :align t :size 0.3)
     ("*Help*" :custom shackle-display-helm-help :select t :align below :size 0.3)
     ("^\\*helpful.*" :custom shackle-set-no-other-window :regexp t :select t :align below :size 0.3)
     ("*Completions*" :custom shackle-set-no-other-window :select t :align below :size 0.3)
     ("*Compile-Log*" :custom shackle-set-no-other-window :select t :align below :size 0.3)
     ("*lispy-goto*" :custom shackle-set-no-other-window :align t :size 0.3)
     ("*tide-documentation*" :custom shackle-set-no-other-window :select t :align below :size 0.3 :popup t)
     ("*lispy-help*" :custom shackle-set-no-other-window :select t :align below :size 0.3 :popup t)
     ("magit-process:.*" :custom shackle-set-no-other-window :regexp t :select t :align below :size 0.3 :popup t)
     ("^\\*\\(Wo\\)?Man.*" :custom shackle-set-no-other-window :regexp t :select t :align below :size 0.3 :popup t)
     ("*git-gutter:diff*" :other t :size 0.3)
     ("*Diff*" :select t :other t :size 0.3)
     ("*Package Commit List*" :select t :other t :align left)
     ("^\\*hgrep.*\\*" :custom shackle-other-or-new)
     ("*hmoccur*" :custom shackle-other-or-new)
     (dired-mode :custom shackle-other-or-new)
     ("*xref*" :custom shackle-other-or-new)
     ("^\\*ag.search.text:.*\\*$" :custom shackle-other-or-new)
     ("^\\*xwidget.webkit:.*\\*" :custom shackle-other-or-new)))
 '(shackle-select-reused-windows t)
 '(shell-file-name "/bin/zsh")
 '(show-paren-delay 0.05)
 '(show-paren-mode t)
 '(slime-kill-without-query-p t)
 '(smart-jump-bind-keys nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(sp-echo-match-when-invisible nil)
 '(sp-show-pair-delay 0.01)
 '(sp-show-pair-from-inside nil)
 '(split-height-threshold 80)
 '(split-width-threshold 80)
 '(split-window-preferred-function 'split-window-sensibly)
 '(standard-indent 2)
 '(tab-width 8)
 '(term-buffer-maximum-size 1024)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(term-suppress-hard-newline t)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.emacs.d/cache/auto-saves/" nil (tramp))
 '(tramp-backup-directory-alist '((".*" . "~/.emacs.d/cache/backups/")) nil (tramp))
 '(tramp-persistency-file-name "/Users/Macnube/.emacs.d/cache/tramp" nil (tramp))
 '(use-dialog-box nil)
 '(use-package-always-defer t)
 '(use-package-expand-minimally nil)
 '(use-package-minimum-reported-time 0.01)
 '(use-package-verbose nil)
 '(user-full-name "Christopher McCloud")
 '(user-mail-address "mccloud.christopher@gmail.com")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
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
     (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil)
 '(visible-bell nil)
 '(visible-cursor nil)
 '(vr/default-replace-preview nil)
 '(vr/engine 'pcre2el)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(wgrep-auto-save-buffer nil)
 '(wgrep-enable-key "")
 '(which-key-enable-extended-define-key t)
 '(which-key-min-display-lines 2)
 '(which-key-sort-order 'which-key-prefix-then-key-order-reverse)
 '(whitespace-line-column 80)
 '(whitespace-style '(face lines-tail))
 '(window-combination-limit 'window-size)
 '(window-combination-resize t)
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places 'right-only)
 '(window-divider-default-right-width 1)
 '(winner-dont-bind-my-keys t)
 '(woman-cache-filename "~/.emacs.d/cache/.wmncache.el")
 '(woman-cache-level 3)
 '(woman-fill-frame t)
 '(xref-marker-ring-length 200)
 '(xref-show-xrefs-function 'helm-xref-show-xrefs t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :slant normal :weight normal :height 120 :width ultra-condensed :foundry "nil" :family "Input"))))
 '(aw-leading-char-face ((t (:inherit t :height 1.3))))
 '(doom-modeline-inactive-bar ((t (:inherit mode-line-inactive))))
 '(helm-candidate-number ((t (:inherit bold :background nil))))
 '(helm-match ((t (:inherit font-lock-keyword-face :weight bold))))
 '(helm-match-item ((t (:inherit helm-match :underline t))))
 '(helm-rg-preview-line-highlight ((t (:inherit helm-match-item))))
 '(helm-rg-preview-match-highlight ((t (:inherit helm-match))))
 '(persp-face-lighter-buffer-not-in-persp ((t (:inherit error))))
 '(vc-conflict-state ((t (:inherit (diff-changed vc-state-base)))))
 '(vc-edited-state ((t (:inherit (diff-changed vc-state-base)))))
 '(vc-locally-added-state ((t (:inherit (diff-changed vc-state-base)))))
 '(vc-missing-state ((t (:inherit (diff-changed vc-state-base)))))
 '(vc-needs-update-state ((t (:inherit (diff-changed vc-state-base)))))
 '(vc-removed-state ((t (:inherit (diff-changed vc-state-base)))))
 '(vc-state-base ((t (:weight bold))))
 '(vc-up-to-date-state ((t (:inherit (diff-added vc-state-base))))))
;;; custom.el ends here
