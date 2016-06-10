;; Package Installation
(require 'package)
(package-initialize)

;; Stable Packages
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)
(dolist (pkg '(use-package
                bind-key
                dash
                helm-dash
                seq
                exec-path-from-shell
                ggtags
                smartparens
                avy
                smooth-scrolling
                projectile
                helm
                helm-projectile
                helm-gtags
                multiple-cursors
                helm-ag
                helm-descbinds
                helm-swoop
                helm-themes
                expand-region
                magit
                company
                flycheck
                auctex
                pdf-tools
                clojure-mode
                geiser
                cider
                slime
                slime-company
                which-key
                elisp-slime-nav
                js2-mode
                markdown-mode
                tern
                skewer-mode
                js2-refactor
                osx-trash
                company-tern
                window-numbering
                eyebrowse
                golden-ratio
                shackle
                diminish))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Melpa Packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)
(dolist (pkg '(racket-mode
               lispy
               js-doc
               paper-theme
               labburn-theme
               white-sand-theme
               zenburn-theme
               color-theme-sanityinc-solarized
               arjen-grey-theme
               color-theme-sanityinc-tomorrow))
  (unless (package-installed-p pkg)
    (package-install pkg)))
