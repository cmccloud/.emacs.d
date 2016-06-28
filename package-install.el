;; Package Installation
(require 'package)
(package-initialize)

(use-package paradox
  :config (paradox-enable))

;; Stable Packages
(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-refresh-contents)
(dolist (pkg '(dash
               seq
               f
               request
               deferred
               request-deferred
               exec-path-from-shell
               ggtags
               smartparens
               yasnippet
               avy
               smooth-scrolling
               projectile
               helm
               helm-projectile
               helm-gtags
               helm-ag
               helm-descbinds
               helm-swoop
               helm-themes
               magit
               gist
               diff-hl
               company
               flycheck
               auctex
               rainbow-mode
               clojure-mode
               haskell-mode
               shm
               geiser
               cider
               intero
               slime
               slime-company
               which-key
               elisp-slime-nav
               emmet-mode
               js2-mode
               markdown-mode
               tern
               skewer-mode
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
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)
(dolist (pkg '(use-package
                bind-key
                racket-mode
                lispy
                paradox
                helm-dash
                multiple-cursors
                expand-region
                pdf-tools
                js-doc
                web-mode
                paper-theme
                labburn-theme
                white-sand-theme
                zenburn-theme
                color-theme-sanityinc-solarized
                arjen-grey-theme
                color-theme-sanityinc-tomorrow))
  (unless (package-installed-p pkg)
    (package-install pkg)))
