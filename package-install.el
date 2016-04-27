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
                magit
                company
                clojure-mode
		geiser
		cider
		which-key
                elisp-slime-nav
		js2-mode
                tern
		osx-trash
                company-tern
                window-numbering
                shackle
                zenburn-theme
                color-theme-sanityinc-solarized
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
	       lispy))
  (unless (package-installed-p pkg)
    (package-install pkg)))
