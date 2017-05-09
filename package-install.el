;; Package Installation
(require 'package)
(require 'use-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))

      package-archive-priorities '(("melpa-stable" . 10)
                                   ("melpa" . 5)
                                   ("gnu" . 0)
                                   ("marmalade" . -5)))

(package-initialize)
(package-refresh-contents)
(use-package paradox
  :config (paradox-enable))
