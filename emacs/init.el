; Let the emacs configuration begin!

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)


(require 'package)
(setq inhibit-splash-screen 0)

(package-initialize)
(add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/"))

(require `all-the-icons)'
(load-theme 'doom-palenight)
(global-linum-mode t)



(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

(global-set-key[f8] 'neotree-toggle)

