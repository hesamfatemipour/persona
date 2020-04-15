; Let the emacs configuration begin!

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(setq user-full-name "Hesam"
      user-mail-address "hesaam@riseup.net")

;; use staright.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; installs use-package
(straight-use-package 'use-package)

;;load theme
(use-package doom-themes :straight t :defer t)
(load-theme 'doom-dracula t)

;; font
(defvar hesam/font "Jetbrains Mono-11")
(set-face-attribute 'default t :font hesam/font)
(set-frame-font hesam/font nil t)

;;mode line
(use-package doom-modeline :straight t :config (doom-modeline-mode 1))

;;vterm configuration
(use-package vterm :straight t :bind (("C-c C-t" . vterm)))

;;neotree
(use-package neotree :straight t :bind (("<f8>" . neotree-dir)))

;;magit
(use-package magit :straight t )

;;change cutome file
(use-package cus-edit
  :custom
  (custom-file "~/.emacs.d/temp.el"))

;; basic confs 
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; neotree 
(global-set-key[f8] 'neotree-dir)
