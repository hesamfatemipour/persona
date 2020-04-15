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

;; load theme
(use-package doom-themes :straight t :defer t)
(load-theme 'doom-dracula t)

;; font
(defvar hesam/font "Jetbrains Mono-11")
(set-face-attribute 'default t :font hesam/font)
(set-frame-font hesam/font nil t)

;;spacemacs mode line
(use-package spaceline :straight t
      :config
      (require 'spaceline-config)
      (spaceline-spacemacs-theme))

;; doom mode line 
(use-package doom-modeline :straight t
  :disabled t
    :config
    (doom-modeline-mode 1))

(use-package nyan-mode :straight t :config (nyan-mode 1))

;; vterm configuration
(use-package vterm :straight t :bind (("C-c C-t" . vterm)))

;; neotree
(use-package neotree :straight t :bind (("<f8>" . neotree-dir)))

;; magit
(use-package magit :straight t )

;;fzf
(use-package fzf :straight t :bind (("C-c f" . fzf-directory)))

;;company
(use-package company :straight t
  :custom
  (company-echo-delay 0.1)
  (company-idle-delay 0.0)
  (company-minimum-prefix-lenght)
  (company-tooltip-limit 30)
  (company-backends '(company-capf company-dabbrev company-files company-dabbrev-code))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-o" . company-other-backend)
              ("<tab>" . company-complete-common-or-cycle))
  :config (global-company-mode t)
  
  )

;;language server protocol
(use-package lsp-mode 
  :straight t
  :commands (lsp lsp-deferred)
  :hook 
  (prog-mode . lsp)
  :custom
  (lsp-auto-guess-root t)
  :commands (lsp))


;; change cutome file
(use-package cus-edit
  :custom
  (custom-file "~/.emacs.d/temp.el"))

;; improve scrolling
(use-package emacs
  :custom
  ; vertical scrolling
  (scroll-step 1)
  (scroll-margin 1)
  (scroll-conservatively 101)
  (scroll-up-aggressively 0.01)
  (scroll-down-aggressively 0.01)
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed t)
  ;; Horizontal Scroll
  (hscroll-step 1)
  (hscroll-margin 1))

;; removes toolbars 
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

