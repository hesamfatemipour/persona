;;; init.el --- My emacs config                      -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2020  Hesam

;; Author: Hesam <hesaam@riseup.net>
;; Keywords:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Code:

;; credentials
(setq user-full-name "Hesam"
      user-mail-address "hesaam@riseup.net")

;; garbage collection
(setq gc-cons-threshold (* 1024 1024 100)) ;; 100MB for Emacs initialization process
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 1024 1024 20))))
;; use staright.el as package manager
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

;; save sessions
(desktop-save-mode 1)

;; load theme
(use-package doom-themes :straight t :defer t)
(load-theme 'doom-dracula t)

;; line numbers
(global-display-line-numbers-mode)

;; Window details
  (setq display-buffer-alist
        '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
             (display-buffer-in-side-window)
             (window-width . 0.20)
             (side . right)
             (slot . 0))
          ("^vterm.*"
            (display-buffer-in-side-window)
            (window-height . 0.25)
            (side . bottom)
	    (slot . 1)
          )))
;; font
;;(defvar hesam/font "Jetbrains Mono-11")
;;(set-face-attribute 'default t :font hesam/font)
;;(set-frame-font hesam/font nil t)

;;Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; all the icons
(use-package all-the-icons
  :disabled t
  :straight t
  :commands (all-the-icons-octicon
	       all-the-icons-faicon
	       all-the-icons-fileicon
	       all-the-icons-wicon
	       all-the-icons-material
	       all-the-icons-alltheicon))

(use-package all-the-icons-dired
  :straight t
  :disabled t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; flycheck
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

;; spacemacs mode line
(use-package spaceline :straight t
      :config
      (require 'spaceline-config)
      (spaceline-spacemacs-theme))

;; doom mode line
(use-package doom-modeline :straight t
  :disabled t
    :config
    (doom-modeline-mode 1))

;; eybrowse
 (use-package eyebrowse :straight t :config (eyebrowse-mode +1))

;; nyan mode(the cat)
(use-package nyan-mode :straight t :config (nyan-mode 1))

;; vterm configuration
(use-package vterm
  :straight t
  :commands vterm
  :bind (("C-c C-T" . hesam/open-vterm-vertical))
  :config
  (defun hesam/open-vterm-vertical ()
    (split-window-right)
    (interactive)
    (switch-to-buffer-other-window (buffer-name))
    (vterm))
  (defun hesam/vterm-hooks ()
    (display-line-numbers-mode -1)
    (setq confirm-kill-processes nil)
    (setq hscroll-margin 0))
  (add-hook 'vterm-mode-hook 'hesam/vterm-hooks))

;; neotree
(use-package neotree :straight t :bind (("<f8>" . neotree-dir)))

;; magit
(use-package magit
  :straight t
  :commands (magit-status)
  :bind
  (("C-x g" . 'magit-status)))

;; fzf
(use-package fzf :straight t :bind (("C-c f" . fzf-directory)))

;; company
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
  :config (global-company-mode t))

;; general package
(use-package general :straight t)

;; Helm ftw
;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

;;language server protocol(lsp)
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook
  (prog-mode . lsp)
  :custom
  (lsp-auto-guess-root t)
  :commands (lsp))

;; show parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;;ibuffer
(use-package ibuffer :bind (("C-x C-b" . 'ibuffer)))

;; ibuffer projectile
(use-package ibuffer-projectile
  :straight t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

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

;; removes toolbars for minimal theme
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; confs
(use-package exec-path-from-shell :straight t
  :config (exec-path-from-shell-initialize))

(use-package files
    :config
    (run-with-idle-timer 1 nil (lambda () (save-buffer) )))

;; language compatability

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :config
  (defun hesam/python-insert-docstring ()
    (interactive)
    (insert "'''\n'''")
    (previous-line))
  (amirreza/with-backends python-mode (company-capf))
  :bind
  (:map python-mode-map
	("C-c l p d" . hesam/python-insert-docstring)))

;; Microsoft lsp
(use-package lsp-python-ms :straight t)

;; auto pep8
(use-package py-autopep8
  :straight t
  :hook python-mode
  :config
  (py-autopep8-enable-on-save))

;; go
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)
  :init
  (add-hook 'go-mode-hook (lambda () (add-to-list 'exec-path (concat (getenv "HOME") "/go"))))
  :config
  (add-hook 'go-mode-hook (lambda () (interactive)
                            (add-hook 'before-save-hook 'lsp-format-buffer t t)
                            (add-hook 'before-save-hook 'lsp-organize-imports t t))))

;; lua mode (requires installing lsp for lua)
(use-package lua-mode :straight t :mode "\.lua\'")

;;; init.el ends here

