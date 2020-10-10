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

;; GC
(setq gc-cons-threshold (* 1024 1024 100))
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 1024 1024 20))))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Theme
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'darkburn t))

;; Font
(set-face-attribute 'default nil
                    :family "Ubuntu Mono derivative Powerline"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; Junk Files
(use-package cus-edit
  :custom
  (custom-file "~/.emacs.d/temp.el"))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-bar-width 1)
(setq doom-modeline-project-detection 'project)
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-go t)

;; Terminal
(use-package vterm
  :ensure t
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

;; General
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-display-line-numbers-mode)
(setq byte-compile-warnings '(cl-functions))
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Auto Complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;; To Search
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    ))

;; FZF
(use-package fzf
  :ensure  t
  :bind (("C-c f" . fzf-directory)))

;; RG
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))
;; Python
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package py-autopep8
  :ensure  t
  :hook python-mode
  :config
  (py-autopep8-enable-on-save))

;;; init.el ends here
