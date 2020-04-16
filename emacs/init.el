;;; init.el --- My emacs config                      -*- lexical-binding: t; -*-

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

;; credentials 
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

;; save sessions
(desktop-save-mode 1)

;; load theme
(use-package doom-themes :straight t :defer t)
(load-theme 'doom-nord t)
(global-display-line-numbers-mode)

;; font
(defvar hesam/font "Jetbrains Mono-11")
(set-face-attribute 'default t :font hesam/font)
(set-frame-font hesam/font nil t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; icons
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

;; neyan mode
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
  :config (global-company-mode t))

;;general package
(use-package general :straight t)
;; Helm ftw
 (use-package helm :straight t
     :custom
     (helm-mode-fuzzy-match t) ;; enable fuzzy matching in all helm
     :general
     (:keymaps 'helm-map
               "C-j" 'helm-next-line
               "C-k" 'helm-previous-line
               "<tab>"  'helm-execute-persistent-action   ;; make tab work normal
               "C-z"  'helm-select-action) ;; C-z instead of tab to show helm actions

     (:keymaps 'override
               "M-y" 'helm-show-kill-ring
               "M-x" 'helm-M-x
               "C-s" 'helm-occur)

     (:keymaps 'override
               "<f6> g" 'helm-rg)

     (:prefix "C-h"
              "a" 'helm-apropos
              "f" 'helm-apropos
              "k" 'helm-apropos
              "v" 'helm-apropos
              )
     (:prefix "C-x" :keymaps 'override
              "C-f" 'helm-find-files
              "b" 'helm-mini
              "C-b" 'helm-mini
              ))

 (use-package helm-descbinds :straight t
   :bind (("C-h b" . helm-descbinds)))

 (use-package helm-describe-modes :straight t
   :bind (("C-h m" . helm-describe-modes)))

(use-package helm-make :straight t
  :bind (("<f5> m" . helm-make)))

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
  (scroll-step
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

;; disable scrollbars
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

;; Garbage Collection
(setq gc-cons-threshold (* 1024 1024 100)) ;; 100MB for Emacs initialization process
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 1024 1024 20)))) ;; reseting the gc cons to 20MB

