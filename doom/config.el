;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(setq user-full-name "Hesam Fatemipour"
      user-mail-address "hesamfatemipour@gmail.com")

;; theme and font
(setq doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 19))
(setq doom-theme 'doom-tomorrow-night)

(setq display-line-numbers-type t)
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-scroll-bar-mode t)
(setq git-gutter-mode t)

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2))

(after! neotree
  (setq doom-themes-neotree-file-icons 'icons)
  (setq doom-themes-neotree-enable-file-icons 'icons)
  (setq neo-theme 'icons))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(use-package! windmove
  :config
  (windmove-default-keybindings 'meta))

;; Or for drag-and-drop buffer swapping between windows:
(use-package! buffer-move)

(use-package! nyan-mode
  :config
  (nyan-mode 1)
  (nyan-start-animation))

;; Show word-level diff highlights everywhere
(after! magit
  (setq magit-diff-refine-hunk t))

;; Open Magit status in the same window
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; Better duplicate file naming
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(use-package! jsonnet-mode
  :defer t
  :config
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?{ ?})))

(setq evil-want-fine-undo t)

(defun my-evil-delete-char ()
  "Delete character without yanking."
  (interactive)
  (delete-char 1))

(defun my-evil-delete-char-backward ()
  "Delete character backward without yanking."
  (interactive)
  (delete-char -1))

(map! :n "x" #'my-evil-delete-char
      :n "X" #'my-evil-delete-char-backward)

;; to save sessions on quit
(add-hook 'kill-emacs-hook #'+workspace/save-session)
