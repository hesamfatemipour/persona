;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Hesam Fatemioiur"
      user-mail-address "hesamfatemipour@gmail.com")

;; theme and font
(setq doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 16))

(setq doom-theme 'doom-zenburn)

(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)
            (width . 106)
            (height . 50)
            (background-color . "black")
            (left . 50)
            (top . 50)))
  (setq initial-frame-alist '( (tool-bar-lines . 0))))

(setq default-frame-alist initial-frame-alist)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq-default history-length 1000)

(setq-default prescient-history-length 1000)

(setq git-gutter-mode t)

(after! python
  (setq python-shell-interpreter "python3"))

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2))

(after! neotree
  (setq doom-themes-neotree-file-icons 'icons)
  (setq doom-themes-neotree-enable-file-icons 'icons)
  (setq neo-theme 'icons))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(use-package! lsp-mode
  :hook
  (haskell-mode . lsp)
  (python-mode . lsp)
  (go-mode . lsp)
  (lua-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))
  :commands
  lsp)
