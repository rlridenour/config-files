;;; base-extensions.el -*- lexical-binding: t; -*-

(use-package s)

(use-package dash)

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package general
  :config
  (general-auto-unbind-keys)
  )
(use-package flycheck)


(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package  ace-window
  :bind ("C-c w" . ace-window)
  :config
  ;; (setq aw-leading-char-style 'path)
  (setq aw-background nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shrink-whitespace
  :bind ("M-=" . shrink-whitespace))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-tng-mode)
  (setq company-idle-delay 10))

;; Highlight current line when idle
(use-package hl-line+
  :config
  (toggle-hl-line-when-idle))


(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck)

(use-package smex)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (global-auto-revert-mode))

(use-package reveal-in-osx-finder
  :bind ("C-c z" . reveal-in-osx-finder))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package smartparens
  :diminish smartparens-mode)
(require 'smartparens-config)
(smartparens-global-mode t)


(use-package aggressive-indent)

;; Undo-fu and undo-fu session mode for undo and redo.

(use-package undo-fu
  :general
  ("s-z"   'undo-fu-only-undo
  "s-Z" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(global-undo-fu-session-mode)


(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
        '("~/.config/emacs/snippets")))
  (yas-global-mode 1))

;; With this code, yasnippet will expand the snippet if company didn't complete the word
;; replace company-complete-common with company-complete if you're using it

(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))

(advice-add 'company-complete-common :after (lambda ()
  		  				(when (equal my-company-point (point))
  			  			  (yas-expand))))

(use-package wc-mode)


(use-package company-prescient
	:config
	(company-prescient-mode t))

(use-package crux)

(use-package fish-mode)

(use-package rg)


(provide 'base-extensions)
