;;; evil.el -*- lexical-binding: t; -*-

;; Evil mode for vim emulation.

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-default-state 'insert
        evil-undo-system 'undo-fu
        evil-cross-lines t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-respect-visual-line-mode t)
  :config
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-mode 1)
  (evil-set-initial-state 'calendar-mode 'emacs))

(define-key evil-visual-state-map (kbd "v") 'er/expand-region)

(provide 'evil)

