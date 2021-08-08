;;; evil.el -*- lexical-binding: t; -*-

;; Evil mode for vim emulation.

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-default-state 'insert))

(provide 'evil)


