;;; packages.el -*- lexical-binding: t; -*-


(use-package avy)

(use-package ace-window)

(use-package shrink-whitespace)

(use-package hl-line+
  :config
  (toggle-hl-line-when-idle))

(use-package expand-region)

(use-package magit
  :defer t
  :config
  (global-auto-revert-mode))

(use-package reveal-in-osx-finder)

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))

(use-package aggressive-indent)

(use-package evil-nerd-commenter)


(use-package which-key
  :config
  (which-key-mode))

(use-package crux)

(use-package fish-mode)

;; (use-package pdf-tools
;;    :pin manual
;;    :config
;;    (pdf-tools-install)
;;    (setq-default pdf-view-display-size 'fit-width)
;;    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;    :custom
;;    (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))

;; ;; (evil-set-initial-state 'pdf-view-mode 'emacs)
;; (add-hook 'pdf-view-mode-hook
;;   (lambda ()
;;     (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))


(use-package pulsar
  :custom
  (setq pulsar-pulse-functions
        '(isearch-repeat-forward
          isearch-repeat-backward
          recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          bookmark-jump
          other-window
          delete-window
          delete-other-windows
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          windmove-right
          windmove-left
          windmove-up
          windmove-down
          windmove-swap-states-right
          windmove-swap-states-left
          windmove-swap-states-up
          windmove-swap-states-down
          tab-new
          tab-close
          tab-next
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading))
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.2
        pulsar-iterations 10
        pulsar-face 'pulsar-blue
        pulsar-highlight-face 'pulsar-blue))

(pulsar-global-mode 1)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;;  (setq doom-fallback-buffer-name "*dashboard*")
  (setq dashboard-week-agenda nil)
  (setq dashboard-startup-banner "/Users/rlridenour/.config/doom/logo-emacs.png")
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons nil)
  (setq dashboard-set-navigator nil)
  (setq dashboard-items '((agenda . 5)
                          (recents  . 5)
                          (bookmarks . 10)))
                          
  )


(defun goto-dashboard ()
  "this sends you to the dashboard buffer"
  (interactive)
  (let ((goto-dashboard-buffer (get-buffer "*dashboard*")))
    (switch-to-buffer goto-dashboard-buffer))
  (dashboard-refresh-buffer))

(use-package deadgrep)

(provide 'packages)


;;; packages.el ends here