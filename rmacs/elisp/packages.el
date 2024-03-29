;;; packages.el -*- lexical-binding: t; -*-


(use-package avy
  :defer t
  :config
  (avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package ace-window
  :defer t)

(use-package shrink-whitespace
  :defer t)

(use-package hl-line+
  :config
  (toggle-hl-line-when-idle 1))

(use-package expand-region
  :defer t)

(use-package magit
  :defer t
  :config
  (global-auto-revert-mode)
  (setq magit-refresh-status-buffer nil
	magit-diff-highlight-indentation nil
	magit-diff-highlight-trailing nil
	magit-diff-paint-whitespace nil
	magit-diff-highlight-hunk-body nil
	magit-diff-refine-hunk nil
	magit-revision-insert-related-refs nil)
  )

(use-package reveal-in-osx-finder
  :defer t)

(use-package hungry-delete
  :defer t
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

(use-package evil-nerd-commenter
  :defer t)


(use-package which-key
  :config
  (which-key-mode))

(use-package crux
  :defer t)

(use-package fish-mode
  :defer t)

(use-package vundo)

(use-package unfill
  :defer t)


(use-package yankpad
  :defer t
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :config
  (bind-key "<f6>" 'yankpad-insert))

(use-package titlecase
  :defer t
  :config
  (setq titlecase-style "chicago"))




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
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((agenda . 5)
			  (recents  . 5)
			  (bookmarks . 10)
			  (projects . 5)))
  )


(defun goto-dashboard ()
  "this sends you to the dashboard buffer"
  (interactive)
  (let ((goto-dashboard-buffer (get-buffer "*dashboard*")))
    (switch-to-buffer goto-dashboard-buffer))
  (dashboard-refresh-buffer))

(use-package deadgrep)


;; EWW

(defun rrnet ()
  (interactive)
  (eww-browse-url "randyridenour.net")
  )

(defun sep ()
  (interactive)
  (eww-browse-url "plato.stanford.edu")
  )


;; Org-mac-link

(use-package org-mac-link
  :defer t)


;; Emacs-term-toggle
;; https://github.com/amno1/emacs-term-toggle
(use-package emacs-term-toggle
  :defer t
  :straight (emacs-term-toggle :host github :repo "amno1/emacs-term-toggle")
  :config
  (setq term-toggle-no-confirm-exit t)
  )


(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package emacs-everywhere)

(use-package powerthesaurus)

(use-package yaml-mode)

(use-package eat
  :defer t
  :straight (eat :host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("terminfo/e" "terminfo/e/*")
			 ("terminfo/65" "terminfo/65/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el"))))


(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package visual-regexp
  :config
  )

(provide 'packages)


;;; packages.el ends here
