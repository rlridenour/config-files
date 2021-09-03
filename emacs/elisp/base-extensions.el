;;; base-extensions.el -*- lexical-binding: t; -*-
(use-package general
  :config
  (general-auto-unbind-keys)
  )
(use-package flycheck)

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package prescient)

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))



(use-package consult
  :straight (:host github :repo "minad/consult" :branch "main")
  ;; Replace bindings. Lazily loaded due to use-package.


  ;; The :init configuration is always executed (Not lazy!)
  :init
  (setq consult-themes '(modus-operandi modus-vivendi hc-zenburn))
  (setq consult-narrow-key "<")

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  )

(use-package consult-selectrum
  :straight (:host github :repo "minad/consult" :branch "main")
  )

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

(use-package embark
  :straight (:host github :repo "oantolin/embark" :branch "master")
  :after selectrum
  :bind (:map minibuffer-local-map
         ("C-o" . embark-act)
         ("C-S-o" . embark-act-noexit)
         :map embark-file-map
         ("j" . dired-jump)))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))


(use-package consult-dir
  :straight (:host github :repo "karthink/consult-dir" :branch "master")
  :bind (("C-x C-d" . consult-dir)
         :map selectrum-minibuffer-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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
  (add-hook 'after-init-hook 'company-tng-mode))


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

;; (use-package smartparens
;;   :diminish smartparens-mode)
;; (require 'smartparens-config)
;; (smartparens-global-mode t)

;; Use puni-mode globally and disable it for term-mode.
(use-package puni
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package dogears
:straight (dogears :host github :repo "alphapapa/dogears.el")
:bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))


(provide 'base-extensions)
