;;; selectrum.el -*- lexical-binding: t; -*-

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

(provide 'selectrum)
