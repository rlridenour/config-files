;;; theme.el -*- lexical-binding: t; -*-

;;; Fonts 

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0)

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-plain t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
;;  (doom-themes-neotree-config)
  ;; or for treemacs users
;;  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
;;  (doom-themes-org-config))
)


(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))


(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . fullheight))


(provide 'theme)


;;; theme.el ends here
