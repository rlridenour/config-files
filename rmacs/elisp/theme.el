;;; theme.el -*- lexical-binding: t; -*-

;;; Fonts 

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0)

(use-package all-the-icons)


;; Modus Themes


(use-package modus-themes
  :ensure t
  :straight (modus-themes :type git :flavor melpa :host sourcehut :repo "protesilaos/modus-themes")
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi)

  (define-key global-map (kbd "<f9>") #'modus-themes-toggle))



(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))


(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;; Flash modeline instead of warning bell.
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(provide 'theme)


;;; theme.el ends here
