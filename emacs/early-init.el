;; early-init.el -*- lexical-binding: t; -*-
;; NOTE: early-init.el is generated from README.org.  Please edit that file instead.

(setq gc-cons-threshold most-positive-fixnum)

(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

(setq package-enable-at-startup nil)

(setq user-full-name "Randy Ridenour"
      user-mail-address "rlridenour@gmail.com")

(setq byte-compile-warnings '(cl-functions))

;; Silence native compilation warnings
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)
;; Code

(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
	comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq-default frame-inhibit-implied-resize t)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)
(setq use-dialog-box nil)

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(width . 100))

(setq-default cursor-in-non-selected-windows nil
	      frame-title-format '("%f [%m]"))

(setq initial-major-mode 'org-mode)

(set-face-attribute 'default nil :height 160)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
