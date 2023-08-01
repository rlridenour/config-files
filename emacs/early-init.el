;; early-init.el -*- lexical-binding: t; -*-
;; NOTE: early-init.el is generated from README.org.  Please edit that file instead.

(setq gc-cons-threshold most-positive-fixnum)

(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

(setq package-enable-at-startup nil)

(setq user-full-name "Randy Ridenour"
      user-mail-address "rlridenour@gmail.com")

;; Silence the "Package cl is deprecated" warning.
(setq byte-compile-warnings '(cl-functions))

;; Silence native compilation warnings
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)
;; Code

;; compile elisp
(when (fboundp 'native-compile-async)
    (setq comp-deferred-compilation t
	  comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq-default frame-inhibit-implied-resize t)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)
(setq use-dialog-box nil)

(setq-default cursor-in-non-selected-windows nil
	      frame-title-format '("%f [%m]"))

(setq initial-major-mode 'org-mode)

(set-face-attribute 'default nil :height 160)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
