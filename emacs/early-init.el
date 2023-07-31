;; early-init.el -*- lexical-binding: t; -*-
;; NOTE: early-init.el is generated from README.org.  Please edit that file instead.

(setq gc-cons-threshold most-positive-fixnum)

(defvar rr-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq auto-save-list-file-prefix nil)

(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

(setq package-enable-at-startup nil)

(setq package-quickstart nil)

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

(add-hook 'emacs-startup-hook (lambda ()
				;; Reset the file handler list
				(setq file-name-handler-alist rr-file-name-handler-alist)
				;; Reset garbage collection
				(setq gc-cons-threshold 2000000)
				;; Startup time message
				(message (format "Emacs ready in %.5f seconds with %d garbage collections."
						 (float-time (time-subtract after-init-time before-init-time))
						 gcs-done))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
