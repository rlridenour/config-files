;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
;; NOTE: early-init.el is generated from README.org.  Please edit that file instead.

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

(setq-default cursor-in-non-selected-windows nil
	      frame-title-format '("%f [%m]"))

(set-face-attribute 'default nil :height 160)

;;; early-init.el ends here
