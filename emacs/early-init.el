;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Erwin Jansen <echweb@outlook.com>

;; This file is part of rr-emacs
;;
;; rr-emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; rr-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with pur-emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;;;; Defer Garbage Collection
(setq gc-cons-threshold most-positive-fixnum)

;;;; File hander
(defvar rr-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq auto-save-list-file-prefix nil)

;;;; Native Compilation
;; Silence nativecomp warnings popping up
(customize-set-variable 'native-comp-async-report-warnings-errors nil)

;; Improved execution speed
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

;;;; Package Management
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;;;; Remove unnecesary GUI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq-default frame-inhibit-implied-resize t)
(setq-default frame-title-format "\n")
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)

;;;; Initial Mode
(setq initial-major-mode 'fundamental-mode)

;;;; Theme
;; Remove the border around the mode-line (flat look)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

;; Load the theme early to there aren't any flashes when Emacs loads
(load-theme 'modus-operandi t)

;; Increase font size
(set-face-attribute 'default nil :height 160)

;; Reset variables, and new startup message
(add-hook 'emacs-startup-hook (lambda ()
				;; Reset the file handler list
                                (setq file-name-handler-alist rr-file-name-handler-alist)
                                ;; Reset garbage collection
                                (setq gc-cons-threshold 2000000)
				;; Startup time message
                                (message (format "RR-Emacs ready in %.5f seconds with %d garbage collections."
                                                 (float-time (time-subtract after-init-time before-init-time))
						 gcs-done))))

(provide 'early-init)
;;; early-init.el ends here
