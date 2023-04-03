;; init.el -*- lexical-binding: t; -*-
;; This is the main init file for Randy Ridenour's Emacs configuration.

(setq gc-cons-threshold (* 50 1000 1000))

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


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;  Ensure that system utilities required by various packages are installed.
(use-package use-package-ensure-system-package)

;; Allow key chords in use-package bindings.
  (use-package use-package-chords
	:config (key-chord-mode 1))

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(use-package exec-path-from-shell
	:config (exec-path-from-shell-initialize))


(setq custom-file "~/.config/rmacs/custom.el")
(load custom-file)


(require 'settings)
(require 'theme)
(require 'meow)
(require 'completion)
(require 'org)
(require 'latex)
(require 'packages)
(require 'functions)
(require 'keybindings)
(require 'finish)

(provide 'init)



;;; init.el ends here
