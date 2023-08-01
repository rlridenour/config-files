;;; init.el --- Personal Emacs configuration file -*- lexical-binding: t; -*-
;; NOTE: init.el is generated from README.org.  Please edit that file instead

(defconst rr-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst rr-cache-dir "~/.cache/emacs/"
  "The directory for Emacs activity files.")

(defconst rr-backup-dir (concat rr-cache-dir "backup/")
  "The directory for Emacs backup files.")

(defconst rr-org-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/"
  "The directory for my org files.")

(defconst rr-agenda-dir "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
  "The directory for RR-Emacs note storage.")

(defconst rr-notes-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/"
  "The directory for RR-Emacs note storage.")

;;;; Create directories if non-existing
(dolist (dir (list rr-cache-dir
		   rr-backup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package use-package-ensure-system-package)

(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package general :demand t
  :config
  (general-auto-unbind-keys))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(setq message-kill-buffer-on-exit t)

(setf use-short-answers t)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :defer t
  :config
  (global-auto-revert-mode)
  (setq magit-refresh-status-buffer nil
	magit-diff-highlight-indentation nil
	magit-diff-highlight-trailing nil
	magit-diff-paint-whitespace nil
	magit-diff-highlight-hunk-body nil
	magit-diff-refine-hunk nil
	magit-revision-insert-related-refs nil)
	    :commands
    (magit-after-save-refresh-status)
    :hook
    (after-save . magit-after-save-refresh-status)
    :custom
    (transient-history-file
     (expand-file-name "transient/history.el" rr-cache-dir))
    (transient-levels-file
     (expand-file-name "transient/levels.el" rr-cache-dir))
    (transient-values-file
     (expand-file-name "transient/values.el" rr-cache-dir)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
