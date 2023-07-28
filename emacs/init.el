;;; init.el --- Pure Emacs initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Erwin Jansen <echweb@outlook.com>

;; This file is part of = P U R E - E M A C S =
;;
;; pure-emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; pure-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with pure-emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Early decisions
;; - Elpaca package management, as it appears the future
;; - Use-package for package management, as it is built-in (Emacs 29.1)

;;; Code:

;;;; Set Directories
(defconst rr-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst rr-modules-dir (concat rr-emacs-dir "modules/")
  "The directory for RR-Emacs Lisp module configurations.")

(defconst rr-cache-dir "~/.cache/emacs/"
  "The directory for RR-Emacs activity files.")

(defconst rr-backup-dir (concat rr-cache-dir "backup/")
  "The directory for RR-Emacs backup files.")

(defconst rr-notes-dir "~/Org/Denote/"
  "The directory for RR-Emacs note storage.")

;;;; Create directories if non-existing
(dolist (dir (list rr-modules-dir
		   rr-cache-dir
		   rr-backup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;; Load Path
;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push rr-modules-dir load-path)))

;;;; Fonts
(defvar rr-fixed-pitch-font "SF Mono"
  "Font used for fixed-pitch faces.")

;;;; Load RR-Emacs modules
(cl-dolist (mod (list
		 'rr_packman		  ; Elpaca package manager
		 'rr_settings		  ; Default settings
 		 ;'rr_appearance	  ; Emacs looks and feel
		 ;'rr_keybindings	  ; Standard key mapping
		 ;'rr_keybindings_modern ; Modern key mapping
		 ;'rr_completion	  ; Minibuffer completion
		 ;'rr_help		  ; Always more help
		 ;'rr_display		  ; Emacs windows
		 ;'rr_editing		  ; Editing
		 ;'rr_org		  ; Writing
		 ;'rr_email		  ; Mail
		 ;'rr_notes		  ; Note taking
		 ;'rr_buffers		  ; Buffers
		 ;'rr_navigation	  ; Getting around
		 ;'rr_development	  ; Development tools
		 'rr_versioning	  ; Version control
		 ;'rr_tools		  ; Whatever is left
                 ))
  (require mod nil t))

(provide 'init)
;;; init.el ends here
