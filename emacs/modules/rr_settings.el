;;; rr_settings.el --- Default Settings -*- lexical-binding: t -*-

;; This file is part of = P U R E - E M A C S =
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
;; along with rr-emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;; File handling

;;;;; = files - files and backups
(use-feature files
  :custom
  ;; Where to save to backuo file - in the backup dir
  (backup-directory-alist (list (cons "."  rr-backup-dir)))
  ;; Always backup by copying
  (backup-by-copying t)
  ;; Delete old backup files
  (delete-old-version t)
  ;; Keep 5 backup files
  (kept-new-versions 5)
  ;; Make numberic backup versions
  (version-control t)
  ;; Do not automatically save
  (auto-save-default nil)
  :hook
  ;; Remove trailing white spaces when saving
  (before-save-hook . delete-trailing-whitespace))


;;;;; = recentf - recently opened files
;; Maintains a list of recently opened files
(use-feature recentf
  :custom
  ;; Where to save the recentf file - in the .cache
  (recentf-save-file (expand-file-name "recentf" rr-cache-dir))
  ;; Remove duplicates on mode change
  (recentf-auto-cleanup 'mode)
  ;; Max number of files saved
  (recentf-max-saved-items 100)
  ;; Max number of files served in files menu
  (recentf-max-menu-items 15)
  :config
  (recentf-mode t))

;;;;; = saveplace - last position in file
;; Save point position in files between sessions.
(use-feature saveplace
  :custom
  ;; Where to save the saveplaces file - in the .cache
  (save-place-file (expand-file-name "saveplaces" rr-cache-dir))
  :config
  (save-place-mode))

;;;;; = savehist - last commands used
;; Persist emacs minibuffer history
(use-feature savehist
  ;; Where to save the savehsit file - in the .cache
  :custom
  (savehist-file (expand-file-name "savehist" rr-cache-dir))
  :config
  (savehist-mode))

;;;; Parenthesis

;;;;;; = elec-pair
;; Auto insert oposite parenthesis
(use-feature elec-pair
  :disabled
  :hook
  ((text-mode . electric-pair-local-mode)
   (prog-mode . electric-pair-local-mode)))

(provide 'rr_settings)
;;; rr_settings.el ends here






