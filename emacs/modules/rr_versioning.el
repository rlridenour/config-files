;;; rr_versioning.el --- Version Control  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Erwin Jansen

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
;;
;; The following package are used for version control
;; - diff-hl       ; git diff gutter
;; - magit         ; git mastery

;;; Code:

;;;; = diff-hl - a git cutter status indicator
(use-package diff-hl
  :if (display-graphic-p)
  :hook
  (prog-mode . diff-hl-mode))

;;;; = xmagit - emacs version control gui
;; magit is a full GUI application to manage git repositories.
(use-package magit
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


(provide 'rr_versioning)
;;; rr_versioning.el ends here



