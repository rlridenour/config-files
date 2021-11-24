;;; lang-org.el -*- lexical-binding: t; -*-

;; (use-package org
  ;; :straight org-plus-contrib)

(use-package org
  :init
 ;; (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
 (setq org-directory "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
  :config
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (setq org-html-validation-link nil)
  ;; (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/tasks/")
        (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
        )
  )


(use-package org-contrib
  :straight (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"))

(require 'org-tempo)

;; manage citations
(require 'org-bibtex-extras)

;; export citations
(require 'ox-bibtex)
(setq org-bibtex-file "~/Dropbox/bibtex/org/rlr.org")

;; Enable ignoring a headline during export.
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))


(require 'ox-latex)

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
			 '("org-article"
			   "\\documentclass{article}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
			   ("\\section{%s}" . "\\section*{%s}")
			   ("\\subsection{%s}" . "\\subsection*{%s}")
			   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
			 '("org-handout"
			   "\\documentclass{obuhandout}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
			   ("\\section{%s}" . "\\section*{%s}")
			   ("\\subsection{%s}" . "\\subsection*{%s}")
			   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
			 '("org-beamer"
			   "\\documentclass{beamer}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
			   ("\\section{%s}" . "\\section*{%s}")
			   ("\\subsection{%s}" . "\\subsection*{%s}")
			   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
)

;; (add-hook 'org-mode-hook 'wc-mode)


(defun flyspell-ignore-tex ()
	(interactive)
	(set (make-variable-buffer-local 'ispell-parser) 'tex))
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
(add-hook 'org-mode-hook 'flyspell-ignore-tex)

(use-package org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
	org-ref-default-bibliography '("~/bibtex/rlr-bib/rlr.bib")))


;; Return adds new heading or list item. From https://github.com/aaronjensen/emacs-orgonomic
(use-package orgonomic
  :defer t
  :straight (orgonomic :host github :repo "aaronjensen/emacs-orgonomic")
  :hook (org-mode . orgonomic-mode)
  :bind (
         :map orgonomic-mode-map
         ("<S-s-return>" . orgonomic-shift-return)
         ("<S-return>" . crux-smart-open-line)))


;; Functions for automating lecture notes and slides


(defun lecture-slides ()
  "publish org file as beamer slides and notes"
  (interactive)
  (find-file "*-slides.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (find-file "*-notes.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
					;(kill-buffer "*.tex")
  (arara-all)
  (find-file "*-data.org" t))

(defun canvas-copy ()
  "Copy html for canvas pages"
  (interactive)
  (org-html-export-to-html)
  (shell-command "canvas")
)

(setq org-latex-pdf-process '("arara %f"))

(defun rlr/org-mkt ()
  "Make PDF with Arara."
  (interactive)
  (org-latex-export-to-latex)
  (async-shell-command (concat "mkt " (file-name-sans-extension (buffer-file-name))".tex")))

(defun rlr/org-mktc ()
  "Compile continuously with arara."
  (interactive)
  (org-latex-export-to-latex)
  (start-process-shell-command (concat "mktc-" (buffer-file-name)) (concat "mktc-" (buffer-file-name)) (concat "mktc " (file-name-sans-extension (buffer-file-name))".tex")))


;; Org-capture

(setq org-capture-templates
      ;; '(("t" "Todo" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/tasks/inbox.org" "Tasks")
         '(("t" "Todo" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))

(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

;; Archive settings

(setq org-archive-location "~/Library/Mobile Documents/com~apple~CloudDocs/org/archive/%s::datetree/")

;; Org-roam

;; (use-package org-journal
;;   :after org
;;       :bind
;;       ("C-c n j" . org-journal-new-entry)
;;       :custom
;;       (org-journal-dir "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
;;       (org-journal-date-prefix "#+TITLE: ")
;;       (org-journal-file-format "%Y-%m-%d.org")
;;       (org-journal-date-format "%A, %d %B %Y"))
;;     (setq org-journal-enable-agenda-integration t)

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-extensions '("org" "md" "txt"))
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-default-extension "org")
  (deft-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/notes/"))


(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/roam/"))
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;            ("C-c n f" . org-roam-node-find)
  ;;            ("C-c n g" . org-roam-graph)
  ;;            ("C-c n i" . org-roam-node-insert)
  ;;            ("C-c n c" . org-roam-capture)
  ;;            ;; Dailies
  ;;            ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup))


(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(use-package org-download
  :init
  (setq org-image-actual-width nil)
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))


(defun rlr/org-date ()
  "Update existing date: timestamp on a Hugo post."
  (interactive)
  (save-excursion (
                   goto-char 1)
                  (re-search-forward "^#\\+date:")
                  (let ((beg (point)))
                    (end-of-line)
                    (delete-region beg (point)))
                  (insert (concat " " (format-time-string "%B %e, %Y")))))

(provide 'lang-org)
