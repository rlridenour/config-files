;;; org.el -*- lexical-binding: t; -*-

(use-package org
  :init
  ;; (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
  (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
  :config
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  ;; (setq org-footnote-section nil)
  (setq org-html-validation-link nil)
  (setq org-todo-keyword-faces
        '(("DONE" . "green4")))
  (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")))

;; Org-tempo is need for structure templates like "<s".

(require 'org-tempo)

;; I need to keep whitespace at the end of lines for my Beamer slides.

;; (add-hook 'text-mode-hook 'doom-disable-delete-trailing-whitespace-h)

(use-package orgonomic
  :defer t
  :straight (orgonomic :host github :repo "aaronjensen/emacs-orgonomic")
  :hook (org-mode . orgonomic-mode))

;; Some export settings

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
                 "\\documentclass{pdfhandout}
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

(setq org-export-with-smart-quotes t)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-export-smart-quotes-alist 
               '("en-us"
                 (primary-opening   :utf-8 "“" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
                 (primary-closing   :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
                 (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
                 (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
                 (apostrophe        :utf-8 "’" :html "&rsquo;")))

  )


(defun  
    arara-slides ()
  ;; (interactive)
  (async-shell-command-no-window "mkslides"))

(defun  
    arara-notes ()
  ;; (interactive)
  (async-shell-command-no-window "mknotes"))


(defun lecture-slides ()
  "publish org data file as beamer slides"
  (interactive)
  (find-file "*-slides.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (arara-slides)
  (find-file "*-data.org" t))


(defun lecture-notes ()
  "publish org data file as beamer notes"
  (interactive)
  (find-file "*-notes.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (arara-notes)
  (find-file "*-data.org" t))

(defun present ()
  (interactive)
  (async-shell-command "present"))

(defun canvas-copy ()
  "Copy html for canvas pages"
  (interactive)
  (org-html-export-to-html)
  (shell-command "canvas")
  )

(setq org-latex-pdf-process '("arara %f"))

(defun rlr/dwim-mkt ()
  "Run arara and open PDF."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Compile with arara"
   "mkt <<f>>"
   :silent-success t
   )
  )
(defun rlr/org-mkt ()
  "Make PDF with Arara."
  (interactive)
  (org-latex-export-to-latex)
  (async-shell-command-no-window (concat "mkt " (shell-quote-argument(file-name-sans-extension (buffer-file-name)))".tex")))

(defun rlr/dwim-org-mkt ()
  "Make PDF with Arara."
  (interactive)
  (org-latex-export-to-latex)
  (dwim-shell-command-on-marked-files
   "Compile with arara"
   "mkt <<fne>>.tex"
   :silent-success t
   )
  )


(defun rlr/org-mktc ()
  "Compile continuously with arara."
  (interactive)
  (org-latex-export-to-latex)
  (start-process-shell-command (concat "mktc-" (buffer-file-name)) (concat "mktc-" (buffer-file-name)) (concat "mktc " (shell-quote-argument(file-name-sans-extension (buffer-file-name)))".tex")))

;; Org-capture
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org" "Inbox")
         "** TODO %?\n  %i\n  %a")
        ("b" "Bookmark" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org" "Bookmarks")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        )
      )
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

(define-key global-map "\C-cc" 'org-capture)

;; Org super agenda

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "Important"
                             :priority "A"
                             :order 4)
                            (:name "Today's tasks"
                             :file-path "journal/")
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Overdue"
                             :deadline past
                             :order 3)
                            (:discard (:not (:todo "TODO")))))))))))
  :config
  (org-super-agenda-mode))


;; Display 7 full days in the agenda.
(setq org-agenda-span 7)


;; Bibtex

(use-package citar
  :bind (("C-c C-b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (org-cite-global-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (citar-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (org-cite-csl-styles-dir "/usr/local/texlive/2023/texmf-dist/tex/latex/citation-style-language/styles"))

;; Use ebib for bibtex file management


(use-package ebib
  :defer
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  ;;(evil-set-initial-state 'ebib-index-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-entry-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-log-mode 'emacs)
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibtex/rlr.bib")))


;; Denote
(use-package denote
  :config
  (setq denote-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes")
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-date-format nil)
  )

(use-package consult-notes
  :config
  (setq consult-notes-sources
        `(("Notes"  ?n ,denote-directory)
          ;; ("Books"  ?b "~/Documents/books")
          )))

(defun my-denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %B %d %Y") ; format like Tuesday June 14 2022
   '("journal"))) ; multiple keywords are a list of strings: '("one" "two")


(use-package citar-denote
  :after citar denote
  :config
  (citar-denote-mode)
  (setq citar-open-always-create-notes t))

(use-package denote-menu) 


(provide 'org)


;;; org.el ends here