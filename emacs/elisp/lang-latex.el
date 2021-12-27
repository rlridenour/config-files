;;; lang-latex.el -*- lexical-binding: t; -*-

; LaTeX

(use-package tex-site
  :straight auctex
  :defer t
  :init
  (setq TeX-parse-self t
	TeX-auto-save t
  TeX-electric-math nil
  LaTeX-electric-left-right-brace nil
	TeX-electric-sub-and-superscript nil
  LaTeX-item-indent 0
	TeX-quote-after-quote nil
	TeX-clean-confirm nil
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex))

  ;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  ;; (setq TeX-view-program-list
	;; '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf -quiet -interaction=nonstopmode  %f"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)


;; Insert math-mode delimiters for LaTeX and ConTeXt.
;; (add-hook 'ConTeXt-mode-hook
;; 	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;; 			  (cons "$" "$"))))
;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;; 			  (cons "\\(" "\\)"))))

(add-hook 'LaTeX-mode-hook #'flymake-mode)

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :config (auctex-latexmk-setup))

;; Cdlatex makes inserting LaTeX easier.


(use-package cdlatex
  :init
  (setq cdlatex-make-sub-superscript-roman-if-pressed-twice t
        cdlatex-use-dollar-to-ensure-math nil
        cdlatex-takeover-dollar t
        cdlatex-paired-parens "([{")
  (setq cdlatex-math-symbol-alist
        '((?. ("\\land" "\\cdot"))
          (?v ("\\lor" "\\vee"))
          (?> ("\\lif" "\\rightarrow"))
          (?= ("\\liff" "\\Leftrightarrow" "\\Longleftrightarrow"))
          (?! ("\\lneg" "\\neg"))
          (?# ("\\Box"))
          (?$ ("\\Diamond"))
          ))
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'latex-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

;; Start Emacs server

;; (server-start)

;; Auto-raise Emacs on activation (from Skim, usually)

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' "))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

;; Configure Biber
;; Allow AucTeX to use biber as well as/instead of bibtex.


;; Biber under AUCTeX
(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber." 
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
	process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
			 "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			 "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
		     "Type `%s' to display output.")
	     (match-string 1) (match-string 2)
	     (substitute-command-keys
	      "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
		     "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
  )    

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
        '("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara.")))

(defun tex-clean ()
  (interactive)
  (shell-command "latexmk -c"))

(defun tex-clean-all ()
  (interactive)
  (shell-command "latexmk -C"))

(defun arara-all ()
  (interactive)
  (async-shell-command "mkall"))

;; Beamer

(setq LaTeX-paragraph-commands '("pause" "blpause"))


;; Dim tilde

(add-hook
 'TeX-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("~" . 'font-latex-sedate-face)))))

;; Start latexmk continuous preview.

(defun rlr/tex-pvc ()
  "Compile continuously with latexmk."
  (interactive)
  (async-shell-command (concat "mkpvc " (buffer-file-name)))
  (TeX-view))

;; Compile with arara

;; Run once

(defun rlr/tex-mkt ()
  "Compile with arara."
  (interactive)
  (async-shell-command (concat "mkt " (buffer-file-name))))

;; Run continuously

(defun rlr/tex-mktc ()
  "Compile continuously with arara."
  (interactive)
  (async-shell-command (concat "mktc " (buffer-file-name))))
;;   (TeX-view))



;; move to edited position after save.

(add-hook 'after-save-hook
	  (lambda ()
	    (when (string= major-mode 'latex-mode)
	      (TeX-view))))

;; Revert PDF after compilation has finished
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)


;; Use Ivy-bibtex for reference completions.
;; (use-package ivy-bibtex
;;   ;; :bind ("s-4" . ivy-bibtex)
;;   :after (ivy)
;;   :config
;;   (setq bibtex-completion-bibliography '("/Users/rlridenour/Dropbox/bibtex/rlr.bib"))
;;   (setq reftex-default-bibliography '("/Users/rlridenour/Dropbox/bibtex/rlr.bib"))
;;   (setq bibtex-completion-pdf-field "File")
;;   (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
;;   (setq bibtex-completion-format-citation-functions
;; 	'((org-mode      . bibtex-completion-format-citation-orgref)
;; 	  (latex-mode    . bibtex-completion-format-citation-cite)
;; 	  ;; (markdown-mode    . bibtex-completion-format-citation-cite)
;; 	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;; 	  (default       . bibtex-completion-format-citation-default))))

;; (use-package bibtex-actions
;;   :straight (:host github :repo "bdarcus/bibtex-actions")
;;   :config
;;   (setq bibtex-completion-bibliography "~/Dropbox/bibtex/rlr.bib")
;;   (setq bibtex-completion-format-citation-functions
;; 	'((org-mode      . bibtex-completion-format-citation-orgref)
;; 	  (latex-mode    . bibtex-completion-format-citation-cite)
;; 	  ;; (markdown-mode    . bibtex-completion-format-citation-cite)
;; 	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;; 	  (default       . bibtex-completion-format-citation-default))))



(use-package citar
  :custom
  (citar-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (citar-major-mode-functions
  '(((org-mode) .
     ((local-bib-files . citar-org-local-bib-files)
      (insert-citation . citar-latex-insert-citation)
      (insert-edit . citar-latex-insert-edit)
      (key-at-point . citar-latex-key-at-point)
      (citation-at-point . citar-latex-citation-at-point)))
    ((latex-mode) .
     ((local-bib-files . citar-latex-local-bib-files)
      (insert-citation . citar-latex-insert-citation)
      (insert-edit . citar-latex-insert-edit)
      (key-at-point . citar-latex-key-at-point)
      (citation-at-point . citar-latex-citation-at-point)))
    ((markdown-mode) .
     ((insert-keys . citar-markdown-insert-keys)
      (insert-citation . citar-markdown-insert-citation)
      (insert-edit . citar-markdown-insert-edit)
      (key-at-point . citar-markdown-key-at-point)
      (citation-at-point . citar-markdown-citation-at-point)))
    (t .
       ((insert-keys . citar--insert-keys-comma-separated))))))

(use-package ebib
  :defer t
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibtex/rlr.bib")))


(defun latex-word-count ()
  (interactive)
  (let* ((this-file (buffer-file-name))
         (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-brief" this-file)))))
    (string-match "\n$" word-count)
    (message (replace-match "" nil nil word-count))))


(provide 'lang-latex)
