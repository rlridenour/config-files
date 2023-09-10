;;; ox-arara.el --- LaTeX Arara Back-End for Org Export Engine -*- lexical-binding: t; -*-

(org-export-define-derived-backend 'arara 'latex
  :menu-entry
  '(?z "Export to arara-latex"
       ((?T "To temporary buffer" (lambda (a s v b) (org-arara-export-as-latex a s v)))
        (?t "To file" (lambda (a s v b) (org-arara-export-to-latex a s v)))
        (?o "To PDF file and open"
            (lambda (a s v b)
              (if a (org-arara-export-to-pdf t s v b)
		(org-open-file (org-arara-export-to-pdf nil s v b)))))))
  :translate-alist '((template . org-arara-template)))

(defun org-arara-export-as-latex (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'arara "*Org Arara Export*"
    async subtreep visible-only nil nil (lambda () (LaTeX-mode))))

(defun org-arara-export-to-latex (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'arara outfile async subtreep visible-only ext-plist)))

(defun org-arara-export-to-pdf (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'arara outfile
      async subtreep visible-only body-only ext-plist
      #'org-latex-compile)))

(defcustom org-arara-compiler-file-string "%% arara: latexmk: { engine: %s } \n"
  "Arara compiler format-string.")

(defun org-arara--insert-compiler (info)
  "Insert LaTeX_compiler info into the document.
INFO is a plist used as a communication channel."
  (let ((compiler (plist-get info :latex-compiler)))
    (and (org-string-nw-p org-arara-compiler-file-string)
	 (member (or compiler "") org-latex-compilers)
	 (format org-arara-compiler-file-string compiler))))


(defun org-arara-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(spec (org-latex--format-spec info)))
    (concat
     ;; LaTeX compiler.
     (org-arara--insert-compiler info)
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     ;; LaTeX displays today's date by default. One can override this by
     ;; inserting \date{} for no date, or \date{string} with any other
     ;; string to be displayed as the date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
	    (formatted-subtitle
	     (when subtitle
	       (format (plist-get info :latex-subtitle-format)
		       (org-export-data subtitle info))))
	    (separate (plist-get info :latex-subtitle-separate)))
       (concat
	(format "\\title{%s%s}\n" title
		(if separate "" (or formatted-subtitle "")))
	(when (and separate subtitle)
	  (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; engrave-faces-latex preamble
     (when (and (eq org-latex-src-block-backend 'engraved)
                (org-element-map (plist-get info :parse-tree)
                    '(src-block inline-src-block) #'identity
                    info t))
       (org-latex-generate-engraved-preamble info))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
	(cond ((not (plist-get info :with-title)) nil)
	      ((string= "" title) nil)
	      ((not (stringp command)) nil)
	      ((string-match "\\(?:[^%]\\|^\\)%s" command)
	       (format command title))
	      (t command))))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (integerp depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 (plist-get info :latex-toc-command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
	  (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))

(provide 'ox-arara)

;;; ox-arara.el ends here
