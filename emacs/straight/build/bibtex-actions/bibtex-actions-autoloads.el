;;; bibtex-actions-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "bibtex-actions" "bibtex-actions.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from bibtex-actions.el

(autoload 'bibtex-actions-refresh "bibtex-actions" "\
Reload the candidates cache.
If called interactively with a prefix or if FORCE-REBUILD-CACHE
is non-nil, also run the `bibtex-actions-before-refresh-hook' hook.

\(fn &optional FORCE-REBUILD-CACHE)" t nil)

(autoload 'bibtex-actions-insert-preset "bibtex-actions" "\
Prompt for and insert a predefined search." t nil)

(autoload 'bibtex-actions-open "bibtex-actions" "\
Open PDF, or URL or DOI link.
Opens the PDF(s) associated with the KEYS.  If multiple PDFs are
found, ask for the one to open using ‘completing-read’.  If no
PDF is found, try to open a URL or DOI in the browser instead.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-open-pdf "bibtex-actions" "\
Open PDF associated with the KEYS.
If multiple PDFs are found, ask for the one to open using
‘completing-read’.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-open-link "bibtex-actions" "\
Open URL or DOI link associated with the KEYS in a browser.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-insert-citation "bibtex-actions" "\
Insert citation for the KEYS.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-insert-reference "bibtex-actions" "\
Insert formatted reference(s) associated with the KEYS.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-insert-key "bibtex-actions" "\
Insert BibTeX KEYS.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-insert-bibtex "bibtex-actions" "\
Insert BibTeX entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-add-pdf-attachment "bibtex-actions" "\
Attach PDF(s) associated with the KEYS to email.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-open-notes "bibtex-actions" "\
Open notes associated with the KEYS.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-open-entry "bibtex-actions" "\
Open BibTeX entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-add-pdf-to-library "bibtex-actions" "\
Add PDF associated with the KEYS to library.
The PDF can be added either from an open buffer, a file, or a
URL.
With prefix, rebuild the cache before offering candidates.

\(fn KEYS)" t nil)

(autoload 'bibtex-actions-dwim "bibtex-actions" "\
Run the default action on citation keys found at point." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bibtex-actions" '("bibtex-actions-")))

;;;***

;;;### (autoloads nil "bibtex-actions-org-cite" "bibtex-actions-org-cite.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bibtex-actions-org-cite.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bibtex-actions-org-cite" '("bibtex-actions-org-cite-")))

;;;***

(provide 'bibtex-actions-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bibtex-actions-autoloads.el ends here
