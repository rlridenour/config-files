;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Randy Ridenour"
      user-mail-address "rlridenour@gmail.com")

(setq doom-font (font-spec :family "Droid Sans Mono Slashed" :size 16)
      doom-big-font (font-spec :family "Droid Sans Mono Slashed" :size 22)
      doom-variable-pitch-font (font-spec :family "Droid Sans" :size 16))

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(use-package! modus-themes
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-fringes 'intense ; {nil,'subtle,'intense}
        modus-themes-mode-line '3d ; {nil,'3d,'moody}
        modus-themes-syntax 'faint ; Lots of options---continue reading the manual
        modus-themes-intense-hl-line nil
        modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
        modus-themes-links 'faint-neutral-underline ; Lots of options---continue reading the manual
        modus-themes-no-mixed-fonts t
        modus-themes-prompts 'intense ; {nil,'subtle,'intense}
        modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
        modus-themes-region 'bg-only-no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs nil ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings t
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)
  :config
  (setq doom-theme 'modus-operandi))

(setq-default
 uniquify-buffer-name-style 'forward)
(display-time-mode 1)
(display-battery-mode 1)
(show-smartparens-global-mode t)
(global-visual-line-mode t)

(setq confirm-kill-processes nil)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-omit-extensions '("fdb_latexmk" "aux" "bbl" "blg" "fls" "glo" "idx" "ilg" "ind" "ist" "log" "out" "gz" "bcf" "run.xml"  "DS_Store"))
(setq dired-dwim-target t)

(load "~/Dropbox/emacs/my-emacs-abbrev")

(setq default-input-method 'TeX)

(after! yasnippet
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.config/doom/snippets"))))

(defun insert-date-string ()
  "Insert current date yyyymmdd."
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%B %e, %Y")))
(global-set-key (kbd "<f8>") 'insert-standard-date)
(global-set-key (kbd "C-c d") 'insert-date-string)

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-personal-dictionary "/Users/rlridenour/Dropbox/emacs/spelling/.aspell.en.pws")
(setq ispell-silently-savep t)

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

(defun goto-scratch ()
  "this sends you to the scratch buffer"
  (interactive)
  (let ((goto-scratch-buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer goto-scratch-buffer)
    (org-mode)))

(map! "M-g s" #'goto-scratch)

(fset 'split-org-item
      [?\C-k ?\M-\\ return ?\C-y])

(fset 'convert-markdown-to-org
      [?\M-< ?\M-% ?* return ?- return ?! ?\M-< ?\C-\M-% ?# ?* backspace backspace ?  ?# ?* ?$ return return ?! ?\M-< ?\M-% ?# return ?* return ?!])

(use-package! grab-mac-link
  :defer
  :config
  (setq grab-mac-link-dwim-favourite-app 'safari)
  (map! "C-c l" #'grab-mac-link-dwim))

(use-package! deadgrep
  :defer)

(use-package! hungry-delete
  :defer
  :config
  (global-hungry-delete-mode))

(use-package! shrink-whitespace
  :defer)
(map! "C-M-s-d" #'shrink-whitespace)

(use-package! ebib
:defer
:config
(setq ebib-bibtex-dialect 'biblatex)
:custom
(ebib-preload-bib-files '("~/bibtex/rlr-bib/rlr.bib")))

(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org/")

(after! org (setq org-startup-indented nil
                  org-adapt-indentation nil))

(after! org (setq org-hide-leading-stars nil))

(after! org (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/tasks/")))

(after! org (setq org-insert-heading-respect-content nil))

(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(require 'org-tempo)

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(add-hook 'org-mode-hook 'wc-mode)

(use-package! org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-default-bibliography '("~/bibtex/rlr-bib/rlr.bib")))

(use-package! orgonomic
  :after org
  :hook (org-mode . orgonomic-mode))

(defun lecture-slides ()
  "publish org file as beamer slides and notes"
  (interactive)
  (find-file "*-slides.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (find-file "*-notes.org" t)
  (org-beamer-export-to-latex)
  (kill-buffer)
  (arara-all)
  (find-file "*-data.org" t))

(defun canvas-copy ()
  "Copy html for canvas pages"
  (interactive)
  (org-html-export-to-html)
  (shell-command "canvas")
  )

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

(add-to-list 'org-latex-classes
             '("rlr-obu-letter"
               "\\documentclass[12pt]{obuletter}

  % Customize variables --- for the entire list, see obuletter.cls and myletter.cls.
  \\newcommand{\\obuDept}{Department of Philosophy} % For personal letterhead, use name here.
  \\newcommand{\\Sender}{Randy Ridenour, Ph.D.}
  \\newcommand{\\obuTitle}{Professor of Philosophy}
  \\newcommand{\\obuCollege}{Hobbs College of Theology and Ministry} % For personal letterhead, use title here.
  \\newcommand{\\obuPhone}{405.585.4432}
  \\newcommand{\\obuFax}{405.878.2401}
  \\newcommand{\\closing}{Sincerely,}
  \\newcommand{\\toName}{Recipient}
  \\newcommand{\\toAddress}{Street Address\\\\City, State ZIP}

          [NO-DEFAULT-PACKAGES]
          [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("rlr-personal-letter"
               "\\documentclass[12pt]{myletter}

  % Customize variables --- for the entire list, see obuletter.cls and myletter.cls.
  \\newcommand{\\Sender}{Randy Ridenour}
  \\newcommand{\\closing}{Sincerely,}
  \\newcommand{\\toName}{Recipient}
  \\newcommand{\\toAddress}{Street Address\\\\City, State ZIP}

          [NO-DEFAULT-PACKAGES]
          [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq company-idle-delay 1.0)

(use-package! deft
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

(use-package! org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (file-truename "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/roam/"))
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))

(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+ROAM_TAGS: \n"
         :unnarrowed t)))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))

;; (add-hook 'LaTeX-mode-hook #'mixed-pitch-mode)

(setq reftex-default-bibliography "~/Dropbox/bibtex/rlr.bib")
(setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf -quiet -interaction=nonstopmode %f"))
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

;; (setq bibtex-completion-bibliography
;;       '("~/bibtex/rlr-bib/rlr.bib"))

(use-package cdlatex
  :defer
  :init
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
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))


(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)



;; Configure AucTeX
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

(defun tex-clean ()
  (interactive)
  (shell-command "latexmk -c"))


(defun tex-clean-all ()
  (interactive)
  (shell-command "latexmk -C"))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Arara" "arara --verbose %s" TeX-run-TeX nil t :help "Run Arara.")))

(defun arara-all ()
  (interactive)
  (async-shell-command "mkall"))

(setq bibtex-completion-cite-default-command "autocite")
(defun bibtex-completion-format-citation-orgref (keys)
  "Formatter for Org mode citation commands.
Prompts for the command and for arguments if the commands can
take any.  If point is inside or just after a citation command,
only adds KEYS to it."
  (let (macro)
    (cond
     ((and (require 'reftex-parse nil t)
           (setq macro (reftex-what-macro 1))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are inside a cite macro.  Insert key at point, with appropriate delimiters.
      (delete-horizontal-space)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              (if (member (following-char) '(?\} ?,))
                  ""
                ", ")))
     ((and (equal (preceding-char) ?\})
           (require 'reftex-parse nil t)
           (save-excursion
             (forward-char -1)
             (setq macro (reftex-what-macro 1)))
           (stringp (car macro))
           (string-match "\\`\\\\cite\\|cite\\'" (car macro)))
      ;; We are right after a cite macro.  Append key and leave point at the end.
      (delete-char -1)
      (delete-horizontal-space t)
      (concat (pcase (preceding-char)
                (?\{ "")
                (?, " ")
                (_ ", "))
              (s-join ", " keys)
              "}"))
     (t
      ;; We are not inside or right after a cite macro.  Insert a full citation.
      (let* ((initial (when bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default (unless bibtex-completion-cite-default-as-initial-input
                        bibtex-completion-cite-default-command))
             (default-info (if default (format " (default \"%s\")" default) ""))
             (cite-command (completing-read
                            (format "Cite command%s: " default-info)
                            bibtex-completion-cite-commands nil nil initial
                            'bibtex-completion-cite-command-history default nil)))
        (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
            (format "\\%s{%s}" cite-command (s-join ", " keys))
          (let ((prenote (if bibtex-completion-cite-prompt-for-optional-arguments
                             (read-from-minibuffer "Prenote: ")
                           ""))
                (postnote (if bibtex-completion-cite-prompt-for-optional-arguments
                              (read-from-minibuffer "Postnote: ")
                            "")))
            (cond ((and (string= "" prenote) (string= "" postnote))
                   (format "[[%s:%s]]" cite-command (s-join ", " keys)))
                  (t
                   (format "[[%s:%s][%s::%s]]" cite-command (s-join ", " keys) prenote postnote)
                   )))))))))

(use-package! ivy-bibtex
  ;; :bind ("s-4" . ivy-bibtex)
  :after (ivy)
  :config
  (setq bibtex-completion-bibliography '("~/bibtex/rlr-bib/rlr.bib"))
  (setq reftex-default-bibliography '("~/bibtex/rlr-bib/rlr.bib"))
  (setq bibtex-completion-pdf-field "File")
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-orgref)
          (latex-mode    . bibtex-completion-format-citation-cite)
          ;; (markdown-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(add-hook! #'mixed-pitch-mode)

(use-package! markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-asymmetric-header t))

(use-package! ox-hugo
  :after org
  :init
  (setq org-hugo-delete-trailing-ws nil
        org-hugo-auto-set-lastmod t
org-hugo-suppress-lastmod-period 86400.0))

(defvar hugo-directory "~/Sites/blog/" "Path to Hugo blog.")
(defvar hugo-posts-dir "content/posts/" "Relative path to posts directory.")

(defun hugo-make-slug (s) "Turn a string into a slug."
       (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun new-post (title) "Create a new blog post."
       (interactive "sPost Title: ")
       (insert "** TODO " title"\n:PROPERTIES:\n:EXPORT_FILE_NAME: "(format-time-string "%Y%m%d-")(hugo-make-slug title)"\n:EXPORT_DATE "(format-time-string "%Y-%m-%d")"\n:END:\n\n"))

;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
  See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " (format-time-string "%Y%m%d-") fname)
                   ,(concat ":EXPORT_DATE: " (format-time-string "%Y-%m-%dT%H:%M:%S"))
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "blog.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))

(defun hugo-timestamp ()
  "Update existing date: timestamp on a Hugo post."
  (interactive)
  (save-excursion (
                   re-search-forward "^:EXPORT_DATE:")
                  (let ((beg (point)))
                    (end-of-line)
                    (delete-region beg (point)))
                  (insert (concat " " (format-time-string "%Y-%m-%dT%H:%M:%S")))))

(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(progn (setq ,orig-dir default-directory)
            (cd ,DIR) ,@FORMS (cd ,orig-dir))))

(defun hugo-deploy ()
  "Push changes upstream."
  (interactive)
  (with-dir hugo-directory
            (shell-command "git add .")
            (--> (current-time-string)
                 (concat "git commit -m \"" it "\"")
                 (shell-command it))
            (magit-push-current-to-upstream nil)))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-h"))
(global-unset-key (kbd "s-w"))

(use-package! major-mode-hydra
  :defer)
(map! "s-m" #'major-mode-hydra)

(major-mode-hydra-define markdown-mode
  (:quit-key "q")
  ("Format"
   (("h" markdown-insert-header-dwim "header")
    ("l" markdown-insert-link "link")
    ("u" markdown-insert-uri "url")
    ("f" markdown-insert-footnote "footnote")
    ("w" markdown-insert-wiki-link "wiki")
    ("r" markdown-insert-reference-link-dwim "r-link")
    ("n" markdown-cleanup-list-numbers "clean-lists")
    ("c" markdown-complete-buffer "complete"))))

(major-mode-hydra-define latex-mode
  (:quit-key "q")
  ("Bibtex"
   (("b" ivy-bibtex "Ivy-Bibtex"))
   "LaTeXmk"
   (("p" rlr/tex-pvc "pvc")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all"))))

(major-mode-hydra-define org-mode
  (:quit-key "q")
  ("Export"
   (("l" org-latex-export-to-latex "Org to LaTeX")
    ("p" org-latex-export-to-pdf "Org to PDF")
    ("b" org-beamer-export-to-pdf "Org to Beamer-PDF")
    ("B" org-beamer-export-to-latex "Org to Beamer-LaTeX")
    ("s" lecture-slides "Lecture slides")
    )
   "Bibtex"
   (("r" ivy-bibtex "Ivy-Bibtex"))
   "View"
   (("p" org-toggle-pretty-entities "org-pretty"))
   "Clean"
   (("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all"))))

(major-mode-hydra-define dired-mode
  (:quit-key "q")
  ("Tools"
   (("d" crux-open-with "Open in default program")
    ("p" diredp-copy-abs-filenames-as-kill "Copy filename and path")
    ("n" dired-toggle-read-only "edit Filenames"))))

(pretty-hydra-define hydra-toggle
  (:color blue :quit-key "q" :title "Toggle")
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("d" toggle-debug-on-error "debug" (default value 'debug-on-error))
    ("i" aggressive-indent-mode "indent" :toggle t)
    ("f" auto-fill-mode "fill" :toggle t)
    ("l" display-line-numbers-mode "linum" :toggle t)
    ("m" toggle-frame-maximized-undecorated "max" :toggle t)
    ("p" smartparens-mode "smartparens" :toggle t)
    ("t" toggle-truncate-lines "truncate" :toggle t)
    ("s" whitespace-mode "whitespace" :toggle t)
    ("C" company-mode "company" :toggle t))
   "Writing"
   (("c" cdlatex-mode "cdlatex" :toggle t)
    ("o" olivetti-mode "olivetti" :toggle t)
    ("r" read-only-mode "read-only" :toggle t)
    ("w" wc-mode "word-count" :toggle t))))

(defun my/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(pretty-hydra-define hydra-logic
  (:color blue :title "Logic")
  ("Operators"
   (("1" (my/insert-unicode "NOT SIGN") "¬" :exit nil)
    ("2" (my/insert-unicode "AMPERSAND") "&" :exit nil)
    ("3" (my/insert-unicode "LOGICAL OR") "v" :exit nil)
    ("4" (my/insert-unicode "RIGHTWARDS ARROW") "→" :exit nil)
    ("5" (my/insert-unicode "LEFT RIGHT ARROW") "↔" :exit nil)
    ("6" (my/insert-unicode "THERE EXISTS") "∃" :exit nil)
    ("7" (my/insert-unicode "FOR ALL") "∀" :exit nil)
    ("8" (my/insert-unicode "WHITE MEDIUM SQUARE") "□" :exit nil)
    ("9" (my/insert-unicode "LOZENGE") "◊" :exit nil))
   "Lowercase"
   (("a" (my/insert-unicode "LATIN SMALL LETTER A") "a" :exit nil)
    ("b" (my/insert-unicode "LATIN SMALL LETTER B") "b" :exit nil)
    ("c" (my/insert-unicode "LATIN SMALL LETTER C") "c" :exit nil)
    ("d" (my/insert-unicode "LATIN SMALL LETTER D") "d" :exit nil)
    ("e" (my/insert-unicode "LATIN SMALL LETTER E") "e" :exit nil)
    ("f" (my/insert-unicode "LATIN SMALL LETTER F") "f" :exit nil)
    ("g" (my/insert-unicode "LATIN SMALL LETTER G") "g" :exit nil)
    ("h" (my/insert-unicode "LATIN SMALL LETTER H") "h" :exit nil)
    ("i" (my/insert-unicode "LATIN SMALL LETTER I") "i" :exit nil)
    ("j" (my/insert-unicode "LATIN SMALL LETTER J") "j" :exit nil)
    ("k" (my/insert-unicode "LATIN SMALL LETTER K") "k" :exit nil)
    ("l" (my/insert-unicode "LATIN SMALL LETTER L") "l" :exit nil)
    ("m" (my/insert-unicode "LATIN SMALL LETTER M") "m" :exit nil)
    ("n" (my/insert-unicode "LATIN SMALL LETTER N") "n" :exit nil)
    ("o" (my/insert-unicode "LATIN SMALL LETTER O") "o" :exit nil)
    ("p" (my/insert-unicode "LATIN SMALL LETTER P") "p" :exit nil)
    ("q" (my/insert-unicode "LATIN SMALL LETTER Q") "q" :exit nil)
    ("r" (my/insert-unicode "LATIN SMALL LETTER R") "r" :exit nil)
    ("s" (my/insert-unicode "LATIN SMALL LETTER S") "s" :exit nil)
    ("t" (my/insert-unicode "LATIN SMALL LETTER T") "t" :exit nil)
    ("u" (my/insert-unicode "LATIN SMALL LETTER U") "u" :exit nil)
    ("v" (my/insert-unicode "LATIN SMALL LETTER V") "v" :exit nil)
    ("w" (my/insert-unicode "LATIN SMALL LETTER W") "w" :exit nil)
    ("x" (my/insert-unicode "LATIN SMALL LETTER X") "x" :exit nil)
    ("y" (my/insert-unicode "LATIN SMALL LETTER Y") "y" :exit nil)
    ("z" (my/insert-unicode "LATIN SMALL LETTER Z") "z" :exit nil))
   "Uppercase"
   (("A" (my/insert-unicode "LATIN CAPITAL LETTER A") "A" :exit nil)
    ("B" (my/insert-unicode "LATIN CAPITAL LETTER B") "B" :exit nil)
    ("C" (my/insert-unicode "LATIN CAPITAL LETTER C") "C" :exit nil)
    ("D" (my/insert-unicode "LATIN CAPITAL LETTER D") "D" :exit nil)
    ("E" (my/insert-unicode "LATIN CAPITAL LETTER E") "E" :exit nil)
    ("F" (my/insert-unicode "LATIN CAPITAL LETTER F") "F" :exit nil)
    ("G" (my/insert-unicode "LATIN CAPITAL LETTER G") "G" :exit nil)
    ("H" (my/insert-unicode "LATIN CAPITAL LETTER H") "H" :exit nil)
    ("I" (my/insert-unicode "LATIN CAPITAL LETTER I") "I" :exit nil)
    ("J" (my/insert-unicode "LATIN CAPITAL LETTER J") "J" :exit nil)
    ("K" (my/insert-unicode "LATIN CAPITAL LETTER K") "K" :exit nil)
    ("L" (my/insert-unicode "LATIN CAPITAL LETTER L") "L" :exit nil)
    ("M" (my/insert-unicode "LATIN CAPITAL LETTER M") "M" :exit nil)
    ("N" (my/insert-unicode "LATIN CAPITAL LETTER N") "N" :exit nil)
    ("O" (my/insert-unicode "LATIN CAPITAL LETTER O") "O" :exit nil)
    ("P" (my/insert-unicode "LATIN CAPITAL LETTER P") "P" :exit nil)
    ("Q" (my/insert-unicode "LATIN CAPITAL LETTER Q") "Q" :exit nil)
    ("R" (my/insert-unicode "LATIN CAPITAL LETTER R") "R" :exit nil)
    ("S" (my/insert-unicode "LATIN CAPITAL LETTER S") "S" :exit nil)
    ("T" (my/insert-unicode "LATIN CAPITAL LETTER T") "T" :exit nil)
    ("U" (my/insert-unicode "LATIN CAPITAL LETTER U") "U" :exit nil)
    ("V" (my/insert-unicode "LATIN CAPITAL LETTER V") "V" :exit nil)
    ("W" (my/insert-unicode "LATIN CAPITAL LETTER W") "W" :exit nil)
    ("X" (my/insert-unicode "LATIN CAPITAL LETTER X") "X" :exit nil)
    ("Y" (my/insert-unicode "LATIN CAPITAL LETTER Y") "Y" :exit nil)
    ("Z" (my/insert-unicode "LATIN CAPITAL LETTER Z") "Z" :exit nil))
   "Parens"
   (("(" (my/insert-unicode "LEFT PARENTHESIS") "(" :exit nil)
    (")" (my/insert-unicode "RIGHT PARENTHESIS") ")" :exit nil)
    ("[" (my/insert-unicode "LEFT SQUARE BRACKET") "[" :exit nil)
    ("]" (my/insert-unicode "RIGHT SQUARE BRACKET") "]" :exit nil)
    ("{" (my/insert-unicode "LEFT CURLY BRACKET") "{" :exit nil)
    ("}" (my/insert-unicode "RIGHT CURLY BRACKET") "}" :exit nil))
   "Space"
   (("<SPC>" (my/insert-unicode "SPACE") "Space" :exit nil)
    ("?" (my/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space" :exit nil)
    ("<left>" backward-char "move-left" :exit nil)
    ("<right>" forward-char "move-right" :exit nil)
    ("<kp-delete>" delete-char "delete" :exit nil))))

(pretty-hydra-define hydra-hugo
  (:color blue :quit-key "q" :title "Hugo")
  ("Blog"
   (("n" hugo-draft-post "New draft")
    ("p" hugo-publish-post "Publish post")
    ("t" hugo-timestamp "Update timestamp")
    ("d" hugo-deploy "Deploy")
    ("h" hugo-posts-dir "Posts"))))

(map! "s-h" #'hydra-hugo/body
      "s-l" #'hydra-logic/body
      "s-t" #'hydra-toggle/body
      )

(map!
 ("<s-backspace>" 'kill-whole-line)
 ("<s-up>"  'beginning-of-buffer)
 ("<s-down>" 'end-of-buffer))

(map! "s-b" #'counsel-switch-buffer
      "s-r" #'counsel-buffer-or-recentf)

(map! "M-;" #'evilnc-comment-or-uncomment-lines)

(map! "M-g g" #'avy-goto-line
      "M-g M-g" #'avy-goto-line
      "s-/" #'avy-goto-char-timer)

(map! "M-g o" #'counsel-outline)

(map! "s-o" #'counsel-find-file)

(setq default-directory "~/")
