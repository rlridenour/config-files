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

;;;; Fonts
(defvar rr-fixed-pitch-font "SF Mono"
  "Font used for fixed-pitch faces.")

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Use-package for installed features
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

(use-package general :demand t
  :config
  (general-auto-unbind-keys))
(elpaca-wait)

(use-package org-auto-tangle
:hook (org-mode . org-auto-tangle-mode))

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

(use-feature simple
  :hook
  (;; Show points line number
   (after-init . line-number-mode)
   ;; Show points column number
   (after-init . column-number-mode)
   ;; Long lines will wrap on buffer edge
   (text-mode . visual-line-mode)))


;;;; = hl-line - highlight line at point
(use-feature hl-line
  :defer 1
  :custom
  (global-hl-line-mode t))

;;;; = frame - configure the frame with some padding, etc
;; Make a clean and minimalist frame
(use-feature frame
  :config
  (setq-default default-frame-alist
                (append (list
		;; NOTE: substitute whatever font you prefer
                ;;'(font . "Mono:style=medium:size=20") 
                '(internal-border-width . 10)
                '(left-fringe    . 5)
                '(right-fringe   . 0)
                '(tool-bar-lines . 0)
                ;;'(menu-bar-lines . 0)
                '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))

;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;;;; = all-the-icons
(use-package all-the-icons :demand t)

;;; Doom Modeline

(use-package doom-modeline
  :demand t
    :init (doom-modeline-mode 1))

;;;; = vertico - minibuffer completion UI
;; Modern version of completion UI.
;; Alternative is fido-vertical-mode
(use-package vertico
  :demand t
  :config
  (vertico-mode))

;;;; = orderless - completion-style
;; An additional completion style that completes with character entered
;; in any order.
(use-package orderless
  :demand t
  :after vertico
  :init
  ;; first search orderless, then basic
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
	'((file (styles partial-completion)))))

;;;; = consult - search for things
;; Auto complete and select from list
(use-package consult
  :demand t
  :commands (consult-line
             consult-line-multi
             consult-buffer
             consult-project-buffer
             consult-find
             consult-apropos
             consult-yank-pop
             consult-goto-line
             consult-outline
             consult-org-agenda
             consult-org-heading
             consult-flymake)
  :bind (;; C-x bindings (ctl-x-map)
	 ("C-x b"  . consult-buffer)
	 :map project-prefix-map
         ("b"      . consult-project-buffer)
         ("m"      .  consult-bookmark)))

;;;; = corfu - completion popup
;; Corfu enhances the default completion in region function with a
;; completion overlay.  The current candidates are shown in a popup
;; below or above the point.  Corfu can be considered the minimalistic
;; completion-in-region counterpart of Vertico.
(use-package corfu
  :demand t
  :elpaca (corfu :host github
		 :repo "minad/corfu"
		 :files (:defaults "extensions/*"))
  :defer 5
  :custom
  (corfu-separator ?\s)
  ;(corfu-preview-current nil)
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first t)
  (corfu-auto-prefix 2)
  ;(corfu-cycle t)
  :bind (:map corfu-map
	      ("SPC" . corfu-insert-separator)
	      ("C-n" . corfu-next)
	      ("C-p" . corfu-previous))
  :init
  ;; Enable corfu selectively
  :hook (prog-mode . corfu-mode)
  :config
  (defun corfu-auto-switch ()
    "Switch corfu auto"
    (interactive)
    (let ((old-state corfu-auto))
      (setq corfu-auto (not corfu-auto))
      (message "corfu-auto turn from %s to %s" old-state corfu-auto))))

;;;; = corfu-terminal - popup in TTY
;; Corfu uses childframes, which won't work on terminals
(use-package corfu-terminal
  :demand t
  :unless (display-graphic-p)
  :init
  (corfu-terminal-mode))

;;;; = kind-icon - colorful icons for completions
;; pretty icons in the corfu popup
(use-package kind-icon
  :demand t
  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; = cape - Completion at Point Extension
;; More help at point
;; TODO Dictionary doesn't work
(use-package cape
  :demand t
  :ensure t
  :defer t
  :init
  ;; Complete file names
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Complete words from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Complete ELisp symbol
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; Complete programming languag keyowrd
  (add-to-list 'completion-at-point-functions #'cape-keyword))


;; Enable indentation+completion using TAB key.
;; Completion is often bound to M-TAB.
(setq tab-always-indent 'complete)

;;;; = icomplete - vertical completion buffer
;; Minibuffer completion UI build-in Emacs
(use-feature icomplete
  :disabled
  :defer 1
  :custom
  ;; Automatically delete superfluous parts of file names
  (icomplete-tidy-shadowed-file-names t)
  ;; Show completions when first prompting for input
  (icomplete-show-matches-on-no-input t)
  ;; Pending completions over which to apply compute-delay
  (icomplete-delay-completions-threshold 50)
  ;; Maximum height completion buffer
  (completions-max-height 10)
  ;; Ignore case for buffer names during completion
  (read-buffer-completion-ignore-case t)
  ;; Ignore case for file names during completion
  (read-file-name-completion-ignore-case t)
  :config
  ;; vertical completion minibuffer with fuzzy finding
  (fido-vertical-mode)
  ;; Usefull mapping
  :bind (:map icomplete-fido-mode-map
	      ("RET" . icomplete-fido-ret)
	      ("TAB" . icomplete-force-complete)))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(global-set-key (kbd "S-<f7>") (lambda ()
                                (interactive)
                                (let ((current-prefix-arg '(4)))
                                  (call-interactively #'jinx-correct))))

;;;; = org-mode - the one and only writing environment (and more)

(use-package org
  :elpaca nil
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
        '(("DONE" . "green4") ("TODO" . org-warning)))
  (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")))

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; Don't export headlines with :ignore: tag, but do export content.
;;(require 'ox-extra)
;;(ox-extras-activate '(ignore-headlines))

;; Org-tempo is need for structure templates like "<s".

(require 'org-tempo)

;; I need to keep whitespace at the end of lines for my Beamer slides.

;; (add-hook 'text-mode-hook 'doom-disable-delete-trailing-whitespace-h)

(use-package orgonomic
  :defer t
  :elpaca (orgonomic :host github :repo "aaronjensen/emacs-orgonomic")
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

;;; Org-Footnote Assistant (https://github.com/lazzalazza/org-footnote-assistant)



(use-package org-footnote-assistant
  :elpaca (org-footnote-assistant :type git :host github :repo "lazzalazza/org-footnote-assistant")
  :commands (org-footnote-assistant)
  :after (org)
  :config
  (org-footnote-assistant-mode 1))


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

(defun canvas-notes ()
  "Copy HTML slide notes for Canvas"
  (interactive)
  (shell-command "canvas-notes")
  (find-file "canvas.org")
  (canvas-copy)
  (kill-buffer)
  (delete-file "canvas-data.org"))


(defun present ()
  (interactive)
  (async-shell-command "present"))

(defun canvas-copy ()
  "Copy html for canvas pages"
  (interactive)
  (org-html-export-to-html)
  (shell-command "canvas")
  )

(defun  create-args ()
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (exchange-point-and-mark)
  (yas-expand-snippet (yas-lookup-snippet "arg-wrap-tex"))
  (previous-line)
  ;; (previous-line)
  (org-beginning-of-line)
  (forward-word)
  (forward-char)
  (forward-char)
  (insert "\\underline{")
  (org-end-of-line)
  (insert "}")
  (next-line)
  (org-beginning-of-line)
  (forward-word)
  (insert "[\\phantom{\\(\\therefore\\)}]")
  (next-line)
  (next-line)
  (org-return)
  (org-return)
  (org-yank)
  (exchange-point-and-mark)
  (yas-expand-snippet (yas-lookup-snippet "arg-wrap-html"))
  )


(defun  create-tex-arg ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "arg-wrap-tex"))
  (previous-line)
  (previous-line)
  (forward-word)
  (forward-char)
  (forward-char)
  (insert "\\underline{")
  (org-end-of-line)
  (insert "}")
  (next-line)
  (org-beginning-of-line)
  (forward-word)
  (insert "[\\phantom{\\(\\therefore\\)}]")
  (next-line)
  (next-line)
  (org-return)
  (org-return)
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

;; Org-capture
(setq org-capture-templates
      '(
	("t" "Todo" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org" "Inbox")
	 "** TODO %?\n  %i\n  %a")
	("b" "Bookmark" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org" "Bookmarks")
	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	)
      )

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))


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
  :defer t
  :bind (("C-c C-b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (org-cite-global-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (citar-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (org-cite-csl-styles-dir "/usr/local/texlive/2023/texmf-dist/tex/latex/citation-style-language/styles"))

;; Use ebib for bibtex file management


(use-package ebib
  :defer t
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  ;;(evil-set-initial-state 'ebib-index-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-entry-mode 'emacs)
  ;;(evil-set-initial-state 'ebib-log-mode 'emacs)
  :custom
  (ebib-preload-bib-files '("~/Dropbox/bibtex/rlr.bib")))



;;; Markdown

(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-asymmetric-header t))

;; Convert markdown files to org format.
(fset 'convert-markdown-to-org
      [?\M-< ?\M-% ?* return ?- return ?! ?\M-< ?\C-\M-% ?# ?* backspace backspace ?  ?# ?* ?$ return return ?! ?\M-< ?\M-% ?# return ?* return ?!])

(fset 'copy-beamer-note
   (kmacro-lambda-form [?\C-r ?: ?E ?N ?D return down ?\C-  ?\C-s ?* ?* ?  ?N ?o ?t ?e ?s return up ?\M-w ?\C-s ?: ?E ?N ?D return down return ?\s-v return] 0 "%d"))

;;; LaTeX

(use-package tex-site
  :elpaca auctex
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

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))

  (setq TeX-view-program-list
	'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; Start Emacs server

;; (server-start)

;; Auto-raise Emacs on activation (from Skim, usually)

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' "))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)





;; Functions for Arara

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

;; Run once

;; (defun rlr/tex-mkt ()
;;   "Compile with arara."
;;   (interactive)
;;   (async-shell-command-no-window (concat "mkt " (shell-quote-argument(buffer-file-name)))))

(defun rlr/tex-mkt ()
  "Compile with arara."
  (interactive)
(save-buffer)
  (shell-command (concat "mkt " (shell-quote-argument(buffer-file-name))))
(TeX-view))



;; Run continuously

(defun rlr/tex-mktc ()
  "Compile continuously with arara."
  (interactive)
  (async-shell-command-no-window (concat "mktc " (shell-quote-argument(buffer-file-name))))
)

;;   (TeX-view))


(defun latex-word-count ()
  (interactive)
  (let* ((this-file (buffer-file-name))
         (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-brief" this-file)))))
    (string-match "\n$" word-count)
    (message (replace-match "" nil nil word-count))))

(use-package latex-change-env
  :after latex
  :bind 
  (:map LaTeX-mode-map ("C-c r" . latex-change-env)))

(use-package math-delimiters
  :elpaca (math-delimiters :type git :host github :repo "oantolin/math-delimiters")
  :after (:any org latex)
  :commands (math-delimiters-no-dollars math-delimiters-mode)
  :hook ((LaTeX-mode . math-delimiters-mode)
           (org-mode . math-delimiters-mode))
  :ensure nil
  :config (progn
            (setq math-delimiters-compressed-display-math nil)


            (define-minor-mode math-delimiters-mode
              "Math Delimeters"
              :init-value nil
              :lighter " MD"
              :keymap (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "$")  #'math-delimiters-insert)
                        map))))

(use-package org-bulletproof
  :defer t
  :elpaca (org-bulletproof :type git :host github :repo "pondersson/org-bulletproof")
  :config
  (setq org-bulletproof-default-ordered-bullet "1.")
  (global-org-bulletproof-mode +1))

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



;;;; = xeft - search notes with the xapian syntax
;; Search large volume of data (notes) with search engine syntax
;; +word -word AND NOT etc
;; <tab>   to preview
;; <enter> to open the file in the same buffer
;(use-package (xeft :host github :repo "casouri/xeft")
(use-package xeft
  :commands (xeft)
  :config
  (custom-set-faces '(xeft-excerpt-title ((t (:weight bold)))))
  (custom-set-faces '(xeft-excerpt-body ((t (:height 150)))))
  :custom
  ;; Default extension for files created with xeft
  (xeft-default-extension "org")
  ;; Where is my search source
  (xeft-directory rr-notes-dir)
  ;; Only parse the root directory
  (xeft-recursive nil))

;;;; = avy - jumping to visual text using char based navigation
;; In addition to the fast way of jumping around the visual frame
;; it comes with many useful functions, such as killing lines, regions, etc
(use-package avy
  :demand t
  :defer 2
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package ace-window
  :demand t
  :defer 2)

(use-feature outline
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
         ("TAB"       . outline-cycle)
         ("<backtab>" . outline-cycle-buffer)
         ("M-j"       . outline-move-subtree-down)
         ("M-k"       . outline-move-subtree-up)
         ("M-h"       . outline-promote)
         ("M-l"       . outline-demote))
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;;\\(;* \\)")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;; " . 1)
                            (";;;; " . 2)
                            (";;;;; " . 3)
                            (";;;;;; " . 4)
                            (";;;;;;; " . 5))))))

;;;; = project - project management
(use-feature project
  :custom
  ;; Place project files in the .cache
  (project-list-file (expand-file-name "projects" rr-cache-dir)))

;;;; = flymake - code fault finder
;; Error and warning code checking
(use-feature flymake
  :hook (flymake-mode . rr-flymake-toggle-diagnostics-buffer)
  :config
  
  (defun rr-flymake-toggle-diagnostics-buffer ()
    "Toggle the diagnostics buffer when entering/exiting `flymake-mode'."
    (let* ((root (vc-root-dir))
           (command (if root
                        #'flymake-show-project-diagnostics
                      #'flymake-show-buffer-diagnostics))
           (window (get-buffer-window
                    (if root
                        (flymake--project-diagnostics-buffer root)
                      (flymake--diagnostics-buffer-name)))))
      (if flymake-mode
          (funcall command)
        (when (window-live-p window)
          (with-selected-window window
            (kill-buffer-and-window)))))))

;;;; = paredit - structured editing on S-expresion data
;; Used as a better alternative to the build-in elec-pair
(use-package paredit
  :hook
  ((emacs-lisp-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (scheme-mode . paredit-mode)))

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

;;;; = dired - file management

;;;; = vundo - visual undo function
;; Call M-x vundo to visually undo
(use-package vundo
  :defer 2
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

;; Yasnippet
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/snippets"))
  :config
  (yas-global-mode 1))

;; Auto-activating snippets 
(use-package aas
  :defer 2
  :hook (LaTeX-mode . aas-activate-for-major-mode)
:hook (org-mode . aas-activate-for-major-mode)
:config
(aas-set-snippets 'text-mode
;; expand unconditionally
";o-" "ō"
";i-" "ī"
";a-" "ā"
";u-" "ū"
";e-" "ē")
(aas-set-snippets 'org-mode
"bp" "#+ATTR_BEAMER: :overlay +-"
"haarg" "#+ATTR_HTML: :class arg"
)
(aas-set-snippets 'latex-mode
;; set condition!
:cond #'texmathp ; expand only while in math
"." "\\land "
">" "\\lif "
"==" "\\liff "
"v" "\\lor "
"~" "\\lnot "
"#" "\\exists "
"@" "\\forall "
))

;; disable snippets by redefining them with a nil expansion


(use-package laas
  :defer 2
  :hook (TeX-mode . laas-mode))



(use-package yankpad
  :defer 2
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :config
  (bind-key "<f6>" 'yankpad-insert))

(use-package crux :demand t)


(use-package reveal-in-osx-finder)

(use-package deadgrep)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
