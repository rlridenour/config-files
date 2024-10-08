;;; init.el --- Personal Emacs configuration file -*- lexical-binding: t; -*-
;; NOTE: init.el is generated from README.org.  Please edit that file instead

(defconst rr-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Install org from upstream repo before using org-babel, or we would
  ;;; get whatever old version is distributed with Emacs.
(straight-use-package '(org
			:type git
			:repo "https://code.orgmode.org/bzg/org-mode.git"
			:local-repo "org"
			:depth full
			:pre-build (straight-recipes-org-elpa--build)
			:build (:not autoloads)
			:files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package use-package-ensure-system-package)

(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package general :demand t
  :config
  (general-auto-unbind-keys))

(use-package dash)
(use-package s)
(use-package f)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(setq ns-right-option-modifier 'hyper)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(setq insert-directory-program "gls")

(setq message-kill-buffer-on-exit t)

(setf use-short-answers t)

(setq ns-function-modifier 'hyper)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq world-clock-list
      '(
	("America/Chicago" "Oklahoma City")
	("America/Los_Angeles" "Seattle")
	("Pacific/Honolulu" "Honolulu")
	("America/New_York" "New York")
	("Etc/UTC" "UTC")))

(setq world-clock-time-format "%a, %d %b %R %Z")

(defun my-calendar ()
  (interactive)
  (calendar)
  )

(setq help-window-select t)
(setq Man-notify-method 'aggressive)

(use-package helpful)

(setq project-vc-ignores '("*.aux" "*.bbl" "*.bcf" "*.blg" "*.fdb_latexmk" "*.fls" "*.log" "*.out" "*.run.xml" "*.run.xml" "*.synctex.gz" "auto/" "*.pdf"))
(setq project-vc-extra-root-markers '(".proj"))

(use-package which-key
  :straight (emacs-which-key :host github :repo "wesnel/emacs-which-key" :branch "wesnel/add-devil-support")
  :config
  (which-key-mode))

(line-number-mode)
(column-number-mode)
(global-visual-line-mode 1)
(global-hl-line-mode)
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

(use-package all-the-icons)

(when (memq system-type '(darwin))
  (set-fontset-font t nil "SF Pro Display" nil 'append))

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160 :weight 'medium)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0 :weight 'medium)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0 :weight 'medium)

(setq-default line-spacing 0.25)

(use-package modus-themes
  :straight (modus-themes :type git :flavor melpa :host sourcehut :repo "protesilaos/modus-themes")
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
	modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t))

(general-define-key
 "<f9>" #'modus-themes-toggle)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq display-time-day-and-date t)
  )

(set-face-attribute 'mode-line nil
		    :foreground "black" :background "wheat3" :box '(:line-width 1 :color "black"))

(setq display-time-24hr-format t)
(display-time-mode)

(setq ring-bell-function 'ignore)

(electric-pair-mode 1)

(show-paren-mode)
(setq show-paren-delay 0)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
  )

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-week-agenda nil)
  (setq dashboard-startup-banner "/Users/rlridenour/.config/doom/logo-emacs.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons nil)
  (setq dashboard-set-navigator nil)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((agenda . 5)
			  (recents  . 5)
			  (bookmarks . 10)
			  (projects . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
				    dashboard-insert-newline
				    dashboard-insert-banner-title
				    dashboard-insert-newline
				    dashboard-insert-navigator
				    dashboard-insert-newline
				    dashboard-insert-init-info
				    dashboard-insert-items
				    dashboard-insert-newline)))

(defun dashboard-insert-agenda (&rest _)
  "Insert a copy of org-agenda buffer."
  (insert (save-window-excursion
	    (org-agenda nil "d")
	    (prog1 (buffer-string)
	      (kill-buffer)))))

(defun goto-dashboard ()
  "this sends you to the dashboard buffer"
  (interactive)
  (let ((goto-dashboard-buffer (get-buffer "*dashboard*")))
    (switch-to-buffer goto-dashboard-buffer))
  (dashboard-refresh-buffer))

(general-define-key
 "s-d" #'goto-dashboard)

(use-package rainbow-mode)

(general-define-key
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease)

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Where to save to backup file - in the backup dir
(setq backup-directory-alist (list (cons "."  rr-backup-dir)))
;; Always backup by copying
(setq backup-by-copying t)
;; Delete old backup files
(setq delete-old-versions t)
;; Keep 5 backup files
(setq kept-new-versions 5)
;; Make numeric backup versions
(setq version-control t)
;; Do not automatically save
(setq auto-save-default nil)

(use-package recentf
  :init
  (setq
   recentf-save-file "~/.cache/emacs/recentf"
   recentf-max-saved-items 10000
   recentf-max-menu-items 5000
   )
  (recentf-mode 1)
  (add-to-list 'recentf-exclude "~/.config/emacs/bookmarks")
  (run-at-time nil (* 5 60) 'recentf-save-list)
  )

;;;;; = saveplace - last position in file
;; Save point position in files between sessions.

;; Where to save the saveplaces file - in the .cache
(setq save-place-file (expand-file-name "saveplaces" rr-cache-dir))
(save-place-mode)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

(require 'uniquify)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      dired-auto-revert-buffer t
      auto-revert-verbose nil)

(setq ibuffer-expert t)

(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "home")))

;;;;; = savehist - last commands used
;; Persist emacs minibuffer history
;; Where to save the savehsit file - in the .cache
(setq savehist-file (expand-file-name "savehist" rr-cache-dir))
(savehist-mode)

(setq large-file-warning-threshold nil)

(add-hook 'before-save-hook 'time-stamp)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))
(add-hook 'find-file-not-found-functions #'make-parent-directory)

(defun nuke-all-buffers ()
  "Kill all the open buffers except the current one.
      Leave *scratch*, *dashboard* and *Messages* alone too."
  (interactive)
  (mapc
   (lambda (buffer)
     (unless (or
	      (string= (buffer-name buffer) "*scratch*")
	      (string= (buffer-name buffer) "*Org Agenda*")
	      (string= (buffer-name buffer) "*Messages*"))
       (kill-buffer buffer)))
   (buffer-list))
  (delete-other-windows)
  )

(use-package super-save
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
	  (progn (next-buffer)
		 (setq i (1+ i)))
	(progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
	  (progn (previous-buffer)
		 (setq i (1+ i)))
	(progn (setq i 100))))))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
	nil
      t
      )))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(general-define-key
 "s-]" #'xah-next-user-buffer
 "s-[" #'xah-previous-user-buffer
 "s-}" #'xah-next-emacs-buffer
 "s-{" #'xah-previous-emacs-buffer
 "C-<tab>" #'xah-next-user-buffer
 )

(defvar *my-previous-buffer* t
  "can we switch?")

(defun my-previous-buffer ()
  (interactive)
  (message "custom prev: *my-previous-buffer*=%s" *my-previous-buffer*)
  (when *my-previous-buffer*
    (previous-buffer)
    (setq *my-previous-buffer* nil)
    (run-at-time "1 sec" nil (lambda ()
			       (setq *my-previous-buffer* t)))))

(defvar *my-next-buffer* t
  "can we switch?")

(defun my-next-buffer ()
  (interactive)
  (message "custom prev: *my-next-buffer*=%s" *my-next-buffer*)
  (when *my-next-buffer*
    (next-buffer)
    (setq *my-next-buffer* nil)
    (run-at-time "1 sec" nil (lambda ()
			       (setq *my-next-buffer* t)))))

(global-set-key [triple-wheel-right] 'my-previous-buffer)
(global-set-key [triple-wheel-left] 'my-next-buffer)

(setq initial-scratch-message nil
      initial-major-mode 'org-mode)

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

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(load "~/Dropbox/emacs/my-emacs-abbrev")

(require 'bookmark)
(bookmark-bmenu-list)
(setq bookmark-save-flag 1)

(general-define-key
 "C-x c" #'save-buffers-kill-emacs
 "C-x C-b" #'ibuffer
 "C-`" #'iterm-goto-filedir-or-home
 "s-o" #'find-file
 "s-k" #'kill-this-buffer
 "M-s-k" #'kill-buffer-and-window
 "s-K" #'nuke-all-buffers
 "s-r" #'consult-buffer
 "M-s-r" #'consult-buffer-other-window
 "C-S-a" #'embark-act)

(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun rlr/find-file-right ()
  "Split window vertically and select recent file."
  (interactive)
  (split-window-right-focus)
  (consult-buffer))

(defun rlr/find-file-below ()
  "Split window horizontally and select recent file."
  (interactive)
  (split-window-below-focus)
  (consult-buffer))

(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
	 (w1 (car windows))
	 (w2 (nth 1 windows))
	 (w1b (window-buffer w1))
	 (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun toggle-frame-maximized-undecorated () (interactive) (let* ((frame (selected-frame)) (on? (and (frame-parameter frame 'undecorated) (eq (frame-parameter frame 'fullscreen) 'maximized))) (geom (frame-monitor-attribute 'geometry)) (x (nth 0 geom)) (y (nth 1 geom)) (display-height (nth 3 geom)) (display-width (nth 2 geom)) (cut (if on? (if ns-auto-hide-menu-bar 26 50) (if ns-auto-hide-menu-bar 4 26)))) (set-frame-position frame x y) (set-frame-parameter frame 'fullscreen-restore 'maximized) (set-frame-parameter nil 'fullscreen 'maximized) (set-frame-parameter frame 'undecorated (not on?)) (set-frame-height frame (- display-height cut) nil t) (set-frame-width frame (- display-width 20) nil t) (set-frame-position frame x y)))

(general-define-key
 ;; "C-1" #'delete-other-windows
 ;; "C-2" #'split-window-below-focus
 ;; "C-3" #'split-window-right-focus
 "s-6" #'toggle-window-split
 "S-C-<left>" #'shrink-window-horizontally
 "S-C-<right>" #'enlarge-window-horizontally
 "S-C-<down>" #'shrink-window
 "S-C-<up>" #'enlarge-window
 "C-x w" #'delete-frame
 "M-o" #'crux-other-window-or-switch-buffer)

(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-indexed
				vertico-flat
				vertico-grid
				vertico-mouse
				vertico-quick
				vertico-buffer
				vertico-repeat
				vertico-reverse
				vertico-directory
				vertico-multiform
				vertico-unobtrusive
				))
  :general
  (:keymaps 'vertico-map
	    "<tab>" #'vertico-insert    ; Choose selected candidate
	    "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
	    ;; NOTE 2022-02-05: Cycle through candidate groups
	    "C-M-n" #'vertico-next-group
	    "C-M-p" #'vertico-previous-group)
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode)
  )

(vertico-multiform-mode)

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
      ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
      ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
      ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
      ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
      ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
	(which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
	   "Become"
	 (format "Act on %s '%s'%s"
		 (plist-get (car targets) :type)
		 (embark--truncate-target (plist-get (car targets) :target))
		 (if (cdr targets) "…" "")))
       (if prefix
	   (pcase (lookup-key keymap prefix 'accept-default)
	     ((and (pred keymapp) km) km)
	     (_ (key-binding prefix 'accept-default)))
	 keymap)
       nil nil t (lambda (binding)
		   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
      embark-highlight-indicator
      embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
	 (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
	    :around #'embark-hide-which-key-indicator)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :general (:prefix "M-p"
		    "p" 'completion-at-point ;; capf
		    "d" 'cape-dabbrev        ;; or dabbrev-completion
		    "a" 'cape-abbrev
		    "i" 'cape-ispell
		    "w" 'cape-dict
		    "\\" 'cape-tex
		    "_" 'cape-tex
		    "^" 'cape-tex)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  )

(delete-selection-mode 1)

(setq default-fill-column 100)

(defun fill-sentences-in-paragraph ()
  "Put a newline at the end of each sentence in the current paragraph."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (call-interactively 'fill-sentences-in-region)))

(defun fill-sentences-in-region (start end)
  "Put a newline at the end of each sentence in the region maked by (start end)."
  (interactive "*r")
  (call-interactively 'unfill-region)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (forward-sentence)
      (if (looking-at-p " ")

	  (defvar repetition-counter 0
	    "How often cycle-on-repetition was called in a row using the same command.")

	(defun cycle-on-repetition (list-of-expressions)
	  "Return the first element from the list on the first call,
     the second expression on the second consecutive call etc"
	  (interactive)
	  (if (equal this-command last-command)
	      (setq repetition-counter (+ repetition-counter 1)) ;; then
	    (setq repetition-counter 0) ;; else
	    )
	  (nth
	   (mod repetition-counter (length list-of-expressions))
	   list-of-expressions) ;; implicit return of the last evaluated value
	  )

	(defun reformat-paragraph ()
	  "Cycles the paragraph between three states: filled/unfilled/fill-sentences."
	  (interactive)
	  (funcall (cycle-on-repetition '(fill-paragraph fill-sentences-in-paragraph unfill-paragraph)))
	  )
	(newline-and-indent)))))

(setq sentence-end-double-space nil)

(use-package ts)

(defun insert-date-string ()
  "Insert current date yyyymmdd."
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%B %e, %Y")))

(defun rlr-count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
	(e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(use-package devil
  :config
  (global-devil-mode))

(use-package hungry-delete
  :defer t
  :config
  (global-hungry-delete-mode))

(use-package evil-nerd-commenter
  :general
  ("M-;" #'evilnc-comment-or-uncomment-lines))

(use-package shrink-whitespace
  :defer t)

(use-package unfill
  :defer t)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package titlecase
  :defer t
  :config
  (setq titlecase-style "chicago"))

(use-package jinx
  :init
  (setenv "PKG_CONFIG_PATH" (concat "/opt/homebrew/opt/glib/lib/pkgconfig/:" (getenv "PKG_CONFIG_PATH")))
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(defun jinx-correct-all ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'jinx-correct)))

(general-define-key
 "<f7>" #'jinx-correct
 "S-<f7>" #'jinx-correct-all)

(add-to-list 'vertico-multiform-categories
	     '(jinx grid (vertico-grid-annotate . 20)))

(setq case-replace nil)

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

(setq locate-command "mdfind")

(use-package visual-regexp
  :defer)

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun rlr/consult-rg ()
  "Function for `consult-ripgrep' with the `universal-argument'."
  (interactive)
  (consult-ripgrep (list 4)))

(defun rlr/consult-fd ()
  "Function for `consult-find' with the `universal-argument'."
  (interactive)
  (consult-find (list 4)))

(use-package deadgrep)

(general-define-key
 "s-l" #'hydra-locate/body
 "s-f" #'consult-line
 "<f5>" #'deadgrep)
;; "C-s" #'consult-isearch
;; "C-r" #'consult-isearch-reverse

(use-package iedit)

(use-package multiple-cursors
  :ensure t
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(general-define-key
 "<s-up>" #'beginning-of-buffer
 "<s-down>" #'end-of-buffer
 "<s-right>" #'end-of-visual-line
 "<s-left>" #'beginning-of-visual-line
 "<M-down>" #'forward-paragraph
 "<M-up>" #'backward-paragraph
 "M-u" #'upcase-dwim
 "M-l" #'downcase-dwim
 "M-c" #'capitalize-dwim
 "RET" #'newline-and-indent
 "M-/" #'hippie-expand
 "<s-backspace>" #'kill-whole-line
 "s-j" #'crux-top-join-line
 "<S-return>" #'crux-smart-open-line
 "<C-S-return>" #'crux-smart-open-line-above
 "<C-d d>" #'insert-standard-date

 "M-y" #'consult-yank-pop

 "M-q" #'reformat-paragraph
 "M-#" #'dictionary-lookup-definition
 "M-=" #'shrink-whitespace
 "s-l" #'hydra-locate/body
 "s-f" #'consult-line
 "<f5>" #'deadgrep)

(use-package org
  ;; :straight (:type built-in)
  :init
  ;; (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
  (setq org-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/")
  :config
  (setq org-list-allow-alphabetical t)
  (setq org-highlight-latex-and-related '(latex script entities))
  ;; (setq org-startup-indented t)
  (setq org-adapt-indentation nil)
  ;; (setq org-hide-leading-stars nil)
  (setq org-hide-emphasis-markers nil)
  (setq org-support-shift-select t)
  ;; (setq org-footnote-section nil)
  (setq org-html-validation-link nil)
  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-todo-keyword-faces
	'(("DONE" . "green4") ("TODO" . org-warning)))
  (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"))
  )

(use-package mixed-pitch
  ;; :hook
  ;; If you want it in all text modes:
  ;; (text-mode . mixed-pitch-mode)
  )

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
      :straight (orgonomic :host github :repo "aaronjensen/emacs-orgonomic")
      :hook (org-mode . orgonomic-mode))

    ;; Some export settings


    ;; Add arara export

    (require 'ox-arara)

    (require 'ox-beamer)

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
      :straight (org-footnote-assistant :type git :host github :repo "lazzalazza/org-footnote-assistant")
      :commands (org-footnote-assistant)
      :after (org)
      :config
      (org-footnote-assistant-mode 1))


(use-package rlr-teaching
:straight (rlr-teaching :host github :repo "rlridenour/rlr-teaching"))



  ;;   (defun present ()
  ;;     (interactive)
  ;;     (async-shell-
       ;; command "present"))



    ;; (setq org-latex-pdf-process '("arara %f"))
    (setq org-latex-pdf-process '("mkpdf %f"))


    (defun rlr/org-mkpdf ()
      "Make PDF with pdf latexmk."
      (interactive)
      (save-buffer)
      (org-latex-export-to-latex)
      (async-shell-command-no-window (concat "mkpdf " (shell-quote-argument(file-name-nondirectory (file-name-with-extension buffer-file-name "tex"))))))

    (defun rlr/org-open-pdf ()
      "Open PDF in background with default viewer."
      (interactive)
      (async-shell-command-no-window (concat "open -g " (shell-quote-argument(file-name-nondirectory (file-name-with-extension buffer-file-name "pdf"))))))

    (defun rlr/org-mklua ()
      "Make PDF with lua latexmk."
      (interactive)
      (save-buffer)
      (org-latex-export-to-latex)
      (async-shell-command-no-window (concat "mklua " (shell-quote-argument(file-name-nondirectory (file-name-with-extension buffer-file-name "tex"))))))


    (defun rlr/org-arara ()
      "Make PDF with Arara."
      (interactive)
      (save-buffer)
      (org-arara-export-to-latex)
      (async-shell-command-no-window (concat "mkarara " (shell-quote-argument(file-name-sans-extension (buffer-file-name)))".tex")))

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
    	("e" "Event" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/events.org" "Future")
    	 "** %? %T")
    	("b" "Bookmark" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org" "Bookmarks")
    	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
    	("c" "Quick note" entry (file+headline "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/quick-notes.org" "Notes")
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
    	org-agenda-window-setup "current-window"
    	org-agenda-include-diary nil
    	org-agenda-start-on-weekday nil)
      (setq org-agenda-time-grid
    	'((daily today require-timed remove-match)
    	  ()
    	  "......"
    	  ""))

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

(setq org-agenda-custom-commands
      '(("d" "Agenda for today" agenda ""
	 ((org-agenda-overriding-header "Today's agenda")
	  (org-agenda-span 'day)
	  ))))

(defun today-agenda ()
  "Display today's agenda"
  (interactive)
  (org-agenda nil "d")
  )

(today-agenda)

(setq appt-time-msg-list nil)    ;; clear existing appt list
;; (setq appt-message-warning-time '15)  ;; send first warning 15 minutes before appointment
(org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt) ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

(use-package org-bulletproof
  :defer t
  :straight (org-bulletproof :type git :host github :repo "pondersson/org-bulletproof")
  :config
  (setq org-bulletproof-default-ordered-bullet "1.")
  (global-org-bulletproof-mode +1))

(use-package gnuplot)

(defun my/org-toggle-emphasis (type)
  "Toggle org emphasis TYPE (a character) at point."
  (cl-labels ((in-emph (re)
		"See if in org emphasis given by RE."
		(and (org-in-regexp re 2)
		     (>= (point) (match-beginning 3))
		     (<= (point) (match-end 4))))
	      (de-emphasize ()
		"Remove most recently matched org emphasis markers."
		(save-excursion
		  (replace-match "" nil nil nil 3)
		  (delete-region (match-end 4) (1+ (match-end 4))))))
    (let* ((res (vector org-emph-re org-verbatim-re))
	   (idx (cl-case type (?/ 0) (?* 0) (?_ 0) (?+ 0) (?= 1) (?~ 1)))
	   (re (aref res idx))
	   (other-re (aref res (- 1 idx)))
	   (type-re (string-replace (if (= idx 1) "=~" "*/_+")
				    (char-to-string type) re))
	   add-bounds offset is-word)
      (save-match-data
	(if (region-active-p)
	    (if (in-emph type-re) (de-emphasize) (org-emphasize type))
	  (if (eq (char-before) type) (backward-char))
	  (if (in-emph type-re)       ;nothing marked, in emph text?
	      (de-emphasize)
	    (setq add-bounds          ; check other flavors
		  (if (or (in-emph re) (in-emph other-re))
		      (cons (match-beginning 4) (match-end 4))
		    (setq is-word t)
		    (bounds-of-thing-at-point 'symbol))))
	  (if add-bounds
	      (let ((off (- (point) (car add-bounds)))
		    (at-end (= (point) (cdr add-bounds))))
		(set-mark (car add-bounds))
		(goto-char (cdr add-bounds))
		(org-emphasize type)  ;deletes marked region!
		(unless is-word       ; delete extra spaces
		  (goto-char (car add-bounds))
		  (when (eq (char-after) ?\s) (delete-char 1))
		  (goto-char (+ 2 (cdr add-bounds)))
		  (when (eq (char-after) ?\s) (delete-char 1)))
		(goto-char (+ (car add-bounds) off
			      (cond ((= off 0) 0) (at-end 2) (t 1)))))
	    (if is-word (org-emphasize type))))))))

(use-package org-view-mode
  :straight (org-view-mode :type git :host github :repo "amno1/org-view-mode"))

(general-define-key
 :keymaps 'org-mode-map
 "s-i" (lambda () (interactive) (my/org-toggle-emphasis ?/))
 "s-b" (lambda () (interactive) (my/org-toggle-emphasis ?*))
 "s-e" (lambda () (interactive) (my/org-toggle-emphasis ?~))
 "s-=" (lambda () (interactive) (my/org-toggle-emphasis ?=))
 "s-_" (lambda () (interactive) (my/org-toggle-emphasis ?_))
 "s-+" (lambda () (interactive) (my/org-toggle-emphasis ?+)))

(use-package citar
  :defer t
  :bind (("C-c C-b" . citar-insert-citation)
	 :map minibuffer-local-map
	 ("M-b" . citar-insert-preset))
  :custom
  (org-cite-global-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (citar-bibliography '("~/Dropbox/bibtex/rlr.bib"))
  (org-cite-csl-styles-dir "/usr/local/texlive/2023/texmf-dist/tex/latex/citation-style-language/styles")
  (org-cite-export-processors
   '((md . (csl "chicago-author-date.csl"))   ; Footnote reliant
     (latex biblatex)                                   ; For humanities
     (odt . (csl "chicago-author-date.csl"))  ; Footnote reliant
     (t . (csl "chicago-author-date.csl")))))      ; Fallback


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

(defun yank-bibtex-from-doi ()
  "Create and yank bibtex entry from a DOI.
This command expects a DOI, of the form e.g.
10.1371/journal.pcbi.1004947 and will then grab the bibtex entry.
No error handling is performed e.g. if the DOI is invalid.
If you run this from a bibtex buffer, then run C-c C-q to reformat the entry
after it is inserted."
  (interactive)
  (let* ((doi
	  (read-from-minibuffer "doi: "))
	 (cmd
	  (concat
	   "curl -LH \"Accept: application/x-bibtex\" "
	   "https://doi.org/"
	   doi))
	 (bibtex
	  (shell-command-to-string
	   (concat cmd " 2>/dev/null"))))
    (insert-for-yank bibtex)))

(use-package org-cite-overlay
  :straight (org-cite-overlay :type git :host sourcehut :repo "swflint/org-cite-overlay"))

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

(use-package tex
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

(defun rlr/tex-mkpdf ()
  "Compile with pdf latexmk."
  (interactive)
  (save-buffer)
  (async-shell-command-no-window (concat "mkpdf " (shell-quote-argument(file-name-nondirectory buffer-file-name))))
  (TeX-view))

;; Run continuously

(defun rlr/tex-mktc ()
  "Compile continuously with pdf latexmk."
  (interactive)
  (async-shell-command-no-window (concat "mkpdfc " (shell-quote-argument(file-name-nondirectory buffer-file-name))))
  )

(defun rlr/tex-mklua ()
  "Compile with lua latexmk."
  (interactive)
  (save-buffer)
  (async-shell-command-no-window (concat "mklua " (shell-quote-argument(file-name-nondirectory buffer-file-name))))
  (TeX-view))

;; Run continuously

(defun rlr/tex-mkluac ()
  "Compile continuously with lua latexmk."
  (interactive)
  (async-shell-command-no-window (concat "mkluac " (shell-quote-argument(file-name-nondirectory buffer-file-name))))
  )


(defun rlr/tex-arara ()
  "Compile with arara."
  (interactive)
  (save-buffer)
  (async-shell-command-no-window (concat "mkarara " (shell-quote-argument(buffer-file-name))))
  (TeX-view))

;; Run continuously

(defun rlr/tex-arara-c ()
  "Compile continuously with arara."
  (interactive)
  (async-shell-command-no-window (concat "mkarara-c " (shell-quote-argument(buffer-file-name))))
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
  :straight (math-delimiters :type git :host github :repo "oantolin/math-delimiters")
  :after (:any org latex)
  :commands (math-delimiters-no-dollars math-delimiters-mode)
  :hook ((LaTeX-mode . math-delimiters-mode)
	 (org-mode . math-delimiters-mode))
  :config (progn
	    (setq math-delimiters-compressed-display-math nil)


	    (define-minor-mode math-delimiters-mode
	      "Math Delimeters"
	      :init-value nil
	      :lighter " MD"
	      :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "$")  #'math-delimiters-insert)
			map))))

(defun hugo-timestamp ()
  "Update existing date: timestamp on a Hugo post."
  (interactive)
  (save-excursion (
		   goto-char 1)
		  (re-search-forward "^#\\+date:")
		  (let ((beg (point)))
		    (end-of-line)
		    (delete-region beg (point)))
		  (insert (concat " " (format-time-string "%Y-%m-%dT%H:%M:%S")))))

(defvar hugo-directory "~/Sites/blog/" "Path to Hugo blog.")
(defvar hugo-posts-dir "content/posts/" "Relative path to posts directory.")
(defvar hugo-post-ext ".org"  "File extension of Hugo posts.")
(defvar hugo-post-template "#+TITLE: \%s\n#+draft: true\n#+tags[]: \n#+date: \n#+lastmod: \n#+mathjax: \n\n"
  "Default template for Hugo posts. %s will be replace by the post title.")

(defun hugo-make-slug (s) "Turn a string into a slug."
       (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun hugo-yaml-escape (s) "Escape a string for YAML."
       (if (or (string-match ":" s) (string-match "\"" s)) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

;; Create a new blog post.


(defun hugo-draft-post (title) "Create a new Hugo blog post."
       (interactive "sPost Title: ")
       (let ((draft-file (concat hugo-directory hugo-posts-dir
				 (format-time-string "%Y-%m-%d-")
				 (hugo-make-slug title)
				 hugo-post-ext)))
	 (if (file-exists-p draft-file)
	     (find-file draft-file)
	   (find-file draft-file)
	   (insert (format hugo-post-template (hugo-yaml-escape title)))
	   (hugo-timestamp))))

;; This sets the draft tag to false, updates the timestamp, and saves the buffer.


(defun hugo-publish-post ()
  "Set draft to false, update the timestamp, and save."
  (interactive)
  (save-excursion
    (goto-char 1)
    (re-search-forward "^#\\+draft:")
    (let ((beg (point)))
      (end-of-line)
      (delete-region beg (point)))
    (insert " false")
    (hugo-timestamp))
  (save-buffer))

(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(progn (setq ,orig-dir default-directory)
	    (cd ,DIR) ,@FORMS (cd ,orig-dir))))

;; Update the last modified date.


(defun hugo-update-lastmod ()
  "Update the `lastmod' value for a hugo org-mode buffer."
  (interactive)
  (save-excursion
    (goto-char 1)
    (re-search-forward "^#\\+lastmod:")
    (let ((beg (point)))
      (end-of-line)
      (delete-region beg (point)))
    (insert (concat " " (format-time-string "%Y-%m-%dT%H:%M:%S"))))
  (save-buffer))

;; Deploy the blog.


(defun hugo-deploy ()
  "Push changes upstream."
  (interactive)
  (with-dir hugo-directory
	    (shell-command "git add .")
	    (--> (current-time-string)
		 (concat "git commit -m \"" it "\"")
		 (shell-command it))
	    (magit-push-current-to-upstream nil)))

;; Update the last modified date of a post, save the buffer, and deploy.


(defun hugo-org-deploy ()
  "Push changes upstream."
  (interactive)
  (hugo-update-lastmod)
  (save-buffer)
  (with-dir hugo-directory
	    (shell-command "git add .")
	    (--> (current-time-string)
		 (concat "git commit -m \"" it "\"")
		 (shell-command it))
	    (magit-push-current-to-upstream nil)))

;; Insert a tag into a Hugo post. From [[https://whatacold.io/blog/2022-10-10-emacs-hugo-blogging/][Hugo Blogging in Emacs - whatacold's space]]


(defun hugo-select-tags ()
  "Select tags from the hugo org files in the current dir.

  Note that it only extracts tags from lines like the below:
  #+tags[]: Emacs Org-mode"
  (interactive)
  ;; Move to end of tag line.
  (save-excursion
    (goto-char 1)
    (re-search-forward "^#\\+tags")
    (end-of-line)

    (let ((files (directory-files-recursively default-directory "\\.org$")))
      (let ((source (with-temp-buffer
		      (while files
			(when (file-exists-p (car files))
			  (insert-file-contents (car files)))
			(pop files))
		      (buffer-string))))
	(save-match-data
	  (let ((pos 0)
		matches)
	    (while (string-match "^#\\+[Tt]ags\\[\\]: \\(.+?\\)$" source pos)
	      (push (match-string 1 source) matches)
	      (setq pos (match-end 0)))
	    (insert
	     (completing-read
	      "Insert a tag: "
	      (sort
	       (delete-dups
		(delete "" (split-string
			    (replace-regexp-in-string "[\"\']" " "
						      (replace-regexp-in-string
						       "[,()]" ""
						       (format "%s" matches)))
			    " ")))
	       (lambda (a b)
		 (string< (downcase a) (downcase b))))))))))
    (insert " ")
    )
  )

;; Add multiple tags to a Hugo post. I need to try to make it work with consult--read.


(defun w/hugo--collect-tags ()
  "Collect hugo tags from the org files in the current dir.

  Note that it only extracts tags from lines like the below:
  #+tags[]: Emacs Org-mode"
  (interactive)
  (let ((files (directory-files-recursively default-directory "\\.org$")))
    (let ((source (with-temp-buffer
		    (while files
		      (when (file-exists-p (car files))
			(insert-file-contents (car files)))
		      (pop files))
		    (buffer-string))))
      (save-match-data
	(let ((pos 0)
	      matches)
	  (while (string-match "^#\\+[Tt]ags\\[\\]: \\(.+?\\)$" source pos)
	    (push (match-string 1 source) matches)
	    (setq pos (match-end 0)))
	  (sort
	   (delete-dups
	    (delete "" (split-string
			(replace-regexp-in-string "[\"\']" " "
						  (replace-regexp-in-string
						   "[,()]" ""
						   (format "%s" matches)))
			" ")))
	   (lambda (a b)
	     (string< (downcase a) (downcase b)))))))))

(defun w/hugo-select-tags ()
  "Select tags for the current hugo post."
  (interactive)
  (ivy-read "Insert tags: "
	    (w/hugo--collect-tags)
	    :action
	    (lambda (tag)
	      (insert (if (char-equal (preceding-char) 32)
			  ""
			" ")
		      tag))))

;; Insert internal links using C-c C-l. From [[https://lucidmanager.org/productivity/create-websites-with-org-mode-and-hugo/][Create Websites with Emacs: Blogging with Org mode and Hugo]]


;; Follow Hugo links
(defun org-hugo-follow (link)
  "Follow Hugo link shortcodes"
  (org-link-open-as-file
   (string-trim "{{< ref test.org >}}" "{{< ref " ">}}")))

;; New link type for Org-Hugo internal links
(org-link-set-parameters
 "hugo"
 :complete (lambda ()
	     (concat "{{< ref "
		     (file-name-nondirectory
		      (read-file-name "File: "))
		     " >}}"))
 :follow #'org-hugo-follow)

(use-package writeroom-mode)

;; Denote
(use-package denote
  :config
  (setq denote-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes/denote/")
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-date-format nil)
  )

(require 'denote-journal-extras)

(use-package consult-notes
  :config
  (consult-notes-denote-mode))



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

(use-package avy
  :defer t
  :config
  (avy-setup-default)
  :general
  ("s-/" #'avy-goto-char-timer)
  ("C-c C-j" #'avy-resume))

(use-package ace-window
  :defer t)

(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
	(commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(fish "https://github.com/ram02z/tree-sitter-fish")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(latex "https://github.com/latex-lsp/tree-sitter-latex")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package fish-mode)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package lua-mode)

(use-package magit
  :defer t
  :config
  (global-auto-revert-mode)
  (setq magit-refresh-status-buffer nil
	magit-diff-highlight-indentation nil
	magit-diff-highlight-trailing nil
	magit-diff-paint-whitespace nil
	magit-diff-highlight-hunk-body nil
	magit-diff-refine-hunk nil
	magit-revision-insert-related-refs nil)
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

(use-package dired-x
  :straight (:type built-in)
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
	  (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^\\..+$"))
    (setq-default dired-omit-extensions '(".fdb_latexmk" ".aux" ".bbl" ".blg" ".fls" ".glo" ".idx" ".ilg" ".ind" ".ist" ".log" ".out" ".gz" ".DS_Store" ".xml" ".bcf" ".nav" ".snm" ".toc"))))

(setq dired-dwim-target t)

(use-package dired-preview
:config
(setq dired-preview-delay 0.7)
(setq dired-preview-max-size (expt 2 20))
(setq dired-preview-ignored-extensions-regexp
	(concat "\\."
		"\\(gz\\|"
		"zst\\|"
		"tar\\|"
		"xz\\|"
		"rar\\|"
		"zip\\|"
		"iso\\|"
		"epub"
		"\\)")))

(general-define-key
 :keymaps 'dired-mode-map
 "M-<RET>" #'crux-open-with
 "s-j" #'dired-goto-file)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package crux)

(use-package reveal-in-osx-finder)

;;;; = dired - file management

  ;;;; = vundo - visual undo function
;; Call M-x vundo to visually undo
(use-package vundo
  :defer
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

;; Yasnippet
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-global-mode 1))

(use-package yankpad
  :defer t
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :general
  ( "<f6>" #'yankpad-insert))

(use-package osx-dictionary)

(setq async-shell-command-buffer "new-buffer")

(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
	(list
	 (cons
	  "\\*Async Shell Command\\*.*"
	  (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
	    ;; string escaping madness for applescript
	    (replace-regexp-in-string "\\\\" "\\\\\\\\"
				      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )

(setq eshell-scroll-to-bottom-on-input "this")

(use-package eat
  :straight (eat :type git
		 :host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("terminfo/e" "terminfo/e/*")
			 ("terminfo/65" "terminfo/65/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package term-toggle
  :straight (term-toggle :host github :repo "amno1/emacs-term-toggle")
  :config
  (setq term-toggle-no-confirm-exit t)
  )

(defun term-toggle-eat ()
  "Toggle `term'."
  (interactive) (term-toggle 'eat))

(general-define-key
 "<f2>" #'term-toggle-eat
 "<S-f2>" #'term-toggle-eshell
 "C-`" #'iterm-goto-filedir-or-home)

(use-package ace-link
  :init
  (ace-link-setup-default)
  )

;; EWW

(defun rrnet ()
  (interactive)
  (eww-browse-url "randyridenour.net")
  )

(defun sep ()
  (interactive)
  (eww-browse-url "plato.stanford.edu")
  )


;; Org-mac-link

(use-package org-mac-link
  :defer)

(defun jao-eww-to-org (&optional dest)
  "Render the current eww buffer using org markup.
If DEST, a buffer, is provided, insert the markup there."
  (interactive)
  (unless (org-region-active-p)
    (let ((shr-width 80)) (eww-readable)))
  (let* ((start (if (org-region-active-p) (region-beginning) (point-min)))
	 (end (if (org-region-active-p) (region-end) (point-max)))
	 (buff (or dest (generate-new-buffer "*eww-to-org*")))
	 (link (eww-current-url))
	 (title (or (plist-get eww-data :title) "")))
    (with-current-buffer buff
      (insert "#+title: " title "\n#+link: " link "\n\n")
      (org-mode))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(let* ((p (point))
	       (props (text-properties-at p))
	       (k (seq-find (lambda (x) (plist-get props x))
			    '(shr-url image-url outline-level face)))
	       (prop (and k (list k (plist-get props k))))
	       (next (if prop
			 (next-single-property-change p (car prop) nil end)
		       (next-property-change p nil end)))
	       (txt (buffer-substring (point) next))
	       (txt (replace-regexp-in-string "\\*" "·" txt)))
	  (with-current-buffer buff
	    (insert
	     (pcase prop
	       ((and (or `(shr-url ,url) `(image-url ,url))
		     (guard (string-match-p "^http" url)))
		(let ((tt (replace-regexp-in-string "\n\\([^$]\\)" " \\1" txt)))
		  (org-link-make-string url tt)))
	       (`(outline-level ,n)
		(concat (make-string (- (* 2 n) 1) ?*) " " txt "\n"))
	       ('(face italic) (format "/%s/ " (string-trim txt)))
	       ('(face bold) (format "*%s* " (string-trim txt)))
	       (_ txt))))
	  (goto-char next))))
    (pop-to-buffer buff)
    (goto-char (point-min))))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1) (blink-cursor-mode -1)))

;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(require 'mu4e)
(setq
 mue4e-headers-skip-duplicates t
 mu4e-headers-leave-behavior 'apply
 mu4e-headers-date-format "%Y-%m-%d"
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed t
 mu4e-compose-dont-reply-to-self t
 mu4e-compose-reply-ignore-address '("no-?reply")
 mu4e-compose-signature-auto-include nil
 mu4e-date-format "%y-%m-%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"
 mu4e-get-mail-command "mbsync -a"
 mu4e-view-use-gnus t
 mu4e-html2text-command "w3m -T text/html"
 mu4e-update-interval (* 10 60) ;;this setting allows to re-sync and re-index mail by pressing U refresh mbsync  every 10 minutes

 mu43-use-fancy-chars t
 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"
 mu4e-reply-to-address "rlridenour@fastmail.com"
 user-mail-address "rlridenour@fastmail.com"
 user-full-name "Randy Ridenour")

(setq mu4e-maildir-shortcuts
      '( (:maildir "/inbox"     :key  ?i)
       (:maildir "/archive"   :key  ?a)
       (:maildir "/sent"      :key  ?s)
       ))

(fset 'my-move-to-trash "mTrash")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

(setq
 message-send-mail-function   'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.fastmail.com"
 smtpmail-smtp-server         "smtp.fastmail.com"
 smtpmail-smtp-service        587)

;; Show emails as plain text, if possible
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(use-package emacs-everywhere)

(use-package casual-calc
  :general
  (:keymaps 'calc-mode-map
	    "s-."  #'casual-calc-tmenu))

(use-package casual-info
  :general
  (:keymaps 'Info-mode-map
	    "s-." #'casual-info-tmenu))

(use-package casual-dired
  :general
  (:keymaps 'dired-mode-map
	    "s-." #'casual-dired-tmenu))

(use-package casual-avy
  :general
  ("M-g a"  #'casual-avy-tmenu))

(use-package casual-isearch
  :general
  (:keymaps 'isearch-mode-map
	    "<f8>" #'casual-isearch-tmenu))

;; (require 'ibuffer)
(use-package casual-ibuffer
  :general
  (:keymaps 'ibuffer-mode-map
	    "s-." #'casual-ibuffer-tmenu
	    "F" #'casual-ibuffer-filter-tmenu
	    "s" #'casual-ibuffer-sortby-tmenu))

(require 're-builder)
(setq reb-re-syntax 'string)
(use-package casual-re-builder
  :general
  (:keymaps 'reb-mode-map
	    "s-." #'casual-re-builder-tmenu))

;; (require 'bookmarks)
(use-package casual-bookmarks
  :straight (casual-bookmarks :host github :repo "kickingvegas/casual-bookmarks")
  :general
  (:keymaps 'bookmark-bmenu-mode-map
	    "s-." #'casual-bookmarks-tmenu))

(keymap-set bookmark-bmenu-mode-map "J" #'bookmark-jump)

;; (easy-menu-add-item global-map '(menu-bar)
;;                     casual-bookmarks-main-menu
;;                     "Tools")

(require 'hl-line)
(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

(use-package ready-player
  :config
  (ready-player-mode +1))

(use-package mastodon
  :config
  (mastodon-discover)
  (setq mastodon-instance-url "https://emacs.ch/"
	mastodon-active-user "randyridenour"))

(general-unbind
  "C-z"
  "s-p"
  "s-q"
  "s-w"
  "s-m"
  "s-n"
  "s-h")

(defun my/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(use-package major-mode-hydra
  :general
  ("s-m" #'major-mode-hydra))

(pretty-hydra-define hydra-toggle
  (:color teal :quit-key "q" :title "Toggle")
  (" "
   (("a" abbrev-mode "abbrev" :toggle t)
    ("d" toggle-debug-on-error "debug" (default value 'debug-on-error))
    ("e" meow-global-mode "meow" :toggle t)
    ("i" aggressive-indent-mode "indent" :toggle t)
    ("f" auto-fill-mode "fill" :toggle t)
    ("l" display-line-numbers-mode "linum" :toggle t)
    ("m" mixed-pitch-mode "mixed-pitch" :toggle t)
    ("p" electric-pair-mode "electric-pair" :toggle t)
    ("t" toggle-truncate-lines "truncate" :toggle t)
    ("s" whitespace-mode "whitespace" :toggle t))
   " "
   (("c" cdlatex-mode "cdlatex" :toggle t)
    ("w" writeroom-mode "writeroom" :toggle t)
    ("r" read-only-mode "read-only" :toggle t)
    ("v" view-mode "view" :toggle t)
    ("W" wc-mode "word-count" :toggle t)
    ("S" auto-save-visited-mode "auto-save" :toggle t)
    ("C" cua-selection-mode "rectangle" :toggle t))))

(pretty-hydra-define hydra-buffer
  (:color teal :quit-key "q" :title "Buffers and Files")
  ("Open"
   (("b" ibuffer "ibuffer")
    ("m" consult-bookmark "bookmark")
    ("w" consult-buffer-other-window "other window")
    ("f" consult-buffer-other-frame "other frame")
    ("d" crux-recentf-find-directory "recent directory")
    ("a" crux-open-with "open in default app"))
   "Actions"
   (("D" crux-delete-file-and-buffer "delete file")
    ("R" crux-rename-file-and-buffer "rename file")
    ("K" crux-kill-other-buffers "kill other buffers")
    ("N" nuke-all-buffers "Kill all buffers")
    ("c" crux-cleanup-buffer-or-region "fix indentation"))
   "Misc"
   (("t" crux-visit-term-buffer "ansi-term")
    ("T" iterm-goto-filedir-or-home "iTerm2")
    ("i" crux-find-user-init-file "init.el")
    ("s" crux-find-shell-init-file "fish config"))
   ))

(pretty-hydra-define hydra-locate
  (:color teal :quit-key "q" title: "Search")
  ("Buffer"
   (("c" pulsar-highlight-dwim "find cursor")
    ("l" consult-goto-line "goto-line")
    ("i" consult-imenu "imenu")
    ("m" consult-mark "mark")
    ("o" consult-outline "outline"))
   "Global"
   (("M" consult-global-mark "global-mark")
    ("n" consult-notes "notes")
    ("r" consult-ripgrep "ripgrep")
    ("d" rlr/consult-rg "rg from dir")
    ("f" rlr/consult-fd "find from dir"))
   ))

(pretty-hydra-define hydra-window
  (:color teal :quit-key "q" title: "Windows")
  ("Windows"
   (("w" other-window "cycle windows" :exit nil)
    ("a" ace-window "ace window")
    ("m" minimize-window "minimize window")
    ("s" transpose-windows "swap windows")
    ("S" shrink-window-if-larger-than-buffer "shrink to fit")
    ("b" balance-windows "balance windows")
    ("t" toggle-window-split "toggle split")
    ("T" enlarge-window" grow taller" :exit nil)
    ("G" enlarge-window-horizontally "grow wider" :exit nil)
    ("o" delete-other-windows "kill other windows"))
   "Frames"
   (("M" iconify-frame "minimize frame")
    ("d" delete-other-frames "delete other frames")
    ("D" delete-frame "delete this frame")
    ("i" make-frame-invisible "invisible frame")
    ("f" toggle-frame-fullscreen "fullscreen")
    ("n" make-frame-command "new frame"))
   "Writeroom"
   (("W" writeroom-mode "toggle writeroom")
    ("M" writeroom-toggle-mode-line "toggle modeline"))))

(pretty-hydra-define hydra-new
  (:color teal :quit-key "q" title: "New")
  ("Denote"
   (("c" org-capture "capture")
    ("n" denote "note")
    ("v" denote-menu-list-notes "view notes")
    ("j" denote-journal-extras-new-or-existing-entry "journal"))
   "Writing"
   (("b" hugo-draft-post "blog post")
    ("a" new-article "article"))
   "Teaching"
   (("l" new-lecture "lecture")
    ("h" new-handout "handout")
    ("s" new-syllabus "syllabus"))
   ))

(pretty-hydra-define hydra-logic
  (:color pink :quit-key "0" :title "Logic")
  ("Operators"
   (("1" (my/insert-unicode "NOT SIGN") "¬")
    ("2" (my/insert-unicode "AMPERSAND") "&")
    ("3" (my/insert-unicode "LOGICAL OR") "v")
    ("4" (my/insert-unicode "SUPERSET OF") "⊃")
    ;; ("4" (my/insert-unicode "RIGHTWARDS ARROW") "→")
    ("5" (my/insert-unicode "IDENTICAL TO") "≡")
    ;; ("5" (my/insert-unicode "LEFT RIGHT ARROW") "↔")
    ("6" (my/insert-unicode "THERE EXISTS") "∃")
    ("7" (my/insert-unicode "FOR ALL") "∀")
    ("8" (my/insert-unicode "WHITE MEDIUM SQUARE") "□")
    ("9" (my/insert-unicode "LOZENGE") "◊")
    ("`" (my/insert-unicode "NOT EQUAL TO") "≠"))
   "Space"
   (("?" (my/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space"))
   "Quit"
   (("0" quit-window "quit" :color blue))
   ))

(pretty-hydra-define hydra-math
  (:color pink :quit-key "?" :title "Math")
  ("Operators"
   (("1" (my/insert-unicode "NOT SIGN") "¬")
    ("2" (my/insert-unicode "AMPERSAND") "&")
    ("3" (my/insert-unicode "LOGICAL OR") "v")
    ("4" (my/insert-unicode "RIGHTWARDS ARROW") "→")
    ("5" (my/insert-unicode "LEFT RIGHT ARROW") "↔")
    ("6" (my/insert-unicode "THERE EXISTS") "∃")
    ("7" (my/insert-unicode "FOR ALL") "∀")
    ("8" (my/insert-unicode "WHITE MEDIUM SQUARE") "□")
    ("9" (my/insert-unicode "LOZENGE") "◊"))
   "Sets"
   (("R" (my/insert-unicode "DOUBLE-STRUCK CAPITAL R") "ℝ real")
    ("N" (my/insert-unicode "DOUBLE-STRUCK CAPITAL N") "ℕ natural")
    ("Z" (my/insert-unicode "DOUBLE-STRUCK CAPITAL Z") "ℤ integer")
    ("Q" (my/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
    ("Q" (my/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
    ("Q" (my/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
    )
   "Space"
   (("?" (my/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space"))
   "Quit"
   (("?" quit-window "quit" :color blue))
   ))

(pretty-hydra-define hydra-hugo
  (:color teal :quit-key "q" :title "Hugo")
  ("Blog"
   (("n" hugo-draft-post "New draft")
    ("p" hugo-publish-post "Publish")
    ("t" hugo-timestamp "Update timestamp")
    ("e" org-hugo-auto-export-mode "Auto export")
    ("d" hugo-deploy "Deploy"))
   ))

(pretty-hydra-define hydra-hydras
  (:color teal :quit-key "q" :title "Hydras")
  ("System"
   (("t" hydra-toggle/body)
    ("b" hydra-buffer/body)
    ("h" hydra-hugo/body)
    ("p" powerthesaurus-hydra/body))
   "Unicode"
   (("l" hydra-logic/body "logic")
    ("m" hydra-math/body)
    )
   )
  )
;; (global-set-key (kbd "s-t") 'hydra-toggle/body)

(major-mode-hydra-define dashboard-mode
  (:quit-key "q")
  ("Open"
   (("m" consult-bookmark "bookmarks")
    ("a" consult-org-agenda "consult-agenda")
    ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
    ("b" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks")
    )))

(major-mode-hydra-define org-agenda-mode
  (:quit-key "q")
  ("Open"
   (("m" consult-bookmark "bookmarks")
    ("a" consult-org-agenda "consult-agenda")
    ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
    ("b" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks")
    )))

(major-mode-hydra-define eww-mode
  (:quit-key "q")
  ("A"
   (
    ;; ("G" eww "Eww Open Browser")
    ("g" eww-reload "Eww Reload")
    ("6" eww-open-in-new-buffer "Open in new buffer")
    ("l" eww-back-url "Back Url")
    ("r" eww-forward-url "Forward Url")
    ("N" eww-next-url "Next Url")
    ("P" eww-previous-url "Previous Url")
    ("u" eww-up-url "Up Url")
    ("&" eww-browse-with-external-browser "Open in External Browser")
    ("d" eww-download "Download")
    ("w" eww-copy-page-url "Copy Url Page")
    );end theme
   "B"
   (
    ("T" endless/toggle-image-display "Toggle Image Display")
    (">" shr-next-link "Shr Next Link")
    ("<" shr-previous-link "Shr Previous Link")
    ("n" scroll-down-command "Scroll Down")
    ("C" url-cookie-list "Url Cookie List")
    ("v" eww-view-source "View Source")
    ("R" eww-readable "Make Readable")
    ("H" eww-list-histories "List History")
    ("E" eww-set-character-encoding "Character Encoding")
    ("s" eww-switch-to-buffer "Switch to Buffer")
    ("S" eww-list-buffers "List Buffers")
    );end highlighting

   "C"
   (

    ("1" rrnet "randyridenour.net")
    ("2" sep "SEP")
    ("F" eww-toggle-fonts "Toggle Fonts")
    ("D" eww-toggle-paragraph-direction "Toggle Paragraph Direction")
    ("c" eww-toggle-colors "Toggle Colors")
    ("b" eww-add-bookmark "Add Bookmark")
    ("B" eww-list-bookmarks "List Bookmarks")
    ("=" eww-next-bookmark "Next Bookmark")
    ("-" eww-previous-bookmark "Previous Bookmark")
    ("O" jao-eww-to-org "Make Org Version")
    ("<SPC>" nil "Quit" :color pink)
    );end other
   ))

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

(major-mode-hydra-define LaTeX-mode
  (:quit-key "q")
  ("Bibtex"
   (("r" citar-insert-citation "citation"))
   "LaTeXmk"
   (("m" rlr/tex-mkpdf "PDFLaTeX")
    ("l" rlr/tex-mklua "LuaLaTeX")
    ("w" rlr/tex-mktc "watch PDFLaTeX")
    ("L" rlr/tex-mklua "watch LuaLaTeX")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all")
    ("n" latex-word-count "word count"))))

(major-mode-hydra-define org-mode
  (:quit-key "q")
  ("Export"
   (
    ("m" rlr/org-mkpdf "Make PDF with PDFLaTeX")
    ("p" rlr/org-open-pdf "View PDF")
    ("l" rlr/org-mklua "Make PDF with LuaLaTeX")
    ("el" org-latex-export-to-latex "Org to LaTeX")
    ("eb" org-beamer-export-to-pdf "Org to Beamer-PDF")
    ("eB" org-beamer-export-to-latex "Org to Beamer-LaTeX")
    ("s" lecture-slides "Lecture slides")
    ("n" lecture-notes "Lecture notes")
    ("ep" present "Present slides")
    ("ec" canvas-copy "Copy HTML for Canvas")
    ("es" canvas-notes "HTML Canvas notes")
    ("eS" make-syllabus "Syllabus")
    ("eh" make-handout "Handout")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all")
    )
   "Edit"
   (
    ("dd" org-deadline "deadline")
    ("ds" org-schedule "schedule")
    ("r" org-refile "refile")
    ("du" rlr/org-date "update date stamp")
    ;; ("fn" org-footnote-new "insert footnote")
    ("ff" org-footnote-action "edit footnote")
    ("fc" citar-insert-citation "citation")
    ("b" org-cycle-list-bullet "cycle bullets" :exit nil)
    ("il" org-mac-link-safari-insert-frontmost-url "insert safari link")
    ("y" yankpad-set-category "set yankpad")
    )
   "View"
   (
    ("vi" consult-org-heading "iMenu")
    ("vu" org-toggle-pretty-entities "org-pretty")
    ("vI" org-toggle-inline-images "Inline images")
    )
   "Blog"
   (("hn" hugo-draft-post "New draft")
    ("hp" hugo-publish-post "Publish")
    ("ht" hugo-timestamp "Update timestamp")
    ("hd" hugo-org-deploy "Deploy")
    ("he" org-hugo-auto-export-mode "Auto export"))
   "Notes"
   (("1" denote-link "link to note"))
   ))

(major-mode-hydra-define dired-mode
  (:quit-key "q")
  ("New"
   (("a" new-article "article")
    ("l" new-lecture "lecture")
    ("h" new-handout "handout")
    ("s" new-syllabus "syllabus"))
   "Tools"
   (("d" crux-open-with "Open in default program")
    ("h" dired-omit-mode "Show hidden files")
    ("p" diredp-copy-abs-filenames-as-kill "Copy filename and path")
    ("n" dired-toggle-read-only "edit Filenames"))))

(major-mode-hydra-define denote-menu-mode
  (:quit-key "q")
  ("Tools"
   (("f" denote-menu-filter "Filter by regex")
    ("k" denote-menu-filter-by-keyword "Filter by keyword")
    ("c" denote-menu-clear-filters "Clear filters")
    ("d" denote-menu-export-to-dired "Dired")
    )))

(defhydra hydra-org (:color teal)
  ("a" org-agenda "agenda")
  ("l" org-store-link "store-link")
  ("q" nil))

(general-define-key
 "s-h" #'hydra-hydras/body
 "s-n" #'hydra-new/body
 "s-t" #'hydra-toggle/body
 "s-w" #'hydra-window/body
 ;; "s-b" #'hydra-buffer/body
 "C-x 9" #'hydra-logic/body)

(general-define-key
 ;; Editing
 ;; "s-/" #'avy-goto-char-timer
 "C-x 4 b" #'consult-buffer-other-window
 "C-x 5 b" #'consult-buffer-other-frame
 "C-x r x" #'consult-register
 "M-s m" #'consult-multi-occur
 )

(defun open-emacs-config ()
  (interactive)
  (find-file "~/.config/emacs/README.org"))

(defun open-fish-functions ()
  (interactive)
  (dired "~/.config/fish/functions"))

(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to #'org-agenda
 "f f" #'find-file
 "f k" #'crux-kill-other-buffers
 "f r" #'consult-buffer
 "f R" #'crux-rename-file-and-buffer
 "f P" #'open-emacs-config
 "f S" #'open-fish-functions
 ;; Helpful
 "H c" #'helpful-command
 "H F" #'helpful-callable
 "H h" #'helpful-at-point
 "H f" #'helpful-function
 "H v" #'helpful-variable
 "H k" #'helpful-key
 ;; Projects
 "p f" #'consult-project-buffer
 "p d" #'project-find-dired
 "t a" #'centaur-tabs-ace-jump
 "t f" #'centaur-tabs-forward-group
 "t k" #'centaur-tabs-kill-unmodified-buffers-in-current-group
 "t K" #'centaur-tabs-kill-other-buffers-in-current-group
 "a" #'org-agenda
 "2" #'rlr/find-file-below
 "3" #'rlr/find-file-right
 "b" #'consult-bookmark
 "c" #'org-capture
 "d s" #'insert-date-string
 "d d" #'insert-standard-date
 "D" #'crux-delete-file-and-buffer
 ;; "h" #'consult-history
 "k" #'crux-kill-other-buffers
 "l" #'dictionary-search
 "m" #'consult-mark
 "n b" #'hugo-draft-post
 "o" #'consult-outline
 "r" #'crux-rename-file-and-buffer
 "s" #'goto-scratch
 "S" #'crux-cleanup-buffer-or-region
 ;; "t" #'crux-visit-term-buffer
 "u" #'unfill-paragraph
 "w" #'ace-window
 "z" #'reveal-in-osx-finder
 "g l" #'avy-goto-line
 "g w" #'avy-goto-word-1
 "C-g" #'pdf-sync-forward-search)

(defun jump-out-of-pair ()
	(interactive)
	(let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
	  ;;
		(when found
			(cond ((or (looking-back "\\*\\*" 2)
		 (looking-back "``" 2)
		 (looking-back "''" 2)
		 (looking-back "==" 2))
			 (forward-char))
			(t (forward-char 0))))))

(global-set-key (kbd "M-1") 'jump-out-of-pair)

(defun reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(setq default-directory "~/")

;; (setq initial-buffer-choice 'my-calendar)

(setq gc-cons-threshold (* 2 1000 1000))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
