;; init.el -*- lexical-binding: t; -*-
;; This is the main init file for Randy Ridenour's Emacs configuration.

(setq user-full-name "Randy Ridenour")
(setq user-mail-address "rlridenour@gmail.com")


;; Silence the "Package cl is deprecated" warning.
(setq byte-compile-warnings '(cl-functions))


;; Silence native compilation warnings
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)
;; Code

;; compile elisp
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; Package management using straight.el

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


;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;  Ensure that system utilities required by various packages are installed.
(use-package use-package-ensure-system-package)

;; Allow key chords in use-package bindings.
(use-package use-package-chords
  :config (key-chord-mode 1))

;; Suppress ad-handle definition warnings
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)


;; Set the path variable to match the shell.


(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(setq custom-file "~/.config/rmacs/custom.el")
(load custom-file)



;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.backups/emacs/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.backups/emacs/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.backups/emacs/autosaves/" t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Set some misc defaults
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      make-pointer-invisible t
      display-time-24hr-format t
      display-time-day-and-date t)

;; Disable bell and flash modeline
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'my-terminal-visible-bell)

;; Show file path in frame title

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; Match parentheses
(show-paren-mode 1)

;; Turn off tool bar and scroll bar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; (if (display-graphic-p)
;;     (progn
;;       (tool-bar-mode -1)
;;       (scroll-bar-mode -1)))

;; Show line numbers and column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; Turn off winner mode
(winner-mode 0)

;; Display line numbers
(global-display-line-numbers-mode)

;; Set the modifier keys in OS X
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)


;; Always prefer UTF-8 encoding.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; Use TeX to input special characters. Activated later for text, markdown, and org modes.
(setq default-input-method 'TeX)

;; Auto save often — save every 20 characters typed (this is the minimum)
(setq auto-save-interval 20)

;; Auto-save open files. This can be toggled off with the toggle hydra.
(auto-save-visited-mode)


;; Use "y" and "n":
(defalias 'yes-or-no-p 'y-or-n-p)


;; Confirm killing emacs on graphical sessions:
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))


;; Edit by Visual Lines
(global-visual-line-mode t)


;; Navigate visual lines:
(setq line-move-visual t)


;; Single space ends sentence:
(setq sentence-end-double-space nil)


                                        ; Map escape to cancel (like C-g)
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;; Save backups and auto-saves to a temp directory.



;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Use spotlight for locate.

(setq locate-command "mdfind")


;; Open links in default Mac browser.

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Don't ask for confirmation to kill processes when exiting Emacs. Credit to [[http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html][Timothy Pratley]].


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))

;; Flash pasted text — from https://christiantietze.de/posts/2020/12/emacs-pulse-highlight-yanked-text/

(require 'pulse)
(defun ct/yank-pulse-advice (orig-fn &rest args)
  ;; Define the variables first
  (let (begin end)
    ;; Initialize `begin` to the current point before pasting
    (setq begin (point))
    ;; Forward to the decorated function (i.e. `yank`)
    (apply orig-fn args)
    ;; Initialize `end` to the current point after pasting
    (setq end (point))
    ;; Pulse to highlight!
    (pulse-momentary-highlight-region begin end)))
(advice-add 'yank :around #'ct/yank-pulse-advice)

;; Don't display async shell command process buffers

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; ibuffer

;; Don't ask for unnecessary confirmations


(setq ibuffer-expert t)


;; Auto-update buffer list


(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))

;; Shell

;; This kills the buffer after closing the terminal.

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'oleh-term-exec-hook)


;; To paste into term.


(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))


;; Make completion case-insensitive in eshell


(setq eshell-cmpl-ignore-case t)
(setq pcomplete-ignore-case t)


;; Start eshell


(global-set-key (kbd "C-x m") (lambda () (interactive) (eshell t)))
;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))


;; Start a regular shell


(global-set-key (kbd "C-x M-m") 'shell)



;; Misc

;; Kill contents of scratch buffer, not the buffer itself. From [[http://emacswiki.org/emacs/RecreateScratchBuffer][TN]].


(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;; Create new scratch buffer after saving.

(defun goto-scratch ()
  "this sends you to the scratch buffer"
  (interactive)
  (let ((goto-scratch-buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer goto-scratch-buffer)
    (org-mode)))

;; Mark date and time that files were saved.


(add-hook 'before-save-hook 'time-stamp)


;; Move deleted files to system trash.


(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Use GNU ls to avoid "Listing directory failed but 'access-file' worked" error.


(setq insert-directory-program "gls"); use proper GNU ls

                                        ;Auto refresh buffers including dired
(setq global-auto-revert-non-file-buffers t)

                                        ; Do not generate any messages (be quiet about refreshing Dired).
(setq auto-revert-verbose nil)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)
;;show recursion depth in minibuffer
(minibuffer-depth-indicate-mode t)

                                        ;two identical buffers get uniquely numbered names
(require 'uniquify)

;; From [[https://dougie.io/emacs/indentation/#tldr-the-full-configuration]]



                                        ; START TABS CONFIG
;; Create a variable for our preferred tab width
(setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)


;; Prevent escape from closing other windows
(defun my-keyboard-escape-quit (fun &rest args)
  (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
    (apply fun args)))
(advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit)


                                        ; Abbreviations and Bookmarks

;; Load Abbreviations

(load "~/Dropbox/emacs/my-emacs-abbrev")


;; Bookmarks

(require 'bookmark)
(bookmark-bmenu-list)

;; CUA mode for easy rectangle editing.
(cua-selection-mode t)


(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t)
  :config
  ;; load preferred theme
  (load-theme 'lambda-light))


(setq frame-resize-pixelwise t)

;; (setq default-frame-alist '((font . "Droid Sans Mono Slashed-16"))) ;;; set default font for emacs --daemon / emacsclient

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0)

(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;; Use Telephone Line for styling the mode-line.

;; (use-package telephone-line
;;   :config
;;   (telephone-line-mode 1))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Vertico

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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


;; Consult

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
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



(use-package consult-dir
  :straight (:host github :repo "karthink/consult-dir" :branch "master")
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package  ace-window
  :bind ("C-c w" . ace-window)
  :config
  ;; (setq aw-leading-char-style 'path)
  (setq aw-background nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shrink-whitespace
  :bind ("M-=" . shrink-whitespace))

(use-package company
  :init
  (setq company-idle-delay 3)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-tng-mode))

;; Highlight current line when idle
(use-package hl-line+
  :config
  (toggle-hl-line-when-idle))


(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package general
  :config
  (general-auto-unbind-keys)
  )

(use-package flycheck)

(use-package smex)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (global-auto-revert-mode))

(use-package reveal-in-osx-finder
  :bind ("C-c z" . reveal-in-osx-finder))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package smartparens
  :diminish smartparens-mode)
(require 'smartparens-config)
(smartparens-global-mode t)


(use-package aggressive-indent)

;; Undo-fu and undo-fu session mode for undo and redo.

(use-package undo-fu
  :general
  ("s-z"   'undo-fu-only-undo
   "s-Z" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(global-undo-fu-session-mode)


(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.config/emacs/snippets")))
  (yas-global-mode 1))

;; With this code, yasnippet will expand the snippet if company didn't complete the word
;; replace company-complete-common with company-complete if you're using it

(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))

(advice-add 'company-complete-common :after (lambda ()
                                              (when (equal my-company-point (point))
                                                (yas-expand))))

(use-package wc-mode)

(use-package crux)

(use-package fish-mode)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package dogears
  :straight (dogears :host github :repo "alphapapa/dogears.el")
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

(use-package pomm
  :straight (:host github :repo "SqrtMinusOne/pomm.el" :files (:defaults "resources"))
  :commands (pomm))

(use-package anki-editor)

(use-package pdf-tools
  :pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)




;; Keybindings

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-h"))
(global-unset-key (kbd "<S-return>"))


(use-package key-chord
  :defer t
  :config
  (key-chord-mode 1))


;; Hydras


;; (use-package ivy-hydra)

(use-package hydra)

(use-package major-mode-hydra
  :bind
  ("s-m" . major-mode-hydra))



;; Hydra-toggle

(defun my/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(pretty-hydra-define hydra-toggle
  (:color teal :quit-key "q" :title "Toggle")
  (" "
   (("a" abbrev-mode "abbrev" :toggle t)
    ("d" toggle-debug-on-error "debug" (default value 'debug-on-error))
    ("e" evil-mode "evil" :toggle t)
    ("i" aggressive-indent-mode "indent" :toggle t)
    ("f" auto-fill-mode "fill" :toggle t)
    ("l" display-line-numbers-mode "linum" :toggle t)
    ("m" toggle-frame-maximized-undecorated "max" :toggle t)
    ("p" smartparens-mode "smartparens" :toggle t)
    ("t" toggle-truncate-lines "truncate" :toggle t)
    ("s" whitespace-mode "whitespace" :toggle t))
   " "
   (("c" cdlatex-mode "cdlatex" :toggle t)
    ("o" olivetti-mode "olivetti" :toggle t)
    ("r" read-only-mode "read-only" :toggle t)
    ("v" view-mode "view" :toggle t)
    ("w" wc-mode "word-count" :toggle t)
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
   (("l" consult-goto-line "goto-line")
    ("i" consult-imenu "imenu")
    ("m" consult-mark "mark")
    ("o" consult-outline "outline"))
   "Global"
   (("M" consult-global-mark "global-mark")
    ("r" consult-ripgrep "ripgrep"))
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
    ("o" delete-other-windows "other windows"))
   "Frames"
   (("M" iconify-frame "minimize frame")
    ("d" delete-other-frames "delete other frames")
    ("D" delete-frame "delete this frame")
    ("i" make-frame-invisible "invisible frame")
    ("f" toggle-frame-fullscreen "fullscreen")
   )))

(pretty-hydra-define hydra-logic
  (:color pink :quit-key "0" :title "Logic")
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
 

(pretty-hydra-define hydra-hydras
  (:color teal :quit-key "q" :title "Hydras")
  ("Unicode"
   (("l" hydra-logic/body "logic")
    ("m" hydra-math/body))
   )
  )

;; (global-set-key (kbd "s-t") 'hydra-toggle/body)

;; Major-mode Hydras


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
   (("r" citar-insert-citation "citation"))
   "LaTeXmk"
   (("a" rlr/tex-mkt "arara")
    ("w" rlr/tex-mktc "arara watch")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all")
    ("n" latex-word-count "word count"))))

(major-mode-hydra-define org-mode
  (:quit-key "q")
  ("Export"
   (("l" org-latex-export-to-latex "Org to LaTeX")
    ("a" rlr/org-mkt "Make PDF with Arara")
    ("w" rlr/org-mktc "Make PDF and Watch")
    ("b" org-beamer-export-to-pdf "Org to Beamer-PDF")
    ("B" org-beamer-export-to-latex "Org to Beamer-LaTeX")
    ("s" lecture-slides "Lecture slides")
    ("h" canvas-copy "Copy html for Canvas")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all")
    )
   "Edit"
   (("d" rlr/org-date "update date stamp")
    ("i" consult-org-heading "iMenu")
    ("r" citar-insert-citation "citation")
    ("u" org-toggle-pretty-entities "org-pretty"))
   "Blog"
   (("n" hugo-draft-post "New draft")
    ("p" hugo-publish-post "Publish")
    ("t" hugo-timestamp "Update timestamp")
    ("e" org-hugo-auto-export-mode "Auto export"))
"Other"
(("I" org-toggle-inline-images "Inline images"))
   ))






(major-mode-hydra-define dired-mode
  (:quit-key "q")
  ("Tools"
   (("d" crux-open-with "Open in default program")
    ("p" diredp-copy-abs-filenames-as-kill "Copy filename and path")
    ("n" dired-toggle-read-only "edit Filenames"))))

(defhydra hydra-org (:color teal)
  ("a" org-agenda "agenda")
  ("l" org-store-link "store-link")
  ("q" nil))




(bind-chords
 ("jh" . crux-switch-to-previous-buffer)
 ("hj" . crux-switch-to-previous-buffer))



;; Global Keybindings

;; Make things more Mac-like

(general-define-key
 "<s-up>" 'beginning-of-buffer
 "<s-down>" 'end-of-buffer
 "<s-right>" 'end-of-visual-line
 "<s-left>" 'beginning-of-visual-line
 "s-w" 'delete-frame
 "<C-tab>" 'other-window
 "<M-down>" 'forward-paragraph
 "<M-up>" 'backward-paragraph)




(general-define-key

 ;; Windows and frames
 "C-0" 'delete-window-balance
 "C-1" 'delete-other-windows
 "C-2" 'split-window-below-focus
 "C-3" 'split-window-right-focus
 "s-K" 'nuke-all-buffers
 "s-6" 'toggle-window-split
 "S-C-<left>" 'shrink-window-horizontally
 "S-C-<right>" 'enlarge-window-horizontally
 "S-C-<down>" 'shrink-window
 "S-C-<up>" 'enlarge-window
 "C-x w" 'delete-frame
 "M-o" 'crux-other-window-or-switch-buffer

 ;; Files and buffers
 "C-x c" 'save-buffers-kill-emacs
 "C-x C-b" 'ibuffer
 "C-`" 'iterm-goto-filedir-or-home
 "s-o" 'find-file
 "s-k" 'kill-buffer-and-window
 "s-r" 'consult-buffer
 "M-s-r" 'consult-buffer-other-window
 "C-S-a" 'embark-act

 ;; Search

 "s-l" 'hydra-locate/body
 "s-f" 'consult-line
 ;; "C-s" 'consult-isearch
 ;; "C-r" 'consult-isearch-reverse

 ;; Editing
 "RET" 'newline-and-indent
 "M-/" 'hippie-expand
 "C-+" 'text-scale-increase
 "C--" 'text-scale-decrease
 "<s-backspace>" 'kill-whole-line
 "s-j" 'crux-top-join-line
 "<S-return>" 'crux-smart-open-line
 "<C-S-return>" 'crux-smart-open-line-above
 "M-y" 'consult-yank-pop
 "M-q" 'reformat-paragraph

  ;; Hydras
 "s-h" 'hydra-hydras/body
 "s-t" 'hydra-toggle/body
 "s-w" 'hydra-window/body
 "s-b" 'hydra-buffer/body
 "C-x 9" 'hydra-logic/body

 "s-/" 'avy-goto-char-timer
 "s-d" 'consult-dir
 "s-=" 'endless/ispell-word-then-abbrev
 "<help> a" 'consult-apropos
 "C-x 4 b" 'consult-buffer-other-window
 "C-x 5 b" 'consult-buffer-other-frame
 "C-x r x" 'consult-register
 "M-s m" 'consult-multi-occur
 "<f8>" 'insert-standard-date
 "M-u" 'upcase-dwim
 "M-l" 'downcase-dwim
 "M-c" 'capitalize-dwim
 )


;; "C-c u" 'unfill-paragraph
;; "C-c C-<return>" 'split-org-item)
;; "C-c o" 'crux-open-with
;; "C-c D" 'crux-delete-file-and-buffer
;; "C-c C-k" 'compile



;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "2" 'rlr/find-file-below
 "3" 'rlr/find-file-right
 "b" 'consult-bookmark
 "c" 'org-capture
 "D" 'crux-delete-file-and-buffer
 ;; "h" 'consult-history
 "k" 'crux-kill-other-buffers
 "m" 'consult-mark
 "n b" 'hugo-draft-post
 "n c" 'org-roam-capture
 "n f" 'org-roam-node-find
 "n g" 'org-roam-graph
 "n i" 'org-roam-node-insert
 "n j" 'org-roam-dailies-capture-today
 "n t" 'org-roam-buffer-toggle 
 "o" 'consult-outline
 "r" 'crux-rename-file-and-buffer
 "s" 'goto-scratch
 "S" 'crux-cleanup-buffer-or-region
 "t" 'crux-visit-term-buffer
 "u" 'unfill-paragraph
 "w" 'ace-window
 "z" 'reveal-in-osx-finder)





(provide 'init)

;;; init.el ends here
