;;; init.el --- Personal Emacs configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is generated from init.org.  Please edit that file instead

(setq gc-cons-threshold (* 50 1000 1000))

(setq user-full-name "Randy Ridenour"
      user-mail-address "rlridenour@gmail.com")

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

(use-package exec-path-from-shell
	:config (exec-path-from-shell-initialize))

(use-package org-auto-tangle
:defer t
:hook (org-mode . org-auto-tangle-mode))

(global-visual-line-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(column-number-mode)
(global-display-line-numbers-mode)
(save-place-mode 1)

(setq default-fill-column 100
      make-backup-files nil
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      use-dialog-box nil
      vc-follow-symlinks t
      tramp-default-method "ssh"
      global-auto-revert-non-file-buffers t
      message-kill-buffer-on-exit t
      large-file-warning-threshold nil
      sentence-end-double-space nil
      dictionary-server "dict.org"
      case-replace nil
      bookmark-save-flag 1
      dired-auto-revert-buffer t
      dired-dwim-target "dired-dwim-target-next"
      eshell-scroll-to-bottom-on-input "this"
      ediff-split-window-function "split-window-horizontally"
      ediff-window-setup-function "ediff-setup-windows-plain"
      man-notify-method "aggressive"
      show-paren-delay 0
      )

(show-paren-mode)

(setf use-short-answers t)

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

(add-hook 'before-save-hook 'time-stamp)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

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


					; Map escape to cancel (like C-g)
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;;; Search

;; Show number of matches at the end of search field.

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;; Save backups and auto-saves to a temp directory.


(setq
 backup-by-copying t			; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/"))			; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

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

;; Don't display async shell command process buffers

(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; ibuffer

;; Don't ask for unnecessary confirmations


(setq ibuffer-expert t)


;; Auto-update buffer list


(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "home")))


;;; Abbreviations and Bookmarks

;; Load Abbreviations

(load "~/Dropbox/emacs/my-emacs-abbrev")


;; Bookmarks

(require 'bookmark)
(bookmark-bmenu-list)



;; Dired

(use-package dired-x
  :straight (:type built-in)
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
	  (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^\\..+$"))
    (setq-default dired-omit-extensions '("fdb_latexmk" "aux" "bbl" "blg" "fls" "glo" "idx" "ilg" "ind" "ist" "log" "out" "gz" "DS_Store" "xml" "bcf" "nav" "snm" "toc"))))


;; Spelling

;; Use f7 to check word, shift-f7 to check entire buffer.

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(global-set-key (kbd "S-<f7>") (lambda ()
				(interactive)
				(let ((current-prefix-arg '(4)))
				  (call-interactively #'jinx-correct))))

;;; Fonts 

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0)

(use-package all-the-icons)


;; Modus Themes


(use-package modus-themes
  :ensure t
  :straight (modus-themes :type git :flavor melpa :host sourcehut :repo "protesilaos/modus-themes")
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t)

  (define-key global-map (kbd "<f9>") #'modus-themes-toggle))



(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))


(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(use-package meow
  :init
  (defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
  :config
  (meow-setup)
  (add-to-list 'meow-mode-state-list '(text-mode . insert))
  (add-to-list 'meow-mode-state-list '(prog-mode . insert))
  (add-to-list 'meow-mode-state-list '(term-mode . insert))
  (setq meow-use-clipboard t)
  (meow-global-mode 1))

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

;; Yasnippet
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/snippets"))
  :config
  (yas-global-mode 1))

;; Auto-activating snippets 
(use-package aas
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
  :hook (TeX-mode . laas-mode))

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
        '(("DONE" . "green4") ("TODO" . org-warning)))
  (setq org-agenda-files '("/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")))

(use-package org-contrib)

;; Don't export headlines with :ignore: tag, but do export content.
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

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

;;; Org-Footnote Assistant (https://github.com/lazzalazza/org-footnote-assistant)

(straight-use-package '(org-footnote-assistant :type git :host github :repo "lazzalazza/org-footnote-assistant"))

(use-package org-footnote-assistant
  :straight (org-footnote-assistant :type git :host github :repo "lazzalazza/org-footnote-assistant")
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


;; Denote
(use-package denote
  :defer t
  :config
  (setq denote-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/Documents/notes")
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-date-format nil)
  )

(use-package consult-notes
  :defer t
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

(use-package denote-menu
  :defer t) 

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
  :straight (org-bulletproof :type git :host github :repo "pondersson/org-bulletproof")
  :config
  (setq org-bulletproof-default-ordered-bullet "1.")
  (global-org-bulletproof-mode +1))



;; HTML

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package avy
  :defer t
  :config
  (avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package ace-window
  :defer t)

(use-package shrink-whitespace
  :defer t)

(use-package hl-line+
  :config
  (toggle-hl-line-when-idle 1))

(use-package expand-region
  :defer t)

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
  )

(use-package reveal-in-osx-finder
  :defer t)

(use-package hungry-delete
  :defer t
  :config
  (global-hungry-delete-mode))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))

(use-package aggressive-indent)

(use-package evil-nerd-commenter
  :defer t)


(use-package which-key
  :config
  (which-key-mode))

(use-package crux
  :defer t)

(use-package fish-mode
  :defer t)

(use-package vundo)

(use-package unfill
  :defer t)


(use-package yankpad
  :defer t
  :init
  (setq yankpad-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/yankpad.org")
  :config
  (bind-key "<f6>" 'yankpad-insert))

(use-package titlecase
  :defer t
  :config
  (setq titlecase-style "chicago"))




;; (use-package pdf-tools
;;    :pin manual
;;    :config
;;    (pdf-tools-install)
;;    (setq-default pdf-view-display-size 'fit-width)
;;    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;    :custom
;;    (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))

;; ;; (evil-set-initial-state 'pdf-view-mode 'emacs)
;; (add-hook 'pdf-view-mode-hook
;;   (lambda ()
;;     (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))


(use-package pulsar
  :custom
  (setq pulsar-pulse-functions
	'(isearch-repeat-forward
	  isearch-repeat-backward
	  recenter-top-bottom
	  move-to-window-line-top-bottom
	  reposition-window
	  bookmark-jump
	  other-window
	  delete-window
	  delete-other-windows
	  forward-page
	  backward-page
	  scroll-up-command
	  scroll-down-command
	  windmove-right
	  windmove-left
	  windmove-up
	  windmove-down
	  windmove-swap-states-right
	  windmove-swap-states-left
	  windmove-swap-states-up
	  windmove-swap-states-down
	  tab-new
	  tab-close
	  tab-next
	  org-next-visible-heading
	  org-previous-visible-heading
	  org-forward-heading-same-level
	  org-backward-heading-same-level
	  outline-backward-same-level
	  outline-forward-same-level
	  outline-next-visible-heading
	  outline-previous-visible-heading
	  outline-up-heading))
  :hook
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry)
  :config
  (setq pulsar-pulse t
	pulsar-delay 0.2
	pulsar-iterations 10
	pulsar-face 'pulsar-blue
	pulsar-highlight-face 'pulsar-blue))

(pulsar-global-mode 1)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;;  (setq doom-fallback-buffer-name "*dashboard*")
  (setq dashboard-week-agenda nil)
  (setq dashboard-startup-banner "/Users/rlridenour/.config/doom/logo-emacs.png")
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
  )


(defun goto-dashboard ()
  "this sends you to the dashboard buffer"
  (interactive)
  (let ((goto-dashboard-buffer (get-buffer "*dashboard*")))
    (switch-to-buffer goto-dashboard-buffer))
  (dashboard-refresh-buffer))

(use-package deadgrep)


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
  :defer t)


;; Emacs-term-toggle
;; https://github.com/amno1/emacs-term-toggle
(use-package emacs-term-toggle
  :defer t
  :straight (emacs-term-toggle :host github :repo "amno1/emacs-term-toggle")
  :config
  (setq term-toggle-no-confirm-exit t)
  )


(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package emacs-everywhere)

(use-package powerthesaurus)

(use-package yaml-mode)

(use-package eat
  :defer t
  :straight (eat :host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("terminfo/e" "terminfo/e/*")
			 ("terminfo/65" "terminfo/65/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el"))))


(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package visual-regexp
  :config
  )

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


;; Fullscreen


(defun toggle-frame-maximized-undecorated () (interactive) (let* ((frame (selected-frame)) (on? (and (frame-parameter frame 'undecorated) (eq (frame-parameter frame 'fullscreen) 'maximized))) (geom (frame-monitor-attribute 'geometry)) (x (nth 0 geom)) (y (nth 1 geom)) (display-height (nth 3 geom)) (display-width (nth 2 geom)) (cut (if on? (if ns-auto-hide-menu-bar 26 50) (if ns-auto-hide-menu-bar 4 26)))) (set-frame-position frame x y) (set-frame-parameter frame 'fullscreen-restore 'maximized) (set-frame-parameter nil 'fullscreen 'maximized) (set-frame-parameter frame 'undecorated (not on?)) (set-frame-height frame (- display-height cut) nil t) (set-frame-width frame (- display-width 20) nil t) (set-frame-position frame x y)))



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



;; Open files in dired mode using 'open' in OS X
;; (eval-after-load "dired"
;;   '(progn
;;      (define-key dired-mode-map (kbd "z")
;;        (lambda () (interactive)
;;          (let ((fn (dired-get-file-for-visit)))
;;            (start-process "default-app" nil "open" fn))))))



(defun rlr-count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
        (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))




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




(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))



;; From https://github.com/ocodo/.emacs.d/blob/master/custom/handy-functions.el
(defun nuke-all-buffers ()
  "Kill all the open buffers except the current one.
  Leave *scratch*, *dashboard* and *Messages* alone too."
  (interactive)
  (mapc
   (lambda (buffer)
     (unless (or
              (string= (buffer-name buffer) "*scratch*")
              (string= (buffer-name buffer) "*dashboard*")
              (string= (buffer-name buffer) "*Messages*"))
       (kill-buffer buffer)))
   (buffer-list))
  (delete-other-windows))



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



(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))
(add-hook 'find-file-not-found-functions #'make-parent-directory)


;; Fill functions from https://schauderbasis.de/posts/reformat_paragraph/


(use-package unfill)

(defun fill-sentences-in-paragraph ()
  "Put a newline at the end of each sentence in the current paragraph."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (call-interactively 'fill-sentences-in-region)
    )
  )

(defun fill-sentences-in-region (start end)
  "Put a newline at the end of each sentence in the region maked by (start end)."
  (interactive "*r")
  (call-interactively 'unfill-region)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (forward-sentence)
      (if (looking-at-p " ")
          (newline-and-indent)
        )
      )
    )
  )

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



;; Move lines, from [[https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/][Bozhidar Batsov]]


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

;; From Vernon Grant (https://gist.github.com/VernonGrant/1341a3bdcded3fc3a3741427f706ca85)
;; Zap up to char quickly.
(defun vg-quick-zap-up-to-char (p c)
  "The same as zap up to char, but without the mini buffer prompt.
P: The prefix argument or the count.
C: The character to zap up to."
  (interactive "P\nc")
  (let ((cnt (cond ((null p) 1)
                   ((symbolp p) -1)
                   (t p))))
    (zap-up-to-char cnt c)))


;; From https://macowners.club/posts/custom-functions-5-navigation/
(defun rlr/consult-rg ()
  "Function for `consult-ripgrep' with the `universal-argument'."
  (interactive)
  (consult-ripgrep (list 4)))

(defun rlr/consult-fd ()
  "Function for `consult-find' with the `universal-argument'."
  (interactive)
  (consult-find (list 4)))

;; This updates the time stamp on a Hugo post.



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


;; Set a few variables and some utility functions that are used later.


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



;; Set some keybindings for the Hugo functions.



;; (global-set-key (kbd "C-c h n") 'hugo-draft-post)
;; (global-set-key (kbd "C-c h p") 'hugo-publish-post)
;; (global-set-key (kbd "C-c h t") 'hugo-timestamp)
;; (global-set-key (kbd "C-c h O") (lambda () (interactive) (find-file "~/Sites/blog/")))
;; (global-set-key (kbd "C-c h P") (lambda () (interactive) (find-file "~/Sites/blog/content/post/")))
;; (global-set-key (kbd "C-c h d") 'hugo-deploy)
;; (global-set-key (kbd "C-c h g") 'hugo-select-tags)
;; (global-set-key (kbd "C-c h m") 'hugo-update-lastmod)

;; Keybindings

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-d"))
(global-unset-key (kbd "s-h"))
(global-unset-key (kbd "<S-return>"))

(use-package general
  :config
  (general-auto-unbind-keys)
  )

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
    ("e" meow-global-mode "meow" :toggle t)
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
    ("f" rlr/consult-fd "find from dir")
   )))

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
    ("n" make-frame-command "new frame")
   )))

(pretty-hydra-define hydra-new
(:color teal :quit-key "q" title: "New")
("Denote"
(("b" hugo-draft-post "blog post")
("c" org-capture "capture")
("n" denote "note")
("v" denote-menu-list-notes "view notes")
("j" my-denote-journal "journal"))
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
    ("h" hydra-hugo/body)
    ("p" powerthesaurus-hydra/body))
   "Unicode"
   (("l" hydra-logic/body "logic")
    ("m" hydra-math/body)
    )
   )
  )

;; (global-set-key (kbd "s-t") 'hydra-toggle/body)


;; Major-mode Hydras

(major-mode-hydra-define dashboard-mode
  (:quit-key "q")
  ("Open"
   (("m" consult-bookmark "bookmarks")
    ("a" consult-org-agenda "consult-agenda")
    ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
    )))

(major-mode-hydra-define eww-mode
  (:quit-key "q")
  ("A"
(
    ("G" eww "Eww Open Browser")
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


(major-mode-hydra-define latex-mode
  (:quit-key "q")
  ("Bibtex"
   (("r" citar-insert-citation "citation"))
   "LaTeXmk"
   (("m" rlr/tex-mkt "compile")
    ("w" rlr/tex-mktc "watch")
    ("c" tex-clean "clean aux")
    ("C" tex-clean-all "clean all")
    ("n" latex-word-count "word count"))))


(major-mode-hydra-define org-mode
  (:quit-key "q")
  ("Export"
   (
    ("m" rlr/org-mkt "Make PDF with Arara")
    ("el" org-latex-export-to-latex "Org to LaTeX")
    ("eb" org-beamer-export-to-pdf "Org to Beamer-PDF")
    ("eB" org-beamer-export-to-latex "Org to Beamer-LaTeX")
    ("s" lecture-slides "Lecture slides")
    ("n" lecture-notes "Lecture notes")
    ("ep" present "Present slides")
    ("eh" canvas-copy "Copy html for Canvas")
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
    ("l" org-mac-link-safari-insert-frontmost-url "insert safari link")
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
  ("Tools"
   (("d" crux-open-with "Open in default program")
    ("h" dired-omit-mode "Show hidden files")
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
 "<s-up>" #'beginning-of-buffer
 "<s-down>" #'end-of-buffer
 "<s-right>" #'end-of-visual-line
 "<s-left>" #'beginning-of-visual-line
 "s-w" #'delete-frame
 "<C-tab>" #'other-window
 "<M-down>" #'forward-paragraph
 "<M-up>" #'backward-paragraph
 "C-`" #'iterm-goto-filedir-or-home)




(general-define-key

 ;; Windows and frames
 "C-0" #'delete-window-balance
 "C-1" #'delete-other-windows
 "C-2" #'split-window-below-focus
 "C-3" #'split-window-right-focus
 "s-K" #'nuke-all-buffers
 "s-6" #'toggle-window-split
 "S-C-<left>" #'shrink-window-horizontally
 "S-C-<right>" #'enlarge-window-horizontally
 "S-C-<down>" #'shrink-window
 "S-C-<up>" #'enlarge-window
 "C-x w" #'delete-frame
 "M-o" #'crux-other-window-or-switch-buffer

 ;; Files and buffers
 "C-x c" #'save-buffers-kill-emacs
 "C-x C-b" #'ibuffer
 "C-`" #'iterm-goto-filedir-or-home
 "s-o" #'find-file
 "s-k" #'kill-this-buffer
 "M-s-k" #'kill-buffer-and-window
 "s-r" #'consult-buffer
 "M-s-r" #'consult-buffer-other-window
 "C-S-a" #'embark-act
 ;; "M-<RET>" #'crux-open-with

 ;; Search

 "s-l" #'hydra-locate/body
 "s-f" #'consult-line
 "<f5>" #'deadgrep

 ;; "C-s" #'consult-isearch
 ;; "C-r" #'consult-isearch-reverse

 ;; Toggle term
 "<f2>" #'term-toggle-ansi
 "<S-f2>" #'term-toggle-eshell


 ;; Editing
 "RET" #'newline-and-indent
 "M-/" #'hippie-expand
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease
 "C-z" #'vg-quick-zap-up-to-char
 "<s-backspace>" #'kill-whole-line
 "s-j" #'crux-top-join-line
 "<S-return>" #'crux-smart-open-line
 "<C-S-return>" #'crux-smart-open-line-above
 "M-y" #'consult-yank-pop
 "M-q" #'reformat-paragraph
 "M-;" #'evilnc-comment-or-uncomment-lines
 "M-#" #'dictionary-lookup-definition
 "M-=" #'shrink-whitespace
 "<f7>" #'jinx-correct

 ;; Hydras
 "s-h" #'hydra-hydras/body
 "s-n" #'hydra-new/body
 "s-t" #'hydra-toggle/body
 "s-w" #'hydra-window/body
 "s-b" #'hydra-buffer/body
 "C-x 9" #'hydra-logic/body

 "s-/" #'avy-goto-char-timer
 "s-d" #'goto-dashboard
 "s-=" #'endless/ispell-word-then-abbrev
 "<help> a" #'consult-apropos
 "C-x 4 b" #'consult-buffer-other-window
 "C-x 5 b" #'consult-buffer-other-frame
 "C-x r x" #'consult-register
 "M-s m" #'consult-multi-occur
 "<f6>" #'yankpad-insert
 "<f8>" #'insert-standard-date
 "M-u" #'upcase-dwim
 "M-l" #'downcase-dwim
 "M-c" #'capitalize-dwim
 )

;; "C-c u" #'unfill-paragraph
;; "C-c C-<return>" #'split-org-item)
;; "C-c o" #'crux-open-with
;; "C-c D" #'crux-delete-file-and-buffer
;; "C-c C-k" #'compile

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to #'org-agenda
 "a" #'org-agenda
 "2" #'rlr/find-file-below
 "3" #'rlr/find-file-right
 "b" #'consult-bookmark
 "c" #'org-capture
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
 "t" #'crux-visit-term-buffer
 "u" #'unfill-paragraph
 "w" #'ace-window
 "z" #'reveal-in-osx-finder
 "g l" #'avy-goto-line
 "g w" #'avy-goto-word-1
 )


(general-define-key
:keymaps 'dired-mode-map
 "M-<RET>" #'crux-open-with
)

(setq default-directory "~/")


;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

(setq gc-cons-threshold (* 2 1000 1000))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
