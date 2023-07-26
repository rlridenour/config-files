;;; settings.el -*- lexical-binding: t; -*-

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (set-fringe-mode 4)
(global-visual-line-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(column-number-mode)
(global-display-line-numbers-mode)
(save-place-mode 1)
(setq-default cursor-in-non-selected-windows nil
	      frame-title-format '("%f [%m]"))

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

(provide 'settings)


;;; settings.el ends here
