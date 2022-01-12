;;; base-functions.el -*- lexical-binding: t; -*-
;; Window management, from https://www.bytedude.com/useful-emacs-shortcuts/

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

(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (let* ((frame (selected-frame))
         (on? (and (frame-parameter frame 'undecorated)
                   (eq (frame-parameter frame 'fullscreen) 'maximized)))
         (geom (frame-monitor-attribute 'geometry))
         (x (nth 0 geom))
         (y (nth 1 geom))
         (display-height (nth 3 geom))
         (display-width (nth 2 geom))
         (cut (if on?
                  (if ns-auto-hide-menu-bar 26 50)
                (if ns-auto-hide-menu-bar 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height cut) nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

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
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
	 (let ((fn (dired-get-file-for-visit)))
	   (start-process "default-app" nil "open" fn))))))


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


(defun xah-title-case-region-or-line (φbegin φend)
  "Title case text between nearest brackets, or current line, or text selection.
  Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

  When called in a elisp program, φbegin φend are region boundaries.
  URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
  Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
	   ξp1
	   ξp2
	   (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
	 (skip-chars-backward ξskipChars (line-beginning-position))
	 (setq ξp1 (point))
	 (skip-chars-forward ξskipChars (line-end-position))
	 (setq ξp2 (point)))
       (list ξp1 ξp2))))
  (let* (
	 (ξstrPairs [
		     [" A " " a "]
		     [" And " " and "]
		     [" At " " at "]
		     [" As " " as "]
		     [" By " " by "]
		     [" Be " " be "]
		     [" Into " " into "]
		     [" In " " in "]
		     [" Is " " is "]
		     [" It " " it "]
		     [" For " " for "]
		     [" Of " " of "]
		     [" Or " " or "]
		     [" On " " on "]
		     [" Via " " via "]
		     [" The " " the "]
		     [" That " " that "]
		     [" To " " to "]
		     [" Vs " " vs "]
		     [" With " " with "]
		     [" From " " from "]
		     ["'S " "'s "]
		     ]))
    (save-excursion 
      (save-restriction
	(narrow-to-region φbegin φend)
	(upcase-initials-region (point-min) (point-max))
	(let ((case-fold-search nil))
	  (mapc
	   (lambda (ξx)
	     (goto-char (point-min))
	     (while
		 (search-forward (aref ξx 0) nil t)
	       (replace-match (aref ξx 1) 'FIXEDCASE 'LITERAL)))
	   ξstrPairs))))))

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


(defun rlr/ivy-dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
	 (delete-dups
	  (mapcar (lambda (file)
		    (if (file-directory-p file) file (file-name-directory file)))
		  recentf-list))))

    (let ((dir (ivy-read "Directory: "
			 recent-dirs
			 :re-builder #'ivy--regex
			 :sort nil
			 :initial-input nil)))
      (dired dir))))

;; From http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
	With prefix P, create local abbrev. Otherwise it will
	be global.
	If there's nothing wrong with the word at point, keep
	looking for a typo until the beginning of buffer. You can
	skip typos you don't want to fix with `SPC', and you can
	abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
		 ;; Word was corrected or used quit.
		 (if (ispell-word nil 'quiet)
		     nil ; End the loop.
		   ;; Also end if we reach `bob'.
		   (not (bobp)))
	       ;; If there's no word at point, keep looking
	       ;; until `bob'.
	       (not (bobp)))
	(backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
	(let ((aft (downcase aft))
	      (bef (downcase bef)))
	  (define-abbrev
	    (if p local-abbrev-table global-abbrev-table)
	    bef aft)
	  (message "\"%s\" now expands to \"%s\" %sally"
		   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; From Xah Lee, http://ergoemacs.org/emacs/elisp_unicode_replace_invisible_chars.html

(defun rlr-replace-BOM-mark-etc ()
  "Query replace some invisible Unicode chars.
  The chars to be searched are:
   ZERO WIDTH NO-BREAK SPACE (codepoint 65279, #xfeff)
   RIGHT-TO-LEFT MARK (codepoint 8207, #x200f)
   RIGHT-TO-LEFT OVERRIDE (codepoint 8238, #x202e)

  Search begins at cursor position. (respects `narrow-to-region')

  This is useful for text copied from twitter or Google Plus, because they often contain BOM mark. See URL `http://xahlee.info/comp/unicode_BOM_byte_orde_mark.html'

  URL `http://ergoemacs.org/emacs/elisp_unicode_replace_invisible_chars.html'
  Version 2015-10-25"
  (interactive)
  (query-replace-regexp "\u200f\\|\u202e\\|\ufeff" ""))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))
(add-hook 'find-file-not-found-functions #'make-parent-directory)

;; Opens iTerm in the directory of the current file.
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
(global-set-key (kbd "C-`") 'iterm-goto-filedir-or-home)


;; Create pop-up shell with F12 key
;; From https://tsdh.wordpress.com/2011/10/12/a-quick-pop-up-shell-for-emacs/

(defvar th-shell-popup-buffer nil)

(defun th-shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p th-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window th-shell-popup-buffer))
	(dir (file-name-directory (or (buffer-file-name)
				      ;; dired
				      dired-directory
				      ;; use HOME
				      "~/"))))
    (if win
	(quit-window nil win)
      (pop-to-buffer th-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " dir "\n")))))

(global-set-key (kbd "<f12>") 'th-shell-popup)

;; Saved Keyboard Macros

;; Splits Org-mode list items


(fset 'split-org-item
   [?\C-k ?\M-\\ return ?\C-y])

(fset 'convert-markdown-to-org
   [?\M-< ?\M-% ?* return ?- return ?! ?\M-< ?\C-\M-% ?# ?* backspace backspace ?  ?# ?* ?$ return return ?! ?\M-< ?\M-% ?# return ?* return ?!])


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


(provide 'base-functions)
