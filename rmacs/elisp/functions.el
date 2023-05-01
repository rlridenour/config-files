;;; functions.el -*- lexical-binding: t; -*-


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


(provide 'functions)


;;; functions.el ends here
