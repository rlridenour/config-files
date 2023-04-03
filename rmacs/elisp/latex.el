;;; latex.el -*- lexical-binding: t; -*-



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

(server-start)

;; Auto-raise Emacs on activation (from Skim, usually)

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' "))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)


(use-package math-delimiters
  :defer t
  :straight (math-delimiters :type git :host github :repo "oantolin/math-delimiters")
  :config
  (with-eval-after-load 'org
  (define-key org-mode-map "$" #'math-delimiters-insert))
(with-eval-after-load 'tex              ; for AUCTeX
  (define-key TeX-mode-map "$" #'math-delimiters-insert))
(with-eval-after-load 'tex-mode         ; for the built-in TeX/LaTeX modes
  (define-key tex-mode-map "$" #'math-delimiters-insert))
)


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

(defun  
    arara-all ()
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
  (:map LaTeX-mode-map ("C-c r" . latex-change-env))
  ;; (:map LaTeX-mode-map ("s-<return>" . LaTeX-insert-item))
  :config
  (setq latex-change-env-display math-delimiters-display))


(provide 'latex)
;;; latex.el ends here
