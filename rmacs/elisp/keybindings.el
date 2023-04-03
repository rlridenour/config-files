;;; keybindings.el -*- lexical-binding: t; -*-

;; Keybindings

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-m"))
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
 "<f5>" #'deadgrep
 
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


(provide 'keybindings)


;;; keybindings.el ends here
