;;; keybindings.el -*- lexical-binding: t; -*-

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
    ("n" make-frame-command "new frame")
   )))

(pretty-hydra-define hydra-new
(:color teal :quit-key "q" title: "New")
("Denote"
(("b" hugo-draft-post "blog post")
("c" org-capture "capture")
("n" denote "note")
("v" list-denotes "view notes")
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
    ("h" hydra-hugo/body))
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
 "z" #'reveal-in-osx-finder)


(general-define-key
:keymaps 'dired-mode-map
 "M-<RET>" #'crux-open-with
)


(provide 'keybindings)


;;; keybindings.el ends here
