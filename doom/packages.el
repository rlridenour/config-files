;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! hungry-delete)
;; (package! wc-mode)
(package! major-mode-hydra)
(package! shrink-whitespace)
(package! deadgrep)
(package! orgonomic
  :recipe (:host github :repo "aaronjensen/emacs-orgonomic"))
;; (package! org-roam-bibtex
  ;; :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! dired-narrow)
;; (package! grab-mac-link
  ;; :recipe (:host github :repo "xuchunyang/grab-mac-link.el"))
(package! doom-snippets :ignore t)
;; (package! aggressive-indent)
(package! crux)
;; (package! org-contrib
;;   :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"))
(package! ebib)
(package! ox-hugo)
(package! unfill)
(package! key-chord)
(package! dired+)
(package! pulsar)
(package! math-delimiters
  :recipe (:host github :repo "oantolin/math-delimiters" :branch "master"))
(package! latex-change-env)
(package! move-text)
;; (unpin! dirvish)
(package! evil-nerd-commenter)
(package! denote)
(package! denote-menu
  :recipe (:host github :repo "namilus/denote-menu"))
(package! consult-notes)
(package! dired-subtree)
(package! fish-mode)
(package! dwim-shell-command)
(package! mixed-pitch)
(package! org-super-agenda)
(package! dashboard)
(package! citar-denote)
;; (package! hyperbole)
(package! vundo)
(package! deadgrep)
(package! yankpad)
(package! corfu)
(package! cape)
;; (package! ekg)
;; (package! emacsql)
(package! titlecase)
(package! tempel)
(unpin! compat)
(package! aas)
