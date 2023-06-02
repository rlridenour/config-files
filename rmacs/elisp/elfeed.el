;;; elfeed.el -*- lexical-binding: t; -*-

(use-package elfeed-org
:config
(elfeed-org)
(setq rmh-elfeed-org-files (list "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/elfeed/elfeed.org")))

(setq elfeed-db-directory "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/elfeed/elfeeddb")




;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
"Wrapper to load the elfeed db from disk before opening"
(interactive)
(elfeed-db-load)
(elfeed)
(elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
"Wrapper to save the elfeed db to disk before burying buffer"
(interactive)
(elfeed-db-save)
(quit-window))




(use-package elfeed
:ensure t
:bind (:map elfeed-search-mode-map
("q" . bjm/elfeed-save-db-and-bury)
)
)

(use-package elfeed-goodies
:ensure t
:config
(elfeed-goodies/setup))



;;; elfeed.el ends here
