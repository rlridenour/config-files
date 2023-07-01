;;; finish.el -*- lexical-binding: t; -*-

(setq default-directory "~/")


;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

(setq gc-cons-threshold (* 2 1000 1000))


(provide 'finish)


;;; finish.el ends here
