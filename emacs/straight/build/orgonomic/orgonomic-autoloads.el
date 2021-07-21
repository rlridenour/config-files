;;; orgonomic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "orgonomic" "orgonomic.el" (0 0 0 0))
;;; Generated autoloads from orgonomic.el

(autoload 'orgonomic-return "orgonomic" "\
Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. 

\(fn &optional IGNORE)" t nil)

(autoload 'orgonomic-shift-return "orgonomic" "\
Copy down if in table or insert newline and indent.

\(fn N)" t nil)

(autoload 'orgonomic-delete-backward-char "orgonomic" "\
Delete checkboxes, list item bullets and demote headlines.

\(fn &optional N)" t nil)

(autoload 'orgonomic-minus "orgonomic" "\
Convert a headline to a list item if at the beginning of an empty headline, otherwise insert -.

\(fn N)" t nil)

(autoload 'orgonomic-mode "orgonomic" "\
When active, RET and backspace will behave more like they would in a word
processor.

If called interactively, enable Orgonomic mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orgonomic" '("orgonomic-")))

;;;***

(provide 'orgonomic-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; orgonomic-autoloads.el ends here
