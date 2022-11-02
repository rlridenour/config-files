;; -*- mode: lisp -*-
;; see: https://github.com/minad/tempel#template-syntax
;; see: https://www.emacswiki.org/emacs/tempo-c-cpp.el
;; see: https://www.emacswiki.org/emacs/tempo-c-cpp.el
;;
;; "string"                      Inserts a string literal.
;; p                             Inserts an unnamed placeholder field.
;; n                             Inserts a newline.
;; >                             Indents with indent-according-to-mode.
;; r                             Inserts the current region.
;; r>                            The region, but indented.
;; n>                            Inserts a newline and indents.
;; &                             Insert newline if there is only whitespace between line start and point.
;; %                             Insert newline if there is only whitespace between point and line end.
;; o                             Like % but leaves the point before newline.
;; (s NAME)                      Inserts a named field.
;; (p PROMPT <NAME> <NONINS>)    Insert an optionally named field with a prompt.
;;                               The PROMPT is displayed directly in the buffer
;;                               as default value. If NOINSERT is non-nil, no
;;                               field is inserted. Then the minibuffer is used
;;                               for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>)  Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;; (p FORM <NAME> <NONINS>)      Like p described above, but FORM is evaluated.
;; (FORM ...)                    Other Lisp forms are evaluated. Named fields are lexically bound.

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(ydate (format-time-string "%Y%m%d"))
(ddate (format-time-string "%B %e, %Y"))
(xds (format-time-string "%Y%m%d"))
(xtime (current-time-string))
