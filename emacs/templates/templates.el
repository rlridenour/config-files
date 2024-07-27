fundamental-mode

(ddate (format-time-string "%B %e, %Y") " ")
(xtime (current-time-string))
(datelisp "(insert (format-time-string \"%Y%m%d\")))")
(xds (format-time-string "%Y%m%d") "-")
(email (replace-regexp-in-string "@" "@NOSPAM." user-mail-address))
