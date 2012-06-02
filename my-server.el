;;;
;;; Server configuration and launch
;;;

(if (fboundp 'server-start)
    ;; GNU Emacs
    (server-start)
  ;; XEmacs: use gnuserv-start and gnudoit
  (progn
    (require 'gnuserv)
    (if (fboundp 'gnuserv-start)
	(gnuserv-start)))
)
