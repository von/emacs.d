;;
;; my-remote-fs.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ********************
;;; Remote filesystem support
;;;
;;; Under 19.x use ange-ftp, under 20.x use efs.
;;; In any case the pathname syntax is /user@host:/remote/path
;;;
(require 'dired)

(cond ((= emacs-major-version 19)
       (require 'ange-ftp)
       (setq 
	;; id to use for /host:/remote/path
	ange-ftp-default-user "anonymous"      
	
	;; use $USER@`hostname`
	ange-ftp-generate-anonymous-password t

	;; always transfer in binary mode
	ange-ftp-binary-file-name-regexp "."
	)

       (if vons-windows
	   (setq ange-ftp-tmp-name-template (concat
					     (expand-file-name
					      (getenv "TEMP")) "/ange-ftp")
	   ange-ftp-gateway-tmp-name-template (concat
					       (expand-file-name
						(getenv "TEMP")) "/ange-ftp")
	   )
	 )
       )
      ((>= emacs-major-version 20)
       (setq
	;; Default user name
	efs-default-user "vwelch"
	)
       )
      )
