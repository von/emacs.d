;;
;; my-java.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

;; Javac
;; <file>:<line no>: <error text>
(setq javac-compilation-regexp-alist
      '(("\\([^ ]+\\):\\([0-9]+\\):" 1 2)))

(cond ((boundp 'compilation-error-regexp-alist-alist)
       (setq compilation-error-regexp-alist-alist
	     (cons (cons 'javac javac-compilation-regexp-alist)
			   compilation-error-regexp-alist-alist))
       (compilation-build-compilation-error-regexp-alist))

      ((boundp 'compilation-error-regexp-alist)
       (setq compilation-error-regexp-alist
	     (cons javac-compilation-regexp-alist
		   compilation-error-regexp-alist))
       )
      )
