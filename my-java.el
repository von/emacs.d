;;
;; my-java.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

(defun my-java-mode-init()
  "My Java mode init"

  (make-local-variable 'tab-width)
  (setq

   ;; Make tabs always 4
   tab-width 4

   ;; Don't use tabs for indentation
   indent-tabs-mode nil
   )
)

(add-hook 'java-mode-hook 'my-java-mode-init)

;; XXX MAC OS X 10.4 issues
;; ;; Javac
;; ;; <file>:<line no>: <error text>
;; (setq javac-compilation-regexp-alist
;;       '(("\\([^ ]+\\):\\([0-9]+\\):" 1 2)))

;; (cond ((and (boundp 'compilation-error-regexp-alist-alist)
;; 	    (functionp 'compilation-build-compliation-error-regexp-alist))
;;        (setq compilation-error-regexp-alist-alist
;; 	     (cons (cons 'javac javac-compilation-regexp-alist)
;; 			   compilation-error-regexp-alist-alist))
;;        (compilation-build-compilation-error-regexp-alist))

;;       ((boundp 'compilation-error-regexp-alist)
;;        (setq compilation-error-regexp-alist
;; 	     (cons javac-compilation-regexp-alist
;; 		   compilation-error-regexp-alist))
;;        )
;;       )
