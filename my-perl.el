;;
;; my-perl.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

;; Use perl mode for perl modules
(setq auto-mode-alist
      (append '(("\\.pm$" . perl-mode)
		)
	      auto-mode-alist))

;; XXX This is breaking on Mac OS 10.4
;; ;; Perl
;; ;; <error> at <file> line <num>
;; (setq perl-compilation-regexp-alist
;;       '(("[^\n]* at \\([^ ]+\\) line \\([0-9]+\\)" 1 2)))

;; ;; XXX should make value local...
;; (cond ((and (boundp 'compilation-error-regexp-alist-alist)
;; 	    (functionp 'compilation-build-compilation-error-regexp-alist))
;;        (setq compilation-error-regexp-alist-alist
;; 	     (cons (cons 'perl perl-compilation-regexp-alist)
;; 			  compilation-error-regexp-alist-alist))
;;        (compilation-build-compilation-error-regexp-alist))

;;       ((boundp 'compilation-error-regexp-alist) ;; xmeacs 19.x
;;        (setq compilation-error-regexp-alist
;; 	     (cons perl-compilation-regexp-alist
;; 		   compilation-error-regexp-alist))
;;        )
;;       )

(defun my-perl-mode-init()
  "My Perl Mode init"

  (make-menubar-local)

  (if (and modify-menu is-xemacs)
      (add-menu-button '("Perl")
		       ["Comment out region" comment-region (region-exists-p)]
		       )
    )

  (make-local-variable 'cperl-indent-level)

  (setq cperl-indent-level 2)
  (setq tab-width 8)

  (if (and modify-menu is-xemacs
	   (string-match mode-name "CPerl"))
      (progn
	(add-submenu '("Perl") '("Identation"
				 ["2" (setq cperl-indent-level 2)
				  :style radio
				  :selected (eq cperl-indent-level 2)]
				 ["4" (setq cperl-indent-level 4)
				  :style radio
				  :selected (eq cperl-indent-level 4)]
				 ["8" (setq cperl-indent-level 8)
				  :style radio
				  :selected (eq cperl-indent-level 8)]
				 )
		     )
	(add-menu-button '("Perl")
			 ["Comment out region" comment-region (region-exists-p)]
			 )
	)
    )

  ;; Set compile command
  (make-local-variable 'compile-command)
  (setq compile-command
	(concat "perl -w -c " buffer-file-name))

  ;; Turn on paren highlighing
  (paren-set-mode 'paren)

  ;; Set up my style
  
  ;; Carriage return after semicolon and braces
  (setq cperl-auto-newline nil)
  (setq cperl-auto-newline-after-colon nil)

  ;; Carriage return before open brace
  (setq cperl-extra-newline-before-brace nil)

  (setq cperl-autoindent-on-semi t)

  (setq cperl-merge-trailing-else t)
  (setq cperl-brace-offset 0)
  (setq cperl-brace-imaginary-offet 0)

  ;; Open braces on same level as previous lines
  (setq cperl-continued-brace-offset (- cperl-indent-level))
)

(add-hook 'perl-mode-hook 'my-perl-mode-init)
(add-hook 'cperl-mode-hook 'my-perl-mode-init)
