;;
;; my-xml.el
;;
;; Everything having to do with XML editing.
;; Uses nxml from http://www.thaiopensource.com/nxml-mode/
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (concat my-lisp-dir directory-sep-string
	      "nxml-mode-20041004" directory-sep-string
	      "rng-auto.el"))

;; From http://www.emacswiki.org/cgi-bin/wiki/NxmlMode
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

(unify-8859-on-decoding-mode)

(setq magic-mode-alist
      (cons '("<�_�_?xml " . nxml-mode)
	    magic-mode-alist))

(fset 'xml-mode 'nxml-mode)

;; Add below if you would like to edit html files in nxml-mode.
(fset 'html-mode 'nxml-mode)

