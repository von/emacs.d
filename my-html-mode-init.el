;;
;; my-html-mode-init.el
;;
;; Autoloaded file setup my my-html.el
;; 
;; XXX Why do I have all this crap in here?
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-misc)

(defvar html-global-template-file "~/www/template.html"
  "Location of template file inserted by html-insert-template command.")

(defvar html-template-file "template.html"
  "Name of file in local directory to use as template file.")

(defvar html-global-signature-file "~/www/signature.html"
  "Location of global signature file.")

(defun html-insert-template(&optional template-file)
  "Insert the user's HTML template file.
   Inserts templatefile if given, the user's global
   template file otherwise."

  (let ((title (read-input "Title: "))
	(filename (if (null template-file)
		      html-global-template-file
		    template-file)
		  ))
    (progn (insert-file-contents filename)
	   (replace-string-with-file
	    "@SIGNATURE@"
	    html-global-signature-file)
	   (replace-string-with-string
	    "@TITLE@"
	    title)
	)
    )
)

(defvar html-update-begin "<!-- UPDATE -->")
(defvar html-update-end "<!-- /UPDATE -->")

(defun html-insert-global-signature()
  "Insert the user's global signature."

  (insert-file-contents html-global-signature-file)
)

;; XXX - Doesn't work...
(defun html-popup-menu()
  "Popup html popup menu."

  (interactive)
  (popup-menu html-pulldown-menu)
)

(defun html-update()
  "Update all update strings in an HTML document."

  (interactive)

  (insert-string-between-tags
   html-update-begin
   html-update-end
   (html-update-string))
)

(defun html-insert-update-string()
  "Insert update string at current point."

  (interactive)
  (insert html-update-begin (html-update-string) html-update-end)
)

(defun html-update-string()
  "Returns the current update string."

  (concat (current-time-string) " by " (user-full-name))
)


(defun html-enter()
  "Indent current line and enter."

  (html-indent)
  (insert "\n")
)

(defun my-html-mode-init()
  "My HTML mode init."

  ;; Set tab width to 2
  (setq tab-width 2)

  ;; Add our own HTML menu
  (setq html-pulldown-menu
	'("HTML"
	  ["Insert Template file"
	   (html-insert-template html-template-file)
	   (file-exists-p html-template-file)]
	  ["Insert Global Template file"
	   (html-insert-template) 
	   (file-exists-p html-global-template-file)]
	  ["Insert Global Signature file"
	   (html-insert-global-signature)
	   (file-exists-p html-global-signature-file)]
	  ["Insert Update String"
	   (html-insert-update-string)
	   t]
	  ("Preview Document"
	   ["Netscape"
	    (hm--html-send-buffer-to-netscape
	     (current-buffer))
	    t]
	   ["W3" w3-preview-this-buffer t]
	   ["Xmosaic" html-view-view-buffer t]
	   )
	  ("Special Characters"
	   ["&" (html-ampersand) t]
	   ["<" (html-less-than) t]
	   [">" (html-greater-than) t]
	   )
	  )
	)

  (define-key hm--html-mode-map '(button3) 'html-popup-menu)

  (add-submenu nil html-pulldown-menu)

  ;; Making write-file-hooks buffer-local may already be done...
  (make-variable-buffer-local 'write-file-hooks)
  (add-hook 'write-file-hooks 'html-update)


  ;; XXX - doesn't work...
  (define-key hm--html-mode-map "<" 'html-real-less-than)
  (define-key hm--html-mode-map ">" 'html-real-greater-than)
  (define-key hm--html-mode-map "&" 'html-real-ampersand)

  ;;(define-key hm--html-mode-map "return" 'html-enter)
)


(provide 'my-html-autoload)