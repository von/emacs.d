;;
;; my-vm-mime.el
;;
;; My mime configuration for vm.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mime Configuration
;;

;; Added some extra mime types
(setq-default vm-mime-attachment-auto-type-alist
	      '(
		("\\.txt$" . "text/plain")
		("\\.[ch]$" . "text/plain")
		("\\.patch$" . "text/plain")
		("\\.pem$" . "text/plain")
		("\\.tex$" . "text/plain")
		("\\.html$" . "text/html")
		("\\.htm$" . "text/html")
		("\\.wsdl$" . "text/wsdl")
		("\\.doc$" . "application/msword")
		("\\.xls$" . "application/x-msexcel")
		("\\.pdf$" . "application/pdf")
		("\\.ppt$" . "application/x-mspowerpoint")
		("\\.rtf$" . "application/richtext")
		("\\.jpg$" . "image/jpeg")
		("\\.ps$" . "application/postscript")
		("\\.gif$" . "image/gif")
		("\\.bmp$" . "image/bitmap")
		("\\.zip$" . "application/zip")
		("\\.pgp$" . "text/pgp-encrypted")
		("\\.graffle$" . "application/graffle")
		;; Following is a bad guess
		("\\.wav$" . "application/sound")
		))

;; Guess mime's type using filename and vm-mime-attachment-auto-type-alist
(setq-default vm-infer-mime-types t)

;; Don't decode messages when previewing them
(setq-default vm-mime-decode-for-preview nil)

(setq vm-mime-type-converter-alist
      '(
	("text/html" "text/plain" "html2text -nobs")
	))
      
;; Don't display these mime types myself
(setq-default vm-mime-internal-content-type-exceptions
	      '(
		"text/html"
		;;"image/jpeg"
		;;"image/gif"
		)
	      )

;; Don't display these attachments automatically
(setq-default vm-auto-displayed-mime-content-type-exceptions
	      '(
		;;"text/html"
		"image"
		)
	      )

;; Display plain text first if we have a choice...
(setq-default vm-mime-alternative-select-method
	      '(favorite
		"text/plain"
		))

;; Handle attachments
(setq-default vm-mime-external-content-types-alist 
      '(
	;; Use OpenURL() instead of OpenFile so we can specifiy new-window
	;;("text/html" 	                  "netscape" "-noraise" "-remote" "openURL(file:%f, new-window)")
	;;("image/gif" 	                  "netscape" "-noraise" "-remote" "openURL(file:%f, new-window)")
	;;("image/jpeg" 	                  "netscape" "-noraise" "-remote" "openURL(file:%f, new-window)")
	;;("image/gif" 	                  "xview")
	;;("image/jpeg"	                  "xview")

	;; Windows:
	;; Use mime_display for all applications
	;; Windows workaround: exec explicitly with perl or the
	;;                     script doesn't get it's arguments
	;;
	;; Mac: Use open for everything
	;;
	;; Use explicit arg to force correct parsing of attachments
	;; with spaces.
	("application"       "open" "%f")
	("video"             "open" "%f")
	("image"             "open" "%f")
	("text/plain"        "open" "%f")
	)
      )

;; Display all charsets using the default face
(setq-default vm-mime-default-face-charsets t)

;; Don't display these charsets using the default face
;;(setq-default vm-mime-default-face-charset-exceptions '())
;;
;; Use vm-mime-charset-font-alist to set fonts for charsets

;; Don't kill viewer when I leave the message
(setq-default vm-mime-delete-viewer-processes nil)

;; Format of buttons for attachments in email
(setq-default vm-mime-button-format-alist
	      '(("text" . "%-60.60(%d: %f%)")
		("multipart/alternative" . "%-60.60(%d: %f%)")
		("multipart/digest" .  "%-60.60(%d: %f%)")
		("multipart" . "%-60.60(%d, %n part%s%)")
		("message/partial" . "%-60.60(%d, part %N (of %T)%)")
		("message/external-body" . "%-60.60(%d/%x%)")
		("message" . "%-60.60(%d: %f%)")
		("audio" . "%-60.60(%d: %f%)")
		("video" . "%-60.60(%d: %f%)")
		("image" . "%-60.60(%d: %f%)")
		("application/octet-stream" . "%-60.60(%d: %f%)")
		("application/pdf" . "%-60.60(%d: %f%)")
		("application/postscript" . "%-60.60(%d: %f%)")
		("application/ppt" . "%-60.60(%d: %f%)")
		("application/msword" . "%-60.60(%d: %f%)")
		("application/x-gzip" . "%-60.60(%d: %f%)")
		("application/x-mspowerpoint" . "%-60.60(%d: %f%)")
		))

;;
;; Use binary to mime encoding
;(setq vm-mime-base64-decoder-program "mmencode")
;(setq vm-mime-base64-decoder-switches '("-b" "-u"))
;(setq vm-mime-base64-encoder-program "mmencode")
;(setq vm-mime-base64-encoder-switches '("-b"))

;;
;; End Mime Configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-mime)