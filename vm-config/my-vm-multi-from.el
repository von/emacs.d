;;
;; my-vm-multi-from.el
;;
;; Handle being able to send email from multiple addresses.
;;

;;
;; List of (regex function) to be called when mail is sent from
;; a folder matching regex
;;
(defvar mail-folder-compose-function-alist
  '(
    ("ncsa" setup-ncsa-compose)
    )
  "*Alist of regex, function pairs used by mail-folder-compose-function.
When mail is composed from a folder matched a regex in
mail-folder-compose-function-alist the function associated with that
regex is run on the composition buffer.")

(defun setup-mcs-compose ()
  (mail-set-from "Von Welch <welch@mcs.anl.gov>")
)

(defun setup-vwelch-com-compose ()
  (mail-set-from "Von Welch <von@vwelch.com>")
)

(defun setup-ncsa-compose ()
  (mail-set-from "Von Welch <vwelch@ncsa.uiuc.edu>")
)

(defun mail-folder-compose-function ()
  "Call function specified by mail-folder-compose-function-alist
when we compose email from a specific folders."
  
  (let ((folder (vm-current-folder-name)
		))
    (mapcar
     (function
      (lambda(entry)
	(if (string-match (car entry) folder)
	    (funcall (cadr entry)))
	)
      )
     mail-folder-compose-function-alist
     )
  )
)

(easy-menu-define nil vm-mail-mode-map 
  "Allow me to select my personality for sending email."
  '("From"
    ["vwelch@ncsa.uiuc.edu"
     (mail-set-from "Von Welch <vwelch@ncsa.uiuc.edu>") t]
    ["welch@mcs.anl.gov"
     (setup-mcs-compose) t]
    ["von@vwelch.com"
     (setup-vwelch-com-compose) t]
    ))

(provide 'my-vm-multi-from)
