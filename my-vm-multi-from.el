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
  (mail-set-header "FCC" "~/Mail/mcs/outbox")
)

(defun setup-vwelch-com-compose ()
  (mail-set-from "Von Welch <von@vwelch.com>")
  (mail-set-header "FCC" "~/Mail/vwelch.com/outbox")
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

(defun mail-add-from-menu ()
  "Add my From menu to the Mail menu. Intended for compose mode."

  (add-submenu '("Mail")
	       '("From"
		 ["vwelch@ncsa.uiuc.edu"
		  (mail-set-from "Von Welch <vwelch@ncsa.uiuc.edu>") t]
		 ["welch@mcs.anl.gov"
		  (setup-mcs-compose) t]
		 ["von@vwelch.com"
		  (setup-vwelch-com-compose) t]
		 ))
  )


(provide 'my-vm-multi-from)
