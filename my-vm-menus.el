;;
;; my-vm-menus.el
;;
;; $Id$

;; For vm-mail-mode-map
(require 'vm-version)
(require 'vm-vars)
;; For replace-in-string
(require 'vm-rfaddons)

;; No toolbar
(setq-default vm-use-toolbar nil)


(defun my-vm-summary-menu-setup ()
  "Set up main VM menu for summary mode."

  (if modify-menu
      (progn
	(easy-menu-add-item nil '("Send")
			    ["Send queued mail"
			     feedmail-run-the-queue
			     (feedmail-mail-in-queue)])

	(easy-menu-add-item nil  '("Send")
			    '("Forwarding Encapsulation..."
			      ["None" (setq vm-forwarding-digest-type nil)
			       :style radio
			       :selected (eq vm-forwarding-digest-type nil)]
			      ["Mime" (setq vm-forwarding-digest-type "mime")
			       :style radio
			       :selected (string-equal vm-forwarding-digest-type "mime")]
			      ))
	
	(add-submenu '("Send")
		     '("Bandwidth mode"
		       ["Low" (vm-set-bandwidth-mode "low")
			:style radio
			:selected (string-equal "low"
						vm-bandwidth-mode)]
		       ["High" (vm-set-bandwidth-mode "high")
			:style radio
			:selected (string-equal "high"
						vm-bandwidth-mode)]
		       )
		     )

	(add-menu-button '("Folder")
			 ["Expunge IMAP Messages"
			  vm-expunge-imap-messages t]
			 "Expunge POP Messages")
	
	;;(vm-add-maillists-menu)
	)
    )
)

(add-hook 'vm-summary-mode-hook 'my-vm-summary-menu-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Signature inserting stuff
;;

(defun insert-signature (file)
  (let ((mail-signature-file file))
    (mail-signature)))

(defvar my-sig-dir "~/Mail/sigs"
  "Where my signature files are.")

(defvar my-sig-extension ".txt"
  "File extension on signature files.")

(defun generate-insert-signature-menu ()
  "Generate menu for insertion of signatures."

  (cons "Insert-signature..."
	(mapcar
	 (function
	  (lambda(sig-file)
	    (let ((entry (vector
			  (replace-in-string
			   (file-name-nondirectory sig-file)
			   (concat my-sig-extension "$")
			   "")
			  (list 'insert-signature sig-file))
			 ))
	      entry
	      )
	    )
	  )

	 (directory-files my-sig-dir
			  ;; Full path
			  t
			  ;; Don't match files starting with "."
			  ;; (including directories)
			  ;; Don't match files ending with "~"
			  "^[^.].*[^~]$"
			  ;; Sort
			  t)
	 )
	)
  )

(easy-menu-define nil vm-mail-mode-map
  "Allow insertion of a signature"
  (generate-insert-signature-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-menus)
