;;
;; my-vm.el
;;
;; $Id$

(require 'my-vm-funcs)
(require 'my-vm-mime)
(require 'my-vm-conf)
(require 'my-vm-getting-email)
(require 'my-vm-sending-email)
(require 'my-vm-reading-email)
(require 'my-vm-summary)
(require 'my-vm-multi-from)
(require 'vm-rfaddons)

;; Need to load this so variables will be present for menus
(require 'smtpmail)

;; Don't prompt me for my email address, it should be set
(setq-default query-user-mail-address nil)

;; No toolbar
(setq-default vm-use-toolbar nil)

;; Select browser to use for urls
(if is-ms-windows
    ;;(setq vm-url-browser "c:\\Program Files\\Netscape\\Netscape\\netscp.exe")
    ;;(setq vm-url-browser "c:\\Program Files\\Internet Explorer\\IEXPLORE.exe")
    (setq vm-url-browser "c:\\Program Files\\mozilla.org\\Mozilla\\mozilla.exe")
  )

;; Mac OS X - use open command
(setq vm-url-browser "open")


;; Move to next message after deleting or killing
(setq-default vm-move-after-deleting t)
(setq-default vm-move-after-killing t)

;; Prefix to ignore on message subjects when comparing
;; Modified to ignore "[list]" - doesn't work yet
;;(setq vm-subject-ignored-prefix "^\\(re: *\\)+\\(\\\\[.*\\\\])?\\(re: *\\)+")
;;(setq vm-subject-ignored-prefix "^\\(re: *\\)+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BBDB support
;;

;; Add bbdb support
(bbdb-insinuate-vm)

;; Don't auto create enteries for every person I get email from
(setq-default bbdb/mail-auto-create-p nil)

;; End BBDB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up automatic spool files
(setq-default vm-spool-file-suffixes '(".spool"))
(setq-default vm-crash-box-suffix ".crash")

;; Don't delete empty folders
(setq-default vm-default-empty-folders nil)

;; Set up points to my mail box and folders
(setq-default vm-folder-directory "~/Mail/")

;; Save a copy of all outgoing email
(setq-default mail-archive-file-name (expand-file-name
				      (concat vm-folder-directory
					      "outbox")))

;; Where my aliases are
(setq-default mail-abbrev-mailrc-file "~/Mail/mail_aliases")

;; Automatically delete messages after saving or archiving
(setq-default vm-delete-after-archiving t)
(setq-default vm-delete-after-saving t)

;; Set up default save locations
(setq-default vm-auto-folder-alist
	      '(
		("Sender:"
		 ("owner-ogsi-wg@gridforum.org" . "ggf/ogsi")
		 ("owner-wu-ftpd@wugate.wustl.edu" . "technologies/wuftpd")
		 ;; Seems like can only use '*' in next regex and not '?'
		 ("owner-xemacs\(-announce\)*@xemacs.org" . "technologies/xemacs")
		 )
		
		;; HPCWire and DSSTAR
		("^From:"
		 ("hpcnews@tgc.com" . "misc/hpc-wire")
		 ("hpcwire@tgc.com" . "misc/hpc-wire")
		 ("ds-star@tgc.com" . "misc/dsstar")
		 )

		("^To:"
		 ("crypto-gram@chaparraltree.com" . "security/crypto-gram")
		 ("nev@bostic.com" . "misc/nev-dull")
		 )
		)
	      )


;; Set up virtual folders
;; XXX should set up inboxes variable
(setq-default vm-virtual-folder-alist
	      '(
		("New and Unread" (("mbox")
				   (and (not (deleted))
					(or (unread) (new)))))

		("New and Unread and to Me"
		 (("mbox")
		  (and (not (deleted))
		       (or (unread) (new))
		       (recipient "welch@mcs.anl.gov")
		       )))


		("New" (("mbox") (and (not (deleted))
				      (new))))

		("Undeleted" (("mbox") (not (deleted))))

		("From Steve"
		 (("mbox") (author "tuecke@mcs.anl.gov")))
		("From Ian"
		 (("mbox") (author "foster@mcs.anl.gov")))
		("From Frank"
		 (("mbox") (author "franks@mcs.anl.gov")))
		("From Me"
		 (("mbox") (author "welch@mcs.anl.gov")))

		("To be printed"
		 (("mbox") (label "print")))
		("To be read"
		 (("mbox") (label "read")))

		("SPAM"
		 (("mbox" "spam")
		  (or
		   (header "X-Spam-Flag: YES")
		   (header "X-NCSA-MailScanner: Found to be infected")
		   )))
		("News"
		 (("mbox")
		  (or
		   (header "Sender: owner-ip@v2.listbox.com")
		   (header "From: Grid Today <grid@gridtoday.com>")
		   )))
		("Cryptography"
		 (("mbox") (header "Sender: owner-cryptography@metzdowd.com")))
		("Globus Misc"
		 (("mbox")
		  (or
		   (header "Sender: owner-discuss@globus.org")
		   (header "Sender: owner-mpich-g@globus.org")
		   (header "Sender: owner-developer-discuss@globus.org")
		   (header "Sender: owner-java@globus.org")
		   )))
		("Globus Board"
		 (("mbox")
		  (header "Sender: owner-board@globus.org")))
		("GGF Misc"
		 (("mbox")
		  (or (header "Sender: owner-policy-wg@gridforum.org")
		      (header "Sender: ogsa-wg@ggf.org")
		      )))
		("GGF Security"
		 (("mbox")
		  (or
		   (header "Sender: owner-authz-wg@gridforum.org")
		   (header "Sender: owner-ogsa-sec-wg@gridforum.org")
		   (header "Sender: owner-ogsa-authz-wg@gridforum.org")
		   )))
		("IETF Lists"
		 (("mbox")
		  (or
		   (header "Sender: owner-ietf-pkix@mail.imc.org")
		   (header "Sender: owner-ietf-krb-wg@achilles.ctd.anl.gov")
		   (header "Sender: cfrg-admin@ietf.org")
		   (header "Sender: owner-ietf-cat-wg@lists.Stanford.EDU")
		   )
		  )
		 )
		("OASIS Lists"
		 (("mbox")
		  (or
		   (header "Delivered-To: mailing list security-services@lists.oasis-open.org")
		   (header "Delivered-To: mailing list xacml@lists.oasis-open.org")
		   (header "Delivered-To: mailing list wss@lists.oasis-open.org")
		   (header "Delivered-To: mailing list announce@lists.oasis-open.org")
		   (header "Delivered-To: mailing list pki-tc@lists.oasis-open.org")
		  )
		  ))
		("Internet2"
		 (("mbox")
		  (or
		   (header "List-Id: <shibboleth-dev.internet2.edu>")
		   (header "Sender: owner-mace@internet2.edu")
		   (header "List-Id: <mace.internet2.edu>")
		   (header "List-Id: <hepki-tag.internet2.edu>")
		   (header "List-Id: <mace-opensaml-users.internet2.edu>")
		  )))
		("NCSA Security"
		 (("ncsa")
		  (header "Sender: owner-security@ncsa.uiuc.edu")
		  ))
		("NCSA Security Monitoring"
		 (("ncsa-sec-monitoring")
		  (header "Sender: owner-security@ncsa.uiuc.edu")
		  ))
		("Mail Cruft"
		 (("mbox")
		  (or
		   (header "Subject: BOUNCE")
		   (header "Subject: SUBSCRIBE")
		   (header "Subject: UNSUBSCRIBE")
		   )))
		("Misc Reading"
		 (("mbox")
		  (or
		   (header "Sender: owner-ip@v2.listbox.com")
		   )))
		)
	      )

;;
;; Set frame sizes.
;;
;; Make composition frames the same size as my xterms
(setq-default vm-frame-parameter-alist '(
					 ;; Reply and compose frames
					 (composition ((height . 26)))
					 (composition ((edit . 26)))
					 (completion ((height . 26)))
					 ;; visiting folder frames
					 (folder ((height . 59)))
					 )
)

;; Don't look for existing frames
(setq-default vm-search-other-frames nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks
;;

;; Called for composition buffers
(defun my-vm-mail-mode-hook ()
  "my vm-mail-mode hook."
  (easy-menu-remove '("Lisp-Interaction"))
  (mail-folder-compose-function)
  (set-buffer-frame-title-format (concat "Compose: " (buffer-name)))
  ;; Add cc field automatically for me if not already there
  (save-excursion
   (or (mail-position-on-field "CC" t)
       (mail-set-header "CC" "")))
  ;; Load all my aliases
  (my-rebuild-mail-aliases)
)

;;(add-hook 'vm-mail-mode-hook 'my-vm-mail-mode-hook)
(add-hook 'mail-setup-hook 'my-vm-mail-mode-hook)

(defun my-vm-menu-setup-hook ()
  "My vm-menu-setup-hook"

  (if modify-menu
      (progn
;  (add-submenu '("Send")
;	       '("Queuing..."
;		 ["Send directly" (setq smtpmail-queue-mail nil)
;		  :style radio
;		  :selected (not smtpmail-queue-mail)]
;		 ["Queue mail" (setq smtpmail-queue-mail t)
;		  :style radio
;		  :selected smtpmail-queue-mail]
;		 ))

;  (add-menu-button '("Send")
;		 ["Send queued mail"
;		  smtpmail-send-queued-mail (smtpmail-queued)])

	(add-menu-button '("Send")
			 ["Send queued mail"
			  feedmail-run-the-queue (feedmail-mail-in-queue)])

	(add-submenu '("Send")
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

(defun my-vm-add-virtual-key-bindings ()
  "Add keybindings to virtual folder creation functions."

  (local-set-key "VN" 'vm-create-virtual-folder-unread)
)

(add-hook 'vm-menu-setup-hook 'my-vm-menu-setup-hook)

(defun vm-set-frame-title ()
  "Set the title of the current VM buffer"

  (set-buffer-frame-title-format (concat "VM: " (vm-current-folder-name)))
)

(defun vm-add-edit-key-bindings ()
  "Add key binds for editing a mail message."

  (local-set-key [(control tab)] 'bbdb-complete-name)
)

(add-hook 'vm-summary-mode-hook 'vm-set-frame-title)
(add-hook 'vm-summary-mode-hook 'my-vm-add-virtual-key-bindings)
(add-hook 'vm-mode-hook 'vm-set-frame-title)
(add-hook 'vm-mail-mode-hook 'vm-add-edit-key-bindings)
(add-hook 'vm-presentation-mode-hook 'vm-set-frame-title)
(add-hook 'mail-mode-hook 'mail-abbrevs-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mailcrypt
;;

(defun my-vm-mc-install-read-mode ()
  "Install mc-read-mode in mc and preserve vm popup menu"

  ;; Install MC stuff and then rebind mouse-3 to vm menu
  (mc-install-read-mode)
  (setq mode-popup-menu vm-menu-dispose-menu)
)
  
(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
(add-hook 'vm-mode-hook 'my-vm-mc-install-read-mode)
(add-hook 'vm-summary-mode-hook 'my-vm-mc-install-read-mode)
(add-hook 'vm-virtual-mode-hook 'my-vm-mc-install-read-mode)

