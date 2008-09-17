;;
;; my-vm.el
;;
;; $Id$

(if
 my-use-vm
 (progn
   ;; Load all the vm-config/my-*.el files in my config dir
   (setq my-vm-config-dir (concat my-emacs-config-dir
				  directory-sep-string
				  "vm-config"
				  directory-sep-string))

   (mapcar
    (function
     (lambda (file)
       (load-file file)
       ))
    (directory-files my-vm-config-dir t "^my-.*\.el$" nil)
    )
   
   ;; Need to load this so variables will be present for menus
   (require 'smtpmail)
   
   ;; Don't prompt me for my email address, it should be set
   (setq-default query-user-mail-address nil)
   
   ;; Select browser to use for urls
   (cond
    (is-ms-windows
     (setq vm-url-browser "c:\\Program Files\\mozilla.org\\Mozilla\\mozilla.exe"))
    
    (is-darwin
     ;; Mac OS X - use open command
     (setq vm-url-browser "open"))
    )
   
   ;; Move to next message after deleting or killing
   (setq-default vm-move-after-deleting t)
   (setq-default vm-move-after-killing t)
   
   ;; Prefix to ignore on message subjects when comparing
   ;; Modified to ignore "[list]" - doesn't work yet
   ;;(setq vm-subject-ignored-prefix "^\\(re: *\\)+\\(\\\\[.*\\\\])?\\(re: *\\)+")
   ;;(setq vm-subject-ignored-prefix "^\\(re: *\\)+")
   
   ;;----------------------------------------
   ;;
   ;; BBDB support
   ;;
   
   ;; Add bbdb support
   (bbdb-insinuate-vm)

   ;; Don't auto create enteries for every person I get email from
   (setq-default bbdb/mail-auto-create-p nil)
   
   ;; End BBDB
   ;;
   ;;----------------------------------------
   
   ;; Set up automatic spool files
   (setq-default vm-spool-file-suffixes '(".spool"))
   (setq-default vm-crash-box-suffix ".crash")
   
   ;; Don't delete empty folders
   (setq-default vm-default-empty-folders nil)
   
   ;; Ask before creating new folders
   (setq-default vm-confirm-new-folders t)
   
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

		 ("Marked" (("mbox") (marked)))
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
		  ;; Don't cover things in spam folder since they
		  ;; results in the folder being read then changed
		  ;; on disk when new spam comes in
		  (("mbox")
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
		 ("Personal"
		  (("mbox")
		   (recipient "vwelch.com")
		   ))
		 ("GridShib"
		  (("gridshib")
		   (header "Sender: owner-gridshib@ncsa.uiuc.edu")
		   ))
		 )
		 )

   ;;
   ;; Set frame sizes.
   ;;
   ;; Make composition frames the same size as my xterms and folder frames
   ;; the height of the screen
   (setq-default vm-frame-parameter-alist '(
					    ;; Reply and compose frames
   (composition ((height . 26)))
   (composition ((edit . 26)))
   (completion ((height . 26)))
   ;; visiting folder frames
   (folder ((height . 52)))
   )
		 )

   ;; Don't look for existing frames
   (setq-default vm-search-other-frames nil)



   (defun my-vm-add-virtual-key-bindings ()
     "Add keybindings to virtual folder creation functions."

     (local-set-key "VN" 'vm-create-virtual-folder-unread)
     )

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

   ;;----------------------------------------
   ;;
   ;; Mailcrypt
   ;;
  
   (add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
   (add-hook 'vm-mode-hook 'mc-install-read-mode)
   (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
   (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
   )
)