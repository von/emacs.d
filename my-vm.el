;;
;; my-vm.el
;;
;; $Id$

(require 'my-vm-funcs)
(require 'my-vm-mime)
(require 'my-vm-conf)
(require 'my-vm-getting-email)
(require 'my-vm-sending-email)

;; Need to load this so variables will be present for menus
(require 'smtpmail)

;; Don't prompt me for my email address, it should be set
(setq-default query-user-mail-address nil)

;; No toolbar
(setq-default vm-use-toolbar nil)

;; Turn threading off by default
(setq-default vm-summary-show-threads nil)

;; Set summary format (should have CR in string)
(setq-default vm-summary-format "%4n %2M/%02d %*%a %-17.17F %I\"%s\" (%c)
")

;; Don't use subject for threading
(setq-default vm-thread-using-subject nil)

;; Use netscape to display urls, with a new window for each
;; And tell netscape not to raise itself
(setq vm-url-browser 'vm-mouse-send-url-to-netscape-new-window)
(setq vm-netscape-program-switches '("-noraise"))

;; Don't automatically go to new mail messages
(setq-default vm-jump-to-new-messages nil)
(setq-default vm-jump-to-unread-messages nil)

;; Show me messages but don't mark them as read until I say so
(setq-default vm-preview-lines 0)

;; Move to next message after deleting or killing
(setq-default vm-move-after-deleting t)
(setq-default vm-move-after-killing t)

;; Don't take me to the next message when I hit space at the end
;; of the current one
(setq-default vm-auto-next-message nil)

;; Wrap long lings
;; XXX This is broken
;; (setq vm-fill-paragraphs-containing-long-lines nil)

;; Auto-center summary?
(setq-default vm-auto-center-summary nil)

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
(setq-default vm-primary-inbox "~/Mail/mbox")
(setq-default vm-folder-directory "~/Mail/")

;; Save a copy of all outgoing email
(setq-default mail-archive-file-name (expand-file-name
				      (concat vm-folder-directory
					      "outbox")))

;; Where my aliases are
(setq-default mail-abbrev-mailrc-file "~/Mail/mail_aliases")

;; Load all my aliases
(my-rebuild-mail-aliases)

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
(setq-default vm-virtual-folder-alist
	      '(
		("New and Unread" (("mbox")
				   (and (not (deleted))
					(or (unread) (new)))))

		("New" (("mbox") (and (not (deleted))
				      (new))))

		("Undeleted" (("mbox") (not (deleted))))

		("From Steve"
		 (("mbox") (author "tuecke@mcs.anl.gov")))
		("From Ian"
		 (("mbox") (author "foster@mcs.anl.gov")))

		("SPAM"
		 (("mbox") (header "X-Spam-Flag: YES")))
		)
)

;;
;; Preload the highlight-headers package and fix up the fonts.
;; This is a hack, but I can't figure out any other functional
;; way to get the faces consistantly right.
(require 'highlight-headers)

(make-face-unbold 'message-headers)
(set-face-foreground 'message-headers "green")

(make-face-unbold 'message-header-contents)
(copy-face 'default 'message-header-contents)

(make-face-bold 'message-cited-text)
(set-face-foreground 'message-cited-text "yellow")

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

(defun my-vm-mail-mode-hook ()
  "my vm-mail-mode hook."
  (easy-menu-remove '("Lisp-Interaction"))
  (mail-add-from-menu)
  (mail-folder-compose-function)
  (mail-add-insert-signature-menu)
  (set-buffer-frame-title-format (concat "Compose: " (buffer-name)))
  ;; Add cc field automatically for me if not already there
  (save-excursion
   (or (mail-position-on-field "CC" t)
       (mail-set-header "CC" "")))
)

;;(add-hook 'vm-mail-mode-hook 'my-vm-mail-mode-hook)
(add-hook 'mail-setup-hook 'my-vm-mail-mode-hook)

(defun my-vm-menu-setup-hook ()
  "My vm-menu-setup-hook"

  (add-submenu '("Send")
	       '("Queuing..."
		 ["Send directly" (setq smtpmail-queue-mail nil)
		  :style radio
		  :selected (not smtpmail-queue-mail)]
		 ["Queue mail" (setq smtpmail-queue-mail t)
		  :style radio
		  :selected smtpmail-queue-mail]
		 ))

  ;; Nice to find a test here to see if I have queued mail
  (add-menu-button '("Send")
		 ["Send queued mail"
		  smtpmail-send-queued-mail (smtpmail-queued)])

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
		  
  ;;(vm-add-maillists-menu)
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
(add-hook 'vm-mode-hook 'vm-add-edit-key-bindings)
(add-hook 'vm-presentation-mode-hook 'vm-set-frame-title)

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

(defun mail-add-insert-signature-menu ()
  "Add an signature insertion menu to the Mail menu.
Intended for compose mode."

  (let ((menu (generate-insert-signature-menu)))
    (add-submenu '("Mail")
		   menu
		   )
    )
)

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
	 (directory-files my-sig-dir t nil t t)
	 )
	)
  )

;; Don't auto fill header lines
;; XXX Really belongs elsewhere
;;(setq-default auto-fill-inhibit-regexp "^\\(To:\\|Cc:\\|Subject:\\)")

(setq vm-url-browser "c:\\Program Files\\Netscape\\Netscape 6\\netscp6.exe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; vm-summary-hilit
;;

(require `vm-summary-hilit)

(setq vm-summary-hilit-alist
      '(
	("From:"
	 ("welch@mcs.anl.gov" . "grey")
	 )
	("Sender:"
	 ;; Misc lists
	 ("esg-admin@earthsystemgrid.org" . "DarkRed")
	 ("cs-admin@cs.uchicago.edu" . "DarkRed")
	 ("ppdg-admin@ppdg.net" . "DarkRed")
	 ("owner-seminars@mcs.anl.gov" . "DarkRed")
	 ("owner-disc@mcs.anl.gov" . "DarkRed")
	 ("owner-team@grids-center.org" . "DarkRed")
	 ("owner-ietf-krb-wg@achilles.ctd.anl.gov" . "DarkRed")
	 ("owner-mace@internet2.edu" . "DarkRed")
	 ("owner-hepki-tag@internet2.edu" . "DarkRed")
	 ("owner-mcs@mcs.anl.gov" . "DarkRed")
	 ("owner-wu-ftpd@wugate.wustl.edu" . "DarkRed")
	 ("owner-ietf-pkix@mail.imc.org" . "DarkRed")
	 ("owner-ietf-sacred@mail.imc.org" . "DarkRed")
	 ("cfrg-admin@ietf.org" . "DarkRed")
	 ("owner-access-online@ncsa.uiuc.edu" . "DarkRed")
	 ;; High-interest misc lists
	 ("owner-announce@cs.uiuc.edu" . "red")
	 ("owner-dsl@mcs.anl.gov" . "red")
	 ("owner-dsl-core@mcs.anl.gov" . "red")
	 ("owner-dsl-uc@mcs.anl.gov" . "red")
	 ("dsl-admin@cs.uchicago.edu" . "red")
	 ("ppdg-siteaa-admin@ppdg.net" . "red")
	 ("ppdg-cara-admin@ppdg.net" . "red")
	 ("owner-doe-sg@george.lbl.gov" . "red")
	 ;; Globus lists
	 ("owner-discuss@globus.org" . "DarkBlue")
	 ("owner-java@globus.org" . "DarkBlue")
	 ("owner-cray@globus.org" . "DarkBlue")
	 ("owner-mpich-g@globus.org" . "DarkBlue")
	 ("owner-webservices-internal@globus.org" . "DarkBlue")
	 ("owner-windows@globus.org" . "DarkBlue")
	 ("owner-mds@globus.org" . "DarkBlue")
	 ;; High-interest Globus lists
	 ("owner-python-discuss@globus.org" . "blue")
	 ("owner-developers@globus.org" . "blue")
	 ("owner-developer-discuss@globus.org" . "blue")
	 ("owner-ogsa-alpha@globus.org" . "blue")
	 ("owner-ogsa-developers@globus.org" . "blue")
	 ("owner-gt3-developers-internal@globus.org" . "blue")
	 ("owner-gsi-openssh@globus.org" . "blue")
	 ;; Management lists
	 ("owner-dsl-management@mcs.anl.gov" . "orange")
	 ("owner-ogsa-management@globus.org" . "orange")
	 ("owner-globus-ogsa-management@globus.org" . "orange")
	 ;; GGF
	 ("owner-security-wg@gridforum.org" . "green")
	 ("owner-ogsi-wg@gridforum.org" . "green")
	 ("owner-ogsa-security@globus.org" . "green")
	 ("gridforum.org" . "DarkGreen")
	 ("ggf-testscripts-admin@cs.uchicago.edu" . "DarkGreen")
	 ;; Essentially me
	 ("owner-security-internal@globus.org" . "white")
	 )
	("To:\\|Cc:\\cc:"
	 ("lists.oasis-open.org" . "DarkRed")
	 ("doe-sg-pma@george.lbl.gov" . "red")
	 ("doe-sg-ca@george.lbl.gov" . "red")
	 ("welch@mcs.anl.gov" . "white")
	 )
	)
      )

;; Calling this function modifies the above list
(vm-make-summary-hilit-alist-faces)


