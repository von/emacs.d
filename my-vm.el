;;
;; my-vm.el
;;
;; $Id$

(require 'my-vm-funcs)
(require 'my-vm-mime)
(require 'my-vm-conf)
(require 'my-vm-getting-email)
(require 'my-vm-sending-email)

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

;; Automatically delete messages after saving or archiving
(setq-default vm-delete-after-archiving t)
(setq-default vm-delete-after-saving t)

;; Set up default save locations
(setq-default vm-auto-folder-alist
	      '(
		;; For sorting mbox
		("^Subject:"
		 ("myproxy" . "projects/myproxy")
		 )

		;; Personal
		("^From:"
		 ("Kent" . "personal/kent")
		 ("jms@cs.nwu.edu" . "personal/jms")
		 ;;("jms@mcs.anl.gov" . "personal/jms")
		 ("smiths321@yahoo.com" . "personal/sue")
		 )

		;; HPCWire and DSSTAR
		("^From:"
		 ("hpcnews@tgc.com" . "misc/hpc-wire")
		 ("hpcwire@tgc.com" . "misc/hpc-wire")
		 ("ds-star@tgc.com" . "misc/dsstar")
		 )

		;; Globus lists
		("^Sender:"
		 ("owner-announce@mcs.anl.gov" . "globus/announce")
		 ("owner-developers@mcs.anl.gov" . "globus/developers")
		 ("owner-security@mcs.anl.gov" . "globus/security")
		 ("owner-discuss@mcs.anl.gov" . "globus/discuss")
		 ("owner-cray@mcs.anl.gov" . "globus/cray")
		 )
		("^To:"
		 ;; No sender field for support email
		 ("\(support\|bugs\|globus-req\|globusreq\)@\(.*globus.org\|.*mcs.anl.gov\)" . "globus/support")
		 )

		;; Grid Forum lists
		("^Sender:"
		 ("owner-grid-announce@mcs.anl.gov" . "grid-forum/announce")
		 ("owner-security-wg@mcs.anl.gov" . "grid-forum/security-wg")
		 ("owner-accounts-wg@mcs.anl.gov" . "grid-forum/accounts-wg")
		 ("owner-data-wg@mcs.anl.gov" . "grid-forum/data-wg")
		 )

		;; CIC lists
		("^Sender:"
		 ("cic-swg-request" . "cic/swg")
		 ("cic-rpg-request" . "cic/rpg")
		 )

		;; Kerberos lists
		("^Senders:"
		 ("Ietf-krb-wg-Owner@achilles.ctd.anl.gov" . "kerberos/ietf-krb-wg")
		 ("owner-java-kerberos@lists.Stanford.EDU" . "kerberos/java-kerberos")
		 )
		("^To:"
		 ("krbdev@mit.edu" . "kerberos/krbdev")
		 ("krb-protocol@mit.edu" . "kerberos/krb-protocol")
		 ("kerberos@MIT.EDU" . "kerberos/kerberos")
		 ("pc-kerberos@MIT.EDU" . "kerberos/pc-kerberos")
		 )

		;; NCSA maillists
		("^Sender:"
		 ("owner-security@ncsa.uiuc.edu" . "ncsa/security")
		 ("owner-ncsa-irst@ncsa.uiuc.edu" . "ncsa/ncsa-irst")
		 ("owner-comp-pol@ncsa.uiuc.edu" . "ncsa/compol")
		 ("owner-access-online@ncsa.uiuc.edu" . "ncsa/access-online")
		 )
		("^Subject:"
		 ("Headline News" . "ncsa/headline-news")
		 )

		;; Technology lists
		("^Sender:"
		 ("owner-fvwm@hpc.uh.edu" . "technologies/fvwm")
		 ("owner-linux-tp600@icemark.ch" . "technologies/thinkpad")
		 ("owner-wu-ftpd@wugate.wustl.edu" . "technologies/wuftpd")
		 ;; Seems like can only use '*' in next regex and not '?'
		 ("owner-xemacs\(-announce\)*@xemacs.org" . "technologies/xemacs")
		 ("ssh-afs-owner@umich.edu" . "technologies/ssh-afs")
		 ("owner-hippi@storage.network.com" . "technologies/hippi")
		 )
		("^To:"
		 ("perl-update" . "technologies/perl")
		 )


		;; Alliance VMR lists
		("^Sender:"
		 ("owner-gsi-wg@ncsa.uiuc.edu" . "alliance-vmr/gsi-wg")
		 ("owner-vmr-wg@ncsa.uiuc.edu" . "alliance-vmr/vmr-wg")
		 ("owner-hpcportals@ncsa.uiuc.edu" . "alliance-vmr/portals")
		 ("owner-portals@nas.nasa.gov" . "alliance-vmr/portals")
		 )

		;; Miscellaneous lists
		("^Sender:"
		 ("owner-first-team@ncsa.uiuc.edu" . "security/first-teams")
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
		("New and Unread" (("mbox") (or (unread) (new))))

		;; Unread stuff in my inbox
		("Unread" (("mbox") (and (not deleted) unread)))

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

(defun mail-add-insert-signature-menu()
  "Add my insert-signature menu to the Mail menu. Intended for compose mode."

  (add-submenu '("Mail")
	       '("Insert signature..."
		 ["DSL"
		  (insert-signature "~/.sig/dsl") t]
		 ["Formal"
		  (insert-signature "~/.sig/formal") t]
		 ["Personal"
		  (insert-signature "~/.sig/personal") t]
		 ))
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
	 ("owner-dsl@mcs.anl.gov" . "red")
	 ("owner-dsl-core@mcs.anl.gov" . "red")
	 ("owner-dsl-uc@mcs.anl.gov" . "red")
	 ("dsl-admin@cs.uchicago.edu" . "red")
	 ("ppdg-siteaa-admin@ppdg.net" . "red")
	 ("owner-doe-sg@george.lbl.gov" . "red")
	 ;; Globus lists
	 ("owner-discuss@globus.org" . "DarkBlue")
	 ("owner-java@globus.org" . "DarkBlue")
	 ("owner-mpich-g@globus.org" . "blue")
	 ;; High-interest Globus lists
	 ("owner-python-discuss@globus.org" . "blue")
	 ("owner-developers@globus.org" . "blue")
	 ("owner-developer-discuss@globus.org" . "blue")
	 ("owner-ogsa-alpha@globus.org" . "blue")
	 ("owner-ogsa-developers@globus.org" . "blue")
	 ;; Management lists
	 ("owner-dsl-management@mcs.anl.gov" . "orange")
	 ("owner-ogsa-management@globus.org" . "orange")
	 ("owner-globus-ogsa-management@globus.org" . "orange")
	 ;; GGF
	 ("owner-security-wg@gridforum.org" . "green")
	 ("owner-ogsi-wg@gridforum.org" . "green")
	 ("gridforum.org" . "darkgreen")
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


