;;
;; my-vm.el
;;
;; $Id$

;; Set my mailing address
(setq-default user-full-name "Von Welch")
(setq-default user-mail-address "welch@mcs.anl.gov")
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

;; String to append to subjects in my replies
(setq-default vm-reply-subject-prefix "Re: ")

;; Forwarded message subject
(setq-default vm-forwarding-subject-format "Fwd: %s")


;; Use netscape to display urls, with a new window for each
;; And tell netscape not to raise itself
(setq vm-url-browser 'vm-mouse-send-url-to-netscape-new-window)
(setq vm-netscape-program-switches '("-noraise"))

;; Regex of senders for whom I want to receiver to appear in the
;; summary instead of the sender
(setq vm-summary-uninteresting-senders
      "\\(vwelch@ncsa.uiuc.edu\\|welch@mcs.anl.gov\\|von@vwelch.com\\)")

;; Auto check for new mail when visiting each folder?
(setq-default vm-auto-get-new-mail nil)

;; How often to check for mail?
(setq vm-mail-check-interval nil)

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

;; Remove these addresses from replies
(setq-default vm-reply-ignored-addresses
	      '(
		"welch@mcs.anl.gov"
		"vwelch@ncsa.uiuc.edu"
		"von@vwelch.com"
		))

;; BCC me on everything I send?
(setq-default mail-self-blind t)

;; Encapsulation for forwarded messages (nil for none)
;; Legal values are "rfc934", "rfc1153", "mime", nil
(setq-default vm-forwarding-digest-type nil)

(defun my-vm-forward-as-digest()
  "Forward a message using a digest mode."

  (interactive)
  ;; XXX There is certainly a better way to do this
  (setq my-vm-save vm-forwarding-digest-type)
  (setq vm-forwarding-digest-type "rfc1153")
  (vm-forward-message)
  (setq vm-forwarding-digest-type my-vm-save)
)
  
;; Headers not to forward
(setq-default vm-unforwarded-header-regexp "\\(Return-Path:\\|Received:\\|X-\\|Message-Id:\\|In-Reply-To:\\|Mime-Version:\\|Content-Type:\\|Precedence:\\|Content-Disposition:\\|User-Agent:\\|Sender:\\|Organization:\\|References:\\|Content-Transfer-Encoding:\\|List-Help:\\|List-Post:\\|List-Subscribe:\\|List-Id:\\|List-Unsubscribe:\\|List-Archive:\\|Errors-To:\\)")

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

(defun my-rebuild-mail-aliases()
  "Reload mail aliases from bbdb and my aliases file."

  (interactive)
  (rebuild-mail-aliases mail-abbrev-mailrc-file)
  (bbdb-define-all-aliases)
)

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
		("\\.doc$" . "application/msword")
		("\\.xls$" . "application/x-msexcel")
		("\\.pdf$" . "application/pdf")
		("\\.ppt$" . "application/x-mspowerpoint")
		("\\.rtf$" . "application/richtext")
		("\\.jpg$" . "image/jpeg")
		("\\.ps$" . "application/postscript")
		("\\.gif$" . "image/gif")
		))

;; Guess mime's type using filename and vm-mime-attachment-auto-type-alist
(setq-default vm-infer-mime-types t)

;; Default place to save attachments
(setq-default vm-mime-attachment-save-directory "~/")

;; Don't decode messages when previewing them
(setq-default vm-mime-decode-for-preview nil)

;; Don't display these mime types myself
;(setq-default vm-mime-internal-content-type-exceptions
;	      '("text/html" "image/jpeg" "image/gif")
;	      )

;; Don't display these attachments automatically
;(setq-default vm-auto-displayed-mime-content-type-exceptions
;;            Eudora send stuff at HTML, so go ahead and display this
;	      '("text/html")
;	      )

;; Handle attachments
(setq-default vm-mime-external-content-types-alist
      '(
	;; Use OpenURL() instead of OpenFile so we can specifiy new-window
	("text/html" 	                  "netscape" "-noraise" "-remote" "openURL(file:%f, new-window)")
	("image/gif" 	                  "netscape" "-noraise" "-remote" "openURL(file:%f, new-window)")
	("image/jpeg" 	                  "netscape" "-noraise" "-remote" "openURL(file:%f, new-window)")
	;;("image/gif" 	                  "xview")
	;;("image/jpeg"	                  "xview")
	("video/mpeg"                     "mpeg_play")
	("video" 	                  "xanim")
	;; Use mime_display for all applications
	;; Windows workaround: exec explicitly with perl or the
	;;                     script doesn't get it's arguments
	("application"                    "perl c:\\utils\\mime_display.pl %t %f")
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
;; End Mime Configuration
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

(defun vm-create-virtual-folder-unread ()
  "Create a virtual folder containing unread messages from the current folder."
  (interactive)
  ;; XXX Need to add new
  (vm-create-virtual-folder-ext (list 'and
				      (list 'unread)
				      (list 'undeleted)
				      ) nil "Unread EMail"))

(defun vm-create-virtual-folder-undeleted ()
  "Create a virtual folder containing undeleted messages from the current folder."
  (interactive)
  (vm-create-virtual-folder 'undeleted nil "Undeleted Email")
)

(defun vm-create-virtual-folder-for-author (author_name)
  "Create a virtual folder containing messages from given author."
  (interactive)
  (vm-create-virtual-folder 'author author_name))

(defun vm-create-virtual-folder-for-label (label)
  "Create a virtual folder containing messages with given label."
  (interactive)
  (vm-create-virtual-folder 'label label))

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
;; Support functions
;;

(defun vm-current-folder-name()
  "Return name of current folder"

  (if (and (boundp 'folder-buffer)
	   (buffer-file-name folder-buffer))
      ;; If folder-buffer is bound then this is a summary of a folder
      ;; in which case we use the folder name (minus the folder directory)
      ;; as the folder name
      (replace-in-string
       (buffer-file-name folder-buffer)
       (regexp-quote (expand-file-name vm-folder-directory))
       "" t)
    ;; Else this is a virtual folder or something like that and we
    ;; use use the buffer name
    (buffer-name (current-buffer))
    )
)

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
;; Fetchmail stuff
;;

(defvar vm-fetchmail-program "myfetchmail"
  "Program run my the run-fetchmail() function")

(defun vm-run-fetchmail()
  "Run fetchmail"

  (interactive)

  ;; Get/create my buffer
  (setq my-buffer-name "*Fetchmail*")
  (setq my-buffer (get-buffer-create my-buffer-name))
  (view-buffer my-buffer-name)
  ;; Make sure it is writable
  (setq buffer-read-only nil)
  (start-process "Fetchmail" my-buffer fetchmail-program)
  ;;(call-process fetchmail-program nil my-buffer t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Handle multiple from addresses
;;

(defun mail-set-header (header string)
  "Set the header in the current mail message to string. Header will
be overwritten if it already exists."
  (interactive)

  (if (interactive-p) (expand-abbrev))  ; for mail-abbrevs

  (save-excursion
    (if (mail-position-on-field header t)
	(backward-kill-line)
      (progn
	(mail-position-on-field "subject")
	(insert "\n")))
    (insert (concat header ": " string))
    )
  )

(defun mail-set-from (string)
  "Insert a From:, Reply-To: and BCC: address into a piece of mail."
  (interactive)

  (if (interactive-p) (expand-abbrev))  ; for mail-abbrevs

  (mail-set-header "Reply-To" string)
  (mail-set-header "From" string)
  ;; XXX Should really be set header if present
  (if mail-self-blind (mail-set-header "Bcc" string))
  )

;;
;; List of (regex function) to be called when mail is sent from
;; a folder matching regex
;;
(defvar mail-folder-compose-function-alist
  '(
    ("^mcs/" setup-mcs-compose)
    ("^vwelch.com/" setup-vwelch-com-compose)
    ("^personal/" setup-vwelch-com-compose)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maillist handling functions
;;
;; These functions based off of originals by
;; Neal Young <Neal.Young@dartmouth.edu>
;; found at
;; http://www.cs.hmc.edu/~smikes/emacs/vm-procmail.html

(defvar vm-maillists-directory "maillists/"
  "Directories were maillist spool files are kept in order they should be visited")

(defvar vm-maillist-folders '(
			       "hpc-wire"
			       "dsstar"
			       "globus-announce"
			       "grid-forum-announce"
			       "access-online"
			       "headline-news"
			       "crypto-grams"
			       "globus-security"
			       "grid-forum-security-wg"
			       "globus-gsiftp"
			       "globus-developers"
			       "gsi-wg"
			       "vmr-wg"
			       "krbdev"
			       "krb-protocol"
			       "nev-dull"
			       "emerge-tech"
			       "linux-users"
			       "thinkpad"
			       "ncsa-security"
			       "ncsa-irst"
			       "comppol"
			       "globus-discuss"
			       "globus-cray"
			       "hpcportals"
			       "portals"
			       "ietf-krb-wg"
			       "kerberos"
			       "java-kerberos"
			       "pc-kerberos"
			       "grid-forum-accounts-wg"
			       "grid-forum-data-wg"
			       "globus-support"
			       "cic-swg"
			       "cic-rpg"
			       "first-teams"
			       "xemacs"
			       "fvwm"
			       "wuftpd"
			       "perl"
			       "ssh-afs"
			       "hippi"
			       "vbns-techs"
			       "cam-reports"
			       )
  "Folders to visit with vm-visit-maillists")

(defun vm-add-maillists-menu()
  "Add a menu for maillists"
  ;; add-submenu() unfortunately just keeps appending to the same menu
  ;; so we'll delete and add each item individually.
  (interactive)
  (require 'vm)
  (vm-session-initialization)
;;  (add-submenu nil '("Maillists" :filter vm-maillists-menu-filter))
  (mapcar
   (function
    (lambda(folder)
      (let ((path '("Maillists" folder))
	    (item (vector
		   folder
		   (list 'vm-visit-maillist folder)
		   ':included
		   (list 'vm-maillist-mail-waiting-p folder)
		   )
		  )
	    )
	(delete-menu-item path)
	(add-menu-button '("Maillists") item)
	)
      )
    )
   vm-maillist-folders
   )
)

(defun vm-generate-maillists-menu(list)
  "Generate a maillist menu from the given list."
  (mapcar
   (function
    (lambda(item)
      (if (listp item)
	  (list (car item) (vm-generate-maillists-menu (cdr item)))
	(let ((entry (vector
		      item
		      (list 'vm-visit-maillist item)
		      ':active
		      (list 'vm-maillist-mail-waiting-p item)
		      )
		     )
	      )
	  entry
	  )
	)
      )
    )
   list
   )
  )

(defun vm-visit-maillist(maillist)
  "Visit a maillist"
  (vm-visit-folder (concat vm-maillists-directory maillist))
)

(defun vm-maillist-folder(maillist)
  "Given a maillist, return it's folder."
  (concat vm-maillists-directory maillist)
)

(defun vm-maillist-spool-file(maillist)
  "Given a maillist, return it's spool file."
  (concat vm-folder-directory (vm-maillist-folder maillist) ".spool")
)

(defun vm-visit-maillists()
  "Visit all the maillists with mail waiting."
  (interactive)
  (require 'vm)
  (vm-session-initialization)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil)
	(folder
	 (let* ((waiting (vm-maillist-spool-files-with-mail-waiting))
		(default (car waiting))
		(table (mapcar 'list waiting))
		(predicate nil)
		(require-match nil)
		(initial-contents (and default (cons default 0))
				  (history '(waiting . 0))
		)
		(completing-read "Run vm on folder: " table
			    predicate require-match initial-contents
			    history)
	   )
	   ))
	(vm-visit-maillist folder))
    )
  )


(defun vm-maillist-spool-files-with-mail-waiting()
  (let ((spool-files (mapcar
		     (function
		      (lambda (folder)
			(vm-maillist-spool-file folder)
			)
		      )
		     vm-maillist-folders
		     ))
	)
	(delete nil
		(mapcar
		 (function
		  (lambda (file)
		    (if (vm-mail-waiting-p file)
			(replace-in-string (file-name-nondirectory file)
					   ".spool$" "")
		      )
		    )
		  )
		 spool-files
		 )
		)
	)
  )

(defun vm-maillists-menu-filter(list)
  (delete nil
	  (mapcar
	   (function
	    (lambda (vec)
	      (if (vm-maillist-mail-waiting-p (elt vec 0))
		  vec
		)

	      )
	    )
	   list
	   )
	  )
  )

;; Not used
(defun vm-maillist-spool-files()
  (directory-files
   (concat vm-folder-directory vm-maillist-spool-directory)
   t ;; Full path
   ".spool$" ;; Regex to match
   t ;; don't sort
   t ;; files only
   )
)

(defun vm-maillist-mail-waiting-p (maillist)
  (vm-mail-waiting-p (vm-maillist-spool-file maillist))
)

(defun vm-mail-waiting-p (spoolfile)
  (let ((attributes (file-attributes
		     (file-truename (expand-file-name spoolfile)))))
    (and
     attributes                         ; file exists
     (not (eq (car attributes) t))      ; not directory
     (> (nth 7 attributes) 0))))        ; not empty

;; Don't auto fill header lines
;; XXX Really belongs elsewhere
;;(setq-default auto-fill-inhibit-regexp "^\\(To:\\|Cc:\\|Subject:\\)")


(defun vm-create-virtual-folder-ext (selector &optional read-only name)
  "Create a new virtual folder from messages in the current folder.
The messages will be chosen by applying the selector you specify,
which is normally read from the minibuffer.

The difference between this and vm-create-virtual-folder is that selector
is assumed to already be a list so you can do more complicated things.

Prefix read-only means the new virtual folder should be visited read only."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command)
	 (prefix current-prefix-arg))
     (vm-select-folder-buffer)
     (nconc (vm-read-virtual-selector "Create virtual folder of messages: ")
	    (list prefix))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((use-marks (eq last-command 'vm-next-command-uses-marks))
	vm-virtual-folder-alist)
    (if (null name)
	  (setq name (format "%s %s" (buffer-name) selector)))
    (setq vm-virtual-folder-alist
	  (list
	   (list name
		 (list (list (list 'get-buffer (buffer-name)))
		       selector))))
    (vm-visit-virtual-folder name read-only))
  ;; have to do this again here because the known virtual
  ;; folder menu is now hosed because we installed it while
  ;; vm-virtual-folder-alist was bound to the temp value above
  (if vm-use-menus
      (vm-menu-install-known-virtual-folders-menu)))

;; Changes on Windows box
;;(setq vm-spool-files '("localhost:110:pass:welch:*"))
(setq vm-spool-files '("imap:localhost:143:inbox:login:welch:*"))

(setq smtpmail-smtp-server "localhost")

(setq vm-url-browser "c:\\Program Files\\Netscape\\Netscape 6\\netscp6.exe")

;; Maximum size of message before prompting
(setq vm-pop-max-message-size 50000)

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

(vm-make-summary-hilit-alist-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Misc Functions
;;

(defun smtpmail-queued()
  "Is there mail queued as a result of smtp-queue-mail?"
  (> (nth 7 (file-attributes smtpmail-queue-index)) 0)
)

(defun vm-set-max-message-download-size(size)
  "Set the maximum size of message to be downloaded without prompting.
A value of nil indicates that no limit should be set."

  (setq vm-pop-max-message-size size)
  (setq vm-imap-max-message-size size)
)

