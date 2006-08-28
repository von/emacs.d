;;
;; my-vm-conf.el
;;
;; Configuration for vm that is actual specific to
;; my email environment.
;;

(require 'my-setup)

;; Set my mailing address
(setq-default user-full-name "Von Welch")
(setq-default user-mail-address "vwelch@ncsa.uiuc.edu")

(setq my-email-addresses
      '(
	"welch@mcs.anl.gov"
	"vwelch@ncsa.uiuc.edu"
	"vwelch@mail.ncsa.uiuc.edu"
	"von@vwelch.com"
	"vwelch@uiuc.edu"
	))

;; Needed for feedmail and smtpmail to work together
(setq-default mail-envelope-from user-mail-address)

;; Regex of senders for whom I want receiver to appear in the
;; summary instead of the sender
(setq vm-summary-uninteresting-senders (regexp-opt my-email-addresses))

;; Remove these addresses from replies
(setq-default vm-reply-ignored-addresses my-email-addresses)

;; Set up points to my mail box and folders
(setq-default vm-folder-directory "~/mail/")

;; Where my aliases are
(setq-default mail-abbrev-mailrc-file
	      (concat vm-folder-directory "mail_aliases"))

;; Used to set file vm opens by default
(setq vm-primary-inbox
	      (concat vm-folder-directory "mbox"))

(setq vm-spool-files
      '(
	("mbox" "inbox" "mbox.crash")
	;; My system mailbox on my mac (XXX this doesn't work?)
	("mbox" "/var/mail/vwelch" "mbox.crash")
	;;("mcs" "imap:localhost:10143:inbox:login:welch:*" "mcs.crash")
	;;("ncsa" "pop:pop.ncsa.uiuc.edu:110:apop:vwelch:*" "ncsa.crash")
	))

(setq smtpmail-smtp-server "localhost")
(setq esmtpmail-smtp-server smtpmail-smtp-server)

;; Reply-to's to ignore
;; XXX Change this to ignore all lists?
(setq vm-reply-ignored-reply-tos 
      '(
	"ogsi-wg@gridforum.org"
	"secres-leadership@ncsa.uiuc.edgu"
	"security-wg@teragrid.org"
	;; All lists at ncsa seem to have reply-to, so ignore any ncsa
	;; reply to
	"@ncsa.uiuc.edu"
	;; Ditto with teragrid
	"@teragrid.org"
	))

(setq esmtpmail-send-it-by-alist
      ;; Format: (sender-regex smtp-host) or (sender-regex (smtp-host port))
      ;; Note that if I am resending, the sending will show up as the
      ;; original sender and not me.
      '(
	("welch@mcs.anl.gov" ("localhost" 10025))
	("vwelch.com" ("localhost" 12025))
	;; Default, tunnel to NCSA.
	(t ("localhost" 11025))
	;;(t ("smtp.ncsa.uiuc.edu" 25))
	))

;; Default place to save attachments
(setq-default vm-mime-attachment-save-directory my-documents-dir)

;; Directory for relatvice attachments
(setq-default vm-mime-attachment-source-directory my-documents-dir)

(provide 'my-vm-conf)
