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

;; Needed for feedmail and smtpmail to work together
(setq-default mail-envelope-from user-mail-address)

;; Regex of senders for whom I want receiver to appear in the
;; summary instead of the sender
(setq vm-summary-uninteresting-senders
      "\\(vwelch@ncsa.uiuc.edu\\|welch@mcs.anl.gov\\|von@vwelch.com\\)")

;; Remove these addresses from replies
(setq-default vm-reply-ignored-addresses
	      '(
		"welch@mcs.anl.gov"
		"vwelch@ncsa.uiuc.edu"
		"von@vwelch.com"
		))

;; Used to set file vm opens by default
(setq vm-primary-inbox "~/Mail/mbox")

(setq vm-spool-files
      '(
	("mbox" "inbox" "mbox.crash")
	;;("mcs" "imap:localhost:10143:inbox:login:welch:*" "mcs.crash")
	;;("ncsa" "pop:pop.ncsa.uiuc.edu:110:apop:vwelch:*" "ncsa.crash")
	))

(setq smtpmail-smtp-server "localhost")
(setq esmtpmail-smtp-server smtpmail-smtp-server)

;; Reply-to's to ignore
;; XXX Change this to ignore all!
(setq vm-reply-ignored-reply-tos 
      '(
	"ogsi-wg@gridforum.org"
	"secres-leadership@ncsa.uiuc.edgu"
	"security-wg@teragrid.org"
	"otp@ncsa.uiuc.edu"
	))

(setq esmtpmail-send-it-by-alist
      ;; Format: (sender smtp-host) or (sender (smtp-host port))
      ;; Note that if I am resending, the sending will show up as the
      ;; original sender and not me.
      '(
	("welch@mcs.anl.gov" ("localhost" 10025))
	("von@vwelch.com" ("localhost" 12025))
	;; Default, includes NCSA.
	(t "smtp.ncsa.uiuc.edu")
	;; For when I have to tunnel
	(t ("localhost" 11025))
	))

;; Default place to save attachments
(setq-default vm-mime-attachment-save-directory my-documents-dir)

(provide 'my-vm-conf)
