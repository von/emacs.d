;;
;; my-vm-conf.el
;;
;; Configuration for vm that is actual specific to
;; my email environment.
;;

;; Set my mailing address
(setq-default user-full-name "Von Welch")
(setq-default user-mail-address "welch@mcs.anl.gov")

;; Regex of senders for whom I want to receiver to appear in the
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

;;(setq vm-spool-files '("localhost:110:pass:welch:*"))
(setq vm-spool-files '("imap:localhost:143:inbox:login:welch:*"))

(setq smtpmail-smtp-server "localhost")

(provide 'my-vm-conf)
