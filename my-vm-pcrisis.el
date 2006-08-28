;;
;; my-vm-pcrisis.el
;;
;; Configure vm-pcrisis
;;

(load "vm-pcrisis")

(setq mcs-address "welch@mcs.anl.gov")
(setq mcs-address-nice "Von Welch <welch@mcs.anl.gov>")
(setq ncsa-address "vwelch@ncsa.uiuc.edu")
(setq ncsa-address-nice "Von Welch <vwelch@ncsa.uiuc.edu>")
(setq personal-address "von@vwelch.com")
(setq personal-address-nice "Von Welch <von@vwelch.com>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Majordomo functions
;;

(defvar von-vmpc-majordomo-passwords
  '(
    ; GGF
    ("security-area" "ggfsec")
    ("ogsa-authz" "ssapliame")
    ; Globus
    ("security-announce" "ssapliame")
    ("security-internal" "evorppa")
    ("security" "evorppa")
    ("gridshib-beta" "evorppa")
    ("security-report" "ssapliame")
    ; NCSA
    ("otp" "ache1Fo")
    ("Cyberarch-wg" "ache1Fo")
    ("Gridshib" "ache1Fo")
    ("srd" "ache1Fo")
    )
  "*Alist of mailling list names and passwords for use by von-vmpc-process-majordomo-request.")

(defun von-vmpc-process-majordomo-message()
  "Process a Majordomo message. Meant to be called by vmpc-composition-buffer-function."

  (cond
   ;; Handle (un)subscription approval requests
   ;; Remember this is matching response, so need to ingore reply prefix
   ((or
     (save-excursion
       (re-search-forward
	"approve PASSWORD \\(\\S-+\\) \\(\\S-+\\) \\(.*\\)$"
	;; No limit and no error if not found
	(point-max) t)
       )
     (save-excursion
       (re-search-forward
	;; NCSA majordomo does it this way with backslash-escaped newlines
	;; XXX I think this will cover the regex above as well
	"approve PASSWORD[> \n\t\\\\]+\\(\\S-+\\)[> \n\t\\\\]+\\(\\S-+\\)[> \n\t\\\\]+\\(.*\\)"
	;; No limit and no error if not found
	(point-max) t)
       )
     )
    ;; Found
    (let (
	  ;; subscribe or unsubscribe
	  (request-type (match-string 1))
	  (list-name (match-string 2))
	  (address (match-string 3))
	  )
      ;; Build approve command
      (insert (concat "approve "
		      ;; Lookup password for list
		      (or (cadr (assoc list-name
				       von-vmpc-majordomo-passwords))
			  ;; Not found, insert a default
			  "PASSWORD")
		      " " request-type " " list-name " " address "\n"))
      ;; End majordomo processing
      (insert "end\n")
      )
    )
   ;; Subscription confirmation
   ((save-excursion
      (re-search-forward
       ;; auth 1afc1ee9 subscribe security-announce "Von Welch" globus@vwelch.com
       "auth \\(\\S-+\\) subscribe \\(\\S-+\\) \\(.*\\)$"
       ;; No limit and no error if no found
       (point-max) t)
      )
    ;; Found
    (let (
	  (command (match-string 0))
	  )
      (insert (concat command "\n"
		      ;; And end processing
		      "end\n"))
      )
    )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq vmpc-any-recipient
      (regexp-opt '("To:" "CC:")))

;; Receiver or Sender
(setq vmpc-any-address
      (regexp-opt '("To:" "CC:" "From:")))

(setq vmpc-conditions
      '(
	;; Order important, only first match is used
	("to-mcs"
	 (vmpc-header-match "To"
			    (regexp-quote mcs-address)))
	("from-mcs"
	 (vmpc-header-match "From"
			    (regexp-quote mcs-address)))
	("to-ncsa"
	 (vmpc-header-match vmpc-any-address
			    (regexp-quote ncsa-address)))
	("to-family"
	 (vmpc-header-match "To"
			    (regexp-opt '(
					  "ljwelch@comcast.net"
					  "lcwelch@gforcecable.com"
					  "kent@derstrudel.org"
					  "deejaywelch@att.net"
					  ))))
	("personal"
	 (vmpc-header-match "to"
			    (regexp-quote personal-address)))
	("to-vwelch"
	 (vmpc-header-match "To" "vwelch.com"))
	("to-globus"
	 (vmpc-header-match vmpc-any-recipient "globus.org" ", "))
	("to-gridforum"
	 (vmpc-header-match vmpc-any-recipient
			    (regexp-opt '(
					  "gridforum.org"
					  "ggf.org" 
					  )) ", "))
	("to-doesg"
	 (vmpc-header-match vmpc-any-recipient
			    (regexp-opt '(
					  "doe-sg-ca@george.lbl.gov"
					  "doe-sg-pma@george.lbl.gov"
					  )) ", "))
	;; Other lists that I should use my MCS address for
	("to-use-mcs-list"
	 (vmpc-header-match vmpc-any-recipient
			    (regexp-quote "ietf-cat-wg@lists.Stanford.EDU")
			    ))
	("mime-base64"
	 (vmpc-body-match "Content-Transfer-Encoding: base64"))
	("plain-msg"
	 (not (vmpc-other-cond "mime-base64")))
	("majordomo-request"
	 (vmpc-header-match "From" "Majordomo"))
	("spam"
	 (vmpc-header-match "X-Spam-Flag" "YES"))
	("true" 't)
	)
)

(setq vmpc-actions
      '(
	("mcs"
	 (vmpc-substitute-header "From" mcs-address-nice)
	 (vmpc-substitute-header "Bcc" mcs-address)
	 (vmpc-substitute-header "Reply-To" mcs-address-nice)
	 )
	("ncsa"
	 (vmpc-substitute-header "From" ncsa-address-nice)
	 (vmpc-substitute-header "Bcc" ncsa-address)
	 (vmpc-substitute-header "Reply-To" ncsa-address-nice)
	 )
	("personal"
	 (vmpc-substitute-header "From" personal-address-nice)
	 (vmpc-substitute-header "Bcc" personal-address)
	 (vmpc-substitute-header "Reply-To" personal-address-nice)
	 )
	("plain-encode"
	 (vmpc-pre-function '(setq vm-forwarding-digest-type nil))
	 )
	("mime-encode"
	 (vmpc-pre-function '(setq vm-forwarding-digest-type "mime"))
	 )
	("majordomo-respond"
	 (vmpc-composition-buffer-function
	  '(von-vmpc-process-majordomo-message))
	 )
	("test"
	 ;; for testing
	 (vmpc-substitute-header "X-VM-PCrisis-Test" "Yes")
	 )
	)
)

(setq vmpc-replies-alist
      ;; Only first match is evaluated
      '(
	("to-use-mcs-list" "mcs")
	;;("from-mcs" "mcs")
	("to-ncsa" "ncsa")
	("personal" "personal")
	("to-vwelch" "personal")
	("majordomo-request" "majordomo-respond")
	)
)


(setq vmpc-automorph-alist
      '(
	("to-use-mcs-list" "mcs")
	("to-family" "personal")
	)
)

(setq vmpc-forwards-alist
      '(
	;; Encode messages appropriately based on presence of base 64
	;; content.
	;; What would be cool here would be to write a function that
	;; saved all the base64 attachments and then reattached them
	;; to a text message
	("mime-base64" "mime-encode")
	("plain-msg" "plain-encode")
	("personal" "personal")
	("to-vwelch" "personal")
	)
)

;; Do automorph when tabbing from headers to body
(add-hook 'vm-mail-mode-hook
	  '(lambda () (local-set-key [tab] 'vmpc-tab-header-or-tab-stop)))
;; Or explicitly with C-c a
(add-hook 'vm-mail-mode-hook
	  '(lambda () (local-set-key "\C-ca" 'vmpc-automorph)))

;; Do automorph just before sending (should have confirm on for this)
(add-hook 'vm-mail-send-hook 'vmpc-automorph)

(provide 'my-vm-pcrisis)