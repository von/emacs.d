;;
;; my-vm-sending-email.el
;;
;; Configuration for sending email.

;; String to append to subjects in my replies
(setq-default vm-reply-subject-prefix "Re: ")

;; Attibution string (should have CR)
(setq vm-included-text-attribution-format "%F writes (%H %m %d, %y):
")

;; Forwarded message subject
(setq-default vm-forwarding-subject-format "Fwd: %s")

;; BCC me on everything I send?
(setq-default mail-self-blind t)

;; Encapsulation for forwarded messages (nil for none)
;; Legal values are "rfc934", "rfc1153", "mime", nil
(setq-default vm-forwarding-digest-type nil)

;; Headers not to forward
(setq-default vm-unforwarded-header-regexp 
	      (regexp-opt '(
			    "Return-Path:"
			    "Received:"
			    "X-"
			    "Message-Id:"
			    "In-Reply-To:"
			    "Mime-Version:"
			    "Content-Type:"
			    "Precedence:"
			    "Content-Disposition:"
			    "User-Agent:"
			    "Sender:"
			    "Organization:"
			    "References:"
			    "Content-Transfer-Encoding:"
			    "List-Help:"
			    "List-Post:"
			    "List-Subscribe:"
			    "List-Id:"
			    "List-Unsubscribe:"
			    "List-Archive:"
			    "Errors-To:"
			    "Thread-Topic:"
			    "Thread-Index:"
			    "context-class:"
			    "Content-Class:"
			    "thread-index:"
			    "Importance:"
			    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up esmtpmail

(setq esmtpmail-code-conv-from nil)

;; provided by jam@austin.asc.slb.com (James A. McLaughlin);
;; simplified by WJC after more feedmail development;
;; idea (but not implementation) of copying smtpmail trace buffer to
;; feedmail error buffer from:
;;   Mon 14-Oct-1996; Douglas Gray Stephens
;;   modified to insert error for displaying
;; Modified to do esmtpmail by Von Welch (Oct-2003)
(defun feedmail-buffer-to-esmtpmail (prepped errors-to addr-listoid)
  "Function which actually calls esmtpmail-via-smtp to send buffer as e-mail."
  ;; I'm not sure smtpmail.el is careful about the following
  ;; return value, but it also uses it internally, so I will fear
  ;; no evil.
  (feedmail-say-debug ">in-> feedmail-buffer-to-esmtpmail %s" addr-listoid)
  (require 'esmtpmail)
  (if (not (esmtpmail-via-smtp addr-listoid prepped))
	  (progn
		(set-buffer errors-to)
		(insert "Send via esmtpmail failed.  Probable SMTP protocol error.\n")
		(insert "Look for details below or in the *Messages* buffer.\n\n")
		(let ((case-fold-search t)
			  ;; don't be overconfident about the name of the trace buffer
			  (tracer (concat "trace.*smtp.*" (regexp-quote esmtpmail-smtp-server))))
		  (mapcar
		   '(lambda (buffy)
			  (if (string-match tracer (buffer-name buffy))
				  (progn
					(insert "SMTP Trace from " (buffer-name buffy) "\n---------------")
					(insert-buffer buffy)
					(insert "\n\n"))))
		   (buffer-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Feedmail
(setq send-mail-function 'feedmail-send-it)
;; To undo the above
;;(setq send-mail-function 'smtpmail-send-it)

(require 'feedmail)

;;;
;;; If you plan to use the queue stuff, also use this:
;;;
(setq feedmail-enable-queue t)
(setq feedmail-buffer-eating-function 'feedmail-buffer-to-esmtpmail)

;; This requires my change to feedmail-vm-mail-mode or I get extra
;; frames when sending with vm-frame-per-composition on
(setq auto-mode-alist (cons '("\\.fqm$" . feedmail-vm-mail-mode)
			    auto-mode-alist))

;; Make default to send immediately
(setq feedmail-ask-before-queue-default "immediate")

;; Automatically delete queued messages after sending
(setq feedmail-queue-auto-file-nuke t)

;; Be quiet...
(setq feedmail-queue-chatty nil)

(add-hook 'vm-arrived-messages-hook 'feedmail-queue-reminder-medium)
;;(add-hook 'vm-quit-hook 'feedmail-queue-reminder)


(defun feedmail-generate-draft-menu ()
  "Generate menu for insertion of draft messages."

  (cons "Edit draft..."
	(mapcar
	 (function
	  (lambda(draft-file)
	    (let ((entry (vector
			  (replace-in-string
			   (file-name-nondirectory draft-file)
			   (concat feedmail-queue-fqm-suffix "$")
			   "")
			  (list 'find-file draft-file))
			 ))
	      entry
	      )
	    )
	  )
	 (directory-files feedmail-queue-draft-directory t nil t)
	 )
	)
  )

(defun feedmail-insert-draft-menu ()
  "Add a edit drafts menu to the Send menu."

  (if modify-menu
      (let ((menu (feedmail-generate-draft-menu)))
	(add-submenu '("Send")
		     menu
		     )
	)
    )
)

(add-hook 'vm-summary-mode-hook 'feedmail-insert-draft-menu)
;; XXX How to update memu when draft added?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Don't autofill headers incomposition
;; From: Tudor Hulubei
;;       http://www.hulubei.net/tudor/configuration/.emacs

;; Hmmm, not sure this is working...

;;; Without this, the address fields will be auto-filled, resulting in
;;; weird behaviour.  Thanks to WJCarpenter <bill@carpenter.org> for
;;; suggesting this fix.
(defun no-address-auto-fill ()
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp "\\(resent-\\)?\\(To:\\|CC:\\|Bcc:\\)"))

(add-hook 'vm-mail-hook 'no-address-auto-fill)


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

(add-hook 'mail-setup-hook 'my-vm-mail-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-sending-email)
