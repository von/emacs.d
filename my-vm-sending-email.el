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
(setq-default vm-unforwarded-header-regexp "\\(Return-Path:\\|Received:\\|X-\\|Message-Id:\\|In-Reply-To:\\|Mime-Version:\\|Content-Type:\\|Precedence:\\|Content-Disposition:\\|User-Agent:\\|Sender:\\|Organization:\\|References:\\|Content-Transfer-Encoding:\\|List-Help:\\|List-Post:\\|List-Subscribe:\\|List-Id:\\|List-Unsubscribe:\\|List-Archive:\\|Errors-To:\\)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Feedmail
(setq send-mail-function 'feedmail-send-it)
;; To undo the above
;;(setq send-mail-function 'smtpmail-send-it)

(autoload 'feedmail-send-it "feedmail")
(autoload 'feedmail-run-the-queue "feedmail")
(autoload 'feedmail-run-the-queue-no-prompts "feedmail")
(autoload 'feedmail-vm-mail-mode "feedmail")

;;;
;;; If you plan to use the queue stuff, also use this:
;;;
(setq feedmail-enable-queue t)
(setq feedmail-buffer-eating-function 'feedmail-buffer-to-smtpmail)

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

(provide 'my-vm-sending-email)
