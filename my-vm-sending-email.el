;;
;; my-vm-sending-email.el
;;
;; Configuration for sending email.

;; String to append to subjects in my replies
(setq-default vm-reply-subject-prefix "Re: ")

;; Forwarded message subject
(setq-default vm-forwarding-subject-format "Fwd: %s")

;; BCC me on everything I send?
(setq-default mail-self-blind t)

;; Encapsulation for forwarded messages (nil for none)
;; Legal values are "rfc934", "rfc1153", "mime", nil
(setq-default vm-forwarding-digest-type nil)

;; Headers not to forward
(setq-default vm-unforwarded-header-regexp "\\(Return-Path:\\|Received:\\|X-\\|Message-Id:\\|In-Reply-To:\\|Mime-Version:\\|Content-Type:\\|Precedence:\\|Content-Disposition:\\|User-Agent:\\|Sender:\\|Organization:\\|References:\\|Content-Transfer-Encoding:\\|List-Help:\\|List-Post:\\|List-Subscribe:\\|List-Id:\\|List-Unsubscribe:\\|List-Archive:\\|Errors-To:\\)")

(provide 'my-vm-sending-email)
