;;
;; my-vm-getting-email.el
;;
;; Configuration for getting email.

;; Auto check for new mail when visiting each folder?
(setq-default vm-auto-get-new-mail nil)

;; How often to check for mail?
(setq vm-mail-check-interval nil)

;; Don't delete email when I get it
(setq vm-imap-expunge-after-retrieving nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message limit modes
;;

(defvar vm-low-bandwidth-message-limit 50000
  "Limit of message to download when in low-bandwidth
mode before prompting. A value of nil means no limit.")

(defvar vm-high-bandwidth-message-limit nil
  "Limit of message to download when in low-bandwidth
mode before prompting. A value of nil means no limit.")

;; XXX Can make this a enumeration?
(defvar vm-bandwidth-mode "high"
  "Current bandwifth mode. Should be either \"high\"
or \"low\".")

(defun vm-set-bandwidth-mode (mode)
  "Set current bandwidth mode for downloading email.
mode should be either \"high\" or \"low\"."

  (setq vm-bandwidth-mode mode)
  (if (string-equal mode "high")
      (vm-set-max-message-download-size
       vm-high-bandwidth-message-limit)
    (vm-set-max-message-download-size
     vm-low-bandwidth-message-limit))
)

(vm-set-bandwidth-mode "high")
  
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

(provide 'my-vm-getting-email)