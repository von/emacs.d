;;
;; my-vm-getting-email.el
;;
;; Configuration for getting email.

;; Auto check for new mail when visiting each folder?
(setq-default vm-auto-get-new-mail nil)

;; How often to check for mail?
(setq vm-mail-check-interval nil)

;; Maximum size of message before prompting
(setq vm-pop-max-message-size 50000)

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