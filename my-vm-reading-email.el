;;
;; my-vm-reading-email.el
;;
;; $Id$

;; Don't automatically go to new mail messages
(setq-default vm-jump-to-new-messages nil)
(setq-default vm-jump-to-unread-messages nil)

;; Show me messages but don't mark them as read until I say so
(setq-default vm-preview-lines 0)

;; Don't take me to the next message when I hit space at the end
;; of the current one
(setq-default vm-auto-next-message nil)

;; Message size limit for URLS scanning
(setq-default vm-url-search-limit 100000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill presentation buffers
;;

(setq vm-fill-paragraphs-containing-long-lines 78)

(defun my-vm-fill-line (arg)
  "Return nil if line should be filled, t otherwise. Use to avoid filling cited lines. Meant to be used a fill-paragraph-function."

  (save-excursion
    ;; Use (backward-paragraph) instead?
    (beginning-of-line)
    (cond
     ;; Don't fill cited lines
     ((looking-at "[ \t]*>") t)
     ;; Don't fill <quote> and </quote>
     ((looking-at "[ \t]*</?quote>") t)
     ;; Don't fill SPAM assassin header lines
     ((looking-at "SPAM:") t)
     ;; Any sort of non-alphanumeric is probably not something we want to
     ;; fill (XXX better regex for this exists, I'm sure)
     ((looking-at "[ \t]*[\+\*\|#]+") t)
     ;; Else, return nil and fill
     )
    )
  )


(defun my-vm-presentation-fill-setup()
  "Setup for filling of vm presentation buffers."

  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'my-vm-fill-line)
)

(add-hook 'vm-presentation-mode-hook 'my-vm-presentation-fill-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Ulf Jasper font-lock package for VM
(require 'u-vm-color)
;; Just use in message body
;;(add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
(add-hook 'vm-select-message-hook 'u-vm-color-fontify-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-reading-email)
