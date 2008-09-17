;;
;; my-vm-menus.el
;;
;; $Id$

;; For vm-mail-mode-map
(require 'vm-version)
(require 'vm-vars)
;; For replace-in-string
(require 'vm-rfaddons)

;; No toolbar
(setq-default vm-use-toolbar nil)

(defun my-vm-summary-menu-setup ()
  "Set up main VM menu for summary mode."

  (if modify-menu
      (progn
	;; XXX This doesn't add to existing menus
	(easy-menu-add-item nil '("Send")
			    ["Send queued mail"
			     feedmail-run-the-queue
			     (feedmail-mail-in-queue)])

	(easy-menu-add-item nil  '("Send")
			    '("Forwarding Encapsulation..."
			      ["None" (setq vm-forwarding-digest-type nil)
			       :style radio
			       :selected (eq vm-forwarding-digest-type nil)]
			      ["Mime" (setq vm-forwarding-digest-type "mime")
			       :style radio
			       :selected (string-equal vm-forwarding-digest-type "mime")]
			      ))
	
	(easy-menu-add-item nil '("Send")
			    '("Bandwidth mode"
			      ["Low" (vm-set-bandwidth-mode "low")
			       :style radio
			       :selected (string-equal "low"
						       vm-bandwidth-mode)]
			      ["High" (vm-set-bandwidth-mode "high")
			       :style radio
			       :selected (string-equal "high"
						       vm-bandwidth-mode)]
			      )
			    )

	;;(vm-add-maillists-menu)
	)
    )
)

(add-hook 'vm-menu-setup-hook 'my-vm-summary-menu-setup)

(defun my-vm-set-dispose-popup-menu ()
  "Make the VM dispose menu the popup menu."

  ;; XXX For some reason the first message always has the menu with
  ;; all the items inactive
  (cond
   (is-gnu-emacs
    (or (boundp 'my-vm-dispose-menu-keymap)
	(progn
	  (setq my-vm-dispose-menu-keymap (make-sparse-keymap))
	  (easy-menu-define my-vm-dispose-menu-keymap
	    nil nil vm-menu-dispose-menu)))
    (setq mode-popup-menu my-vm-dispose-menu-keymap)
    )
   (t
    (setq mode-popup-menu vm-menu-dispose-menu))
   )
  )

(add-hook 'vm-presentation-mode-hook 'my-vm-set-dispose-popup-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Signature inserting stuff
;;

(defun insert-signature (file)
  (let ((mail-signature-file file))
    (mail-signature)))

(defvar my-sig-dir "~/Mail/sigs"
  "Where my signature files are.")

(defvar my-sig-extension ".txt"
  "File extension on signature files.")

(defun generate-insert-signature-menu ()
  "Generate menu for insertion of signatures."

  (cons "Insert-signature..."
	(if
	    (file-exists-p my-sig-dir)
	    (mapcar
	     (function
	      (lambda(sig-file)
		(let ((entry (vector
			      (replace-in-string
			       (file-name-nondirectory sig-file)
			       (concat my-sig-extension "$")
			       "")
			      (list 'insert-signature sig-file))
			     ))
		  entry
		  )
		)
	      )
	     
	     (directory-files my-sig-dir
			      ;; Full path
			      t
			      ;; Don't match files starting with "."
			      ;; (including directories)
			      ;; Don't match files ending with "~"
			      "^[^.].*[^~]$"
			      ;; Sort
			      t)
	     )
	  )
	)
  )

(easy-menu-define nil vm-mail-mode-map
  "Allow insertion of a signature"
  (generate-insert-signature-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-menus)
