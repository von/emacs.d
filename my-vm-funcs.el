;;
;; my-vm-funcs.el
;;
;; Miscellaneous functions for vm
;;

(defun my-rebuild-mail-aliases()
  "Reload mail aliases from bbdb and my aliases file."

  (interactive)
  ;; This gets rid of other abbrevs and also keeps us from building
  ;; an infinitely long list of abbrev by adding each time this function
  ;; is run.
  (kill-all-abbrevs)
  (if is-xemacs
      (rebuild-mail-aliases mail-abbrev-mailrc-file)
    (rebuild-mail-abbrevs mail-abbrev-mailrc-file)
    )
  
  (bbdb-define-all-aliases)
)

(defun vm-create-virtual-folder-unread ()
  "Create a virtual folder containing unread messages from the current folder."
  (interactive)
  ;; XXX Need to add new
  (vm-create-virtual-folder-ext (list 'and
				      (list 'unread)
				      (list 'undeleted)
				      ) nil "Unread EMail"))

(defun vm-create-virtual-folder-undeleted ()
  "Create a virtual folder containing undeleted messages from the current folder."
  (interactive)
  (vm-create-virtual-folder 'undeleted nil "Undeleted Email")
)

(defun vm-create-virtual-folder-for-author (author_name)
  "Create a virtual folder containing messages from given author."
  (interactive)
  (vm-create-virtual-folder 'author author_name))

(defun vm-create-virtual-folder-for-label (label)
  "Create a virtual folder containing messages with given label."
  (interactive)
  (vm-create-virtual-folder 'label label))

(defun vm-current-folder-name()
  "Return name of current folder"

  (if (and (boundp 'folder-buffer)
	   (buffer-file-name folder-buffer))
      ;; If folder-buffer is bound then this is a summary of a folder
      ;; in which case we use the folder name (minus the folder directory)
      ;; as the folder name
      (replace-in-string
       (buffer-file-name folder-buffer)
       (regexp-quote (expand-file-name vm-folder-directory))
       "" t)
    ;; Else this is a virtual folder or something like that and we
    ;; use use the buffer name
    (buffer-name (current-buffer))
    )
)

(defun mail-set-header (header string)
  "Set the header in the current mail message to string. Header will
be overwritten if it already exists."
  (interactive)

  (if (interactive-p) (expand-abbrev))  ; for mail-abbrevs

  (save-excursion
    (if (mail-position-on-field header t)
	;; Field already exists, kill it and leave us on empty line
	(if (boundp 'backward-kill-line)
	    (backward-kill-line)
	  (kill-whole-line 0)
	  )
      ;; Field doesn't exist, put us on empty line after subject
      (progn
	(mail-position-on-field "subject")
	(insert "\n")))
    (insert (concat header ": " string))
    )
  )

(defun mail-get-from ()
  "Get the contents of the From field or return default From user if not present."
  (interactive)
  (save-excursion
    (if (not (mail-position-on-field "From" t))
	;; No From field, return default
	user-mail-address
      ;; Move to begining of line and parse it
      (beginning-of-line)
      (vm-match-header)
      (vm-matched-header-contents)))
)

(defun mail-set-from (string)
  "Insert a From:, Reply-To: and BCC: address into a piece of mail."
  (interactive)

  (if (interactive-p) (expand-abbrev))  ; for mail-abbrevs

  (mail-set-header "Reply-To" string)
  (mail-set-header "From" string)
  ;; XXX Should really be set header if present
  (if mail-self-blind (mail-set-header "Bcc" string))
  )

(defun vm-mail-waiting-p (spoolfile)
  (let ((attributes (file-attributes
		     (file-truename (expand-file-name spoolfile)))))
    (and
     attributes                         ; file exists
     (not (eq (car attributes) t))      ; not directory
     (> (nth 7 attributes) 0))))        ; not empty

(defun vm-create-virtual-folder-ext (selector &optional read-only name)
  "Create a new virtual folder from messages in the current folder.
The messages will be chosen by applying the selector you specify,
which is normally read from the minibuffer.

The difference between this and vm-create-virtual-folder is that selector
is assumed to already be a list so you can do more complicated things.

Prefix read-only means the new virtual folder should be visited read only."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command)
	 (prefix current-prefix-arg))
     (vm-select-folder-buffer)
     (nconc (vm-read-virtual-selector "Create virtual folder of messages: ")
	    (list prefix))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((use-marks (eq last-command 'vm-next-command-uses-marks))
	vm-virtual-folder-alist)
    (if (null name)
	  (setq name (format "%s %s" (buffer-name) selector)))
    (setq vm-virtual-folder-alist
	  (list
	   (list name
		 (list (list (list 'get-buffer (buffer-name)))
		       selector))))
    (vm-visit-virtual-folder name read-only))
  ;; have to do this again here because the known virtual
  ;; folder menu is now hosed because we installed it while
  ;; vm-virtual-folder-alist was bound to the temp value above
  (if vm-use-menus
      (vm-menu-install-known-virtual-folders-menu)))

(defun smtpmail-queued()
  "Is there mail queued as a result of smtp-queue-mail?"
  (> (nth 7 (file-attributes smtpmail-queue-index)) 0)
)

(defun vm-set-max-message-download-size(size)
  "Set the maximum size of message to be downloaded without prompting.
A value of nil indicates that no limit should be set."

  (setq vm-pop-max-message-size size)
  (setq vm-imap-max-message-size size)
)

(defun my-vm-forward-as-digest()
  "Forward a message using a digest mode."

  (interactive)
  ;; XXX There is certainly a better way to do this
  (setq my-vm-save vm-forwarding-digest-type)
  (setq vm-forwarding-digest-type "rfc1153")
  (vm-forward-message)
  (setq vm-forwarding-digest-type my-vm-save)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Feedmail functions
;;

(autoload 'feedmail-mail-in-queue "feedmail")

(defun feedmail-mail-in-queue()
  "Is there mail queued to be sent?"

  (< 0 (length
	(directory-files feedmail-queue-directory t nil t)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Smime functions
;;

(defun vm-smime-sign-message ()
  "Sign the current message."

  (interactive)
  (let ((body-start (mail-text-start))
	(body-end (point-max))
	;; Get address, stripping the human-readable part
	(address (nth 1 (funcall vm-chop-full-name-function (mail-get-from))))
	)
    (save-excursion
      ;; Got to start of message so we insert signature there
      (goto-char body-start)
      (smime-sign-region body-start body-end
			 (or (smime-get-key-by-email address)
			     (error "No key for address %s" address)))
      )
    ;; XXX Need to move content-type header into headers somehow
    ;;     and integrate with VM mime encoding
    )
)

(defun vm-smime-verify-message ()
  "Verify the signature on the current message."

  (interactive)
  (let ((body-start (mail-text-start))
	;; XXX this gets to end of buffer not message
	(body-end (point-max))
	)
    (smime-verify-region body-start body-end)
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-funcs)