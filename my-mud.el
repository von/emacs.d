;;
;; my-mud.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moo-send-string (string)
  "Send STRING as input to the current moo. Doesn't append carriage return."
  (send-string (mud-process mud-here) string)
)

(defun moo-@who()
  "Do @who."
  (interactive)
  (moo-send-string "@who\n")
)

(defun moo-look()
  "Do look."
  (interactive)
  (moo-send-string "look\n")
)

(defun moo-ncsa()
  "Autoconnect to the NCSA moo."
  (interactive)
  (mud "Rivendell" t)
)

(defun moo-wfg()
  "Autoconnect to the NCSA moo."
  (interactive)
  (mud "WFG" t)
)

(defun my-moo-mode()

  ;; Call original hook
  (define-moo-mode-commands)

  (make-menubar-local)

  (setq mode-popup-menu
	'("MOO"
	  "---"
	  ["Quit" mud-quit t]
	  )
	)

  (add-submenu nil mode-popup-menu)

  (add-to-list 'moo-filter-hook 'mud-check-to-you)

)
				
(autoload 'mud "mud")

(if is-ms-windows
    (setq mud-entry-file "~/local/mud_worlds")
(setq mud-entry-file "~/private/mud_worlds")
)

(setq
 mud-use-entire-window t
 mud-delete-macro-window t
 moo-mode-hook 'my-moo-mode
 use-suppress-all-input t
)

(global-set-key "\C-cm" 'moo-ncsa)


(if modify-menu
    (progn
      (add-menu-button '("Tools") ["Mud" mud t])
      (add-menu-button '("Tools") ["NCSA" moo-ncsa t])
      (add-menu-button '("Tools") ["WFG" moo-wfg t])
      )
)


;; Copied from mud-check-page()
(defvar mud-show-to-you nil
  "*if non-nil, pop up MUD buffer whenver someone says something to you.")

(defvar mud-to-you-regexp "[^ ]* \\[to you\\]:"
  "*Regex for detecting if someone says something to you.")

(defun mud-check-to-you ()
  "Look for message to myself, and pop-up buffer if specified."
  (goto-char (point-min))
  (while (not (eobp))
    (if (and mud-show-to-you (looking-at mud-to-you-regexp))
	(progn
	  (display-buffer (current-buffer))
	  (message "You are being talked to in %s"
		   (buffer-name (current-buffer)))))
    (beginning-of-line 2)))

(setq mud-show-to-you t)
(setq mud-show-page t)
