;;
;; my-vc.el
;;
;; Verson control utilities
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; **********************************************************************
;;;
;;; VC Stuff
;;;

(defun vc-co-export()
  "Check out a clean, unlocked version of a file."

  (interactive)

  (setq save-switches vc-checkout-switches)
  (setq vc-checkout-switches '("-u" "-kv"))

  (vc-checkout buffer-file-name)

  (setq vc-checkout-switches save-switches)
)

(if (and modify-menu is-xemacs (boundp 'vc-mode))
    (add-menu-button '("Tools" "VC")
		     ["Checkout for Export" vc-co-export vc-mode nil]
		     "Show status of"
		     )
  )


;;; **********************************************************************
;;;
;;; ChangeLog Stuff
;;;
(if (and modify-menu is-xemacs)
    (add-menu-button '("Tools")
		     ["ChangeLog Entry" add-change-log-entry-other-window t]
		     "Compare"
		     )
  )

;;; **********************************************************************
;;;
;;; CVS Stuff
;;;

(defun cvs-diff(&optional recurse)
  "Run cvs diff on current directory."

  (interactive)
  
  ;; Get/create my buffer
  (let ((cvs-cmd "cvs")
	(my-buffer-name "*CVS-DIFF*")
	(my-buffer (get-buffer-create my-buffer-name))
	(thisdir default-directory)
	)

    (progn
     ;; Change to our buffer and make sure it empty and writable
     (pop-to-buffer my-buffer-name)
     (setq buffer-read-only nil)
     (delete-region (point-min) (point-max))
     (setq default-directory thisdir)
     (setq command (concat cvs-cmd " diff"))
     (insert "cd " thisdir "\n" command "\n")
     (if (not recurse) (setq command (concat command " -l")))
     (message (format "Executing %s..." command))
     (sit-for 0) ;; Force redisplay
     (call-process shell-file-name nil t nil "-c" command)
     (message (format "%s done." cvs-cmd))
     )
    )
)


(defun cvs-update-default-dir()
  "Run cvs-update on the default directory."

  (interactive)

  (cvs-update default-directory)
)
  

(if (and modify-menu is-xemacs)
    (add-menu-button '("Tools") '("CVS"
				  ["Changelog-commit" cvs-mode-changelog-commit
				   (and (file-exists-p "CVS")
					(file-exists-p "ChangeLog"))]
				  ["Update" cvs-update-default-dir
				   (file-exists-p "CVS")]
				  ["Diff" cvs-diff (file-exists-p "CVS")]
				  )
		     "Compare"
		     )
  )


;;;
;;; pcl-cvs (requires elib)
;;;

(autoload 'cvs-update "pcl-cvs"
          "Run a 'cvs update' in the current working directory. Feed the
output to a *cvs* buffer and run cvs-mode on it.
If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run."
          t)

(autoload 'cvs-update-other-window "pcl-cvs"
          "Run a 'cvs update' in the current working directory. Feed the
output to a *cvs* buffer, display it in the other window, and run
cvs-mode on it.

If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run."
          t)
