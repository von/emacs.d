;;
;; my-xemacs-config.el
;;
;; Xemacs-specific configuration
;;
;; Last updated for Xemacs 20.4
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if is-xemacs
    (progn

      ;; Start gnuserv if available
      (gnuserv-start)

      ;; Load pending-del if present
      ;; XXX - What does this do anyways?
      (require 'pending-del)

      ;; Display line number modeline
      (cond ((boundp 'line-number-mode)
	     (line-number-mode t)
	     )
	    )

      ;; Display column number on modeline
      (cond ((boundp 'display-column-mode)
	     (display-column-mode t)
	     )

	    ((boundp 'column-number-mode)
	     (column-number-mode t)
	     )
	    )
      ;; Configure paren matching
      (cond ((boundp 'paren-set-mode)
	     (paren-set-mode 'paren)
	     )
	    )

      ;; Change the values of some variables.
      ;; (t means true; nil means false.)
      ;;
      ;; Use the "Describe Variable..." option on the "Help" menu
      ;; to find out what these variables mean.
      (setq find-file-use-truenames nil
	    find-file-compare-truenames t
	    minibuffer-confirm-incomplete t
	    next-line-add-newlines nil
	    )

      ;; Make the toolbar go away
      (set-default-toolbar-position 'left)
      (add-spec-list-to-specifier left-toolbar-width '((global (nil . 0))))

      ;; When running ispell, consider all 1-3 character words as correct.
      (setq ispell-extra-args '("-W" "3"))
      
      ;; What the heck is aspell?
      (setq ispell-program-name "ispell")
      
      (setq-default mode-line-buffer-identification '("%17b"))
      (setq mode-line-buffer-identification '("%17b"))

      (cond (is-x-windows
	     ;;
	     ;; Code which applies only when running emacs under X goes here.
	     ;; (Currently, this is always the case in XEmacs, but it will
	     ;; not be in the future.)
	     ;;

	     ;; This changes the variable which controls the text that goes
	     ;; in the top window title bar.  (However, it is not changed
	     ;; unless it currently has the default value, to avoid
	     ;; interfering with a -wn command line argument I may have
	     ;; started emacs with.)
	     (if (equal frame-title-format "%S: %b")
		 (setq frame-title-format
		       (concat "%b:%S"
			       (if (string= (system-name) x-display-host)
				   ""
				 (concat "@" hostname))))
	       )

	     ;; If we're running on display 0, load some nifty sounds that
	     ;; will replace the default beep.  But if we're running on a
	     ;; display other than 0, which probably means my NCD X terminal,
	     ;; which can't play digitized sounds, do two things: reduce the
	     ;; beep volume a bit, and change the pitch of the sound that is
	     ;; made for "no completions."
	     ;;
	     ;; (Note that sampled sounds only work if XEmacs was compiled
	     ;; with sound support, and we're running on the console of a
	     ;; Sparc, HP, or SGI machine, or on a machine which has a
	     ;; NetAudio server; otherwise, you just get the standard beep.)
	     ;;
	     ;; (Note further that changing the pitch and duration of the
	     ;; standard beep only works with some X servers; many servers
	     ;; completely ignore those parameters.)
	     ;;
	     
	     ;;
	     ;; MAE grabs the audio device and doesn't let go, so I comment this out to
	     ;; keep xemacs from complaining.
	     ;;
	     ;;	      (cond ((string-match ":0" (getenv "DISPLAY"))
	     ;;		     (load-default-sounds))
	     ;;		    (t
	     ;;		     (setq bell-volume 40)
	     ;;		     (setq sound-alist
	     ;;			   (append sound-alist '((no-completion :pitch 500))))
	     ;;		     ))
	     
	     ;; Don't iconify
	     (global-unset-key "\C-z")
	     (global-unset-key "\C-x\C-z")
	     
	     ;; Paste at cursor point instead of click point if 't'
	     (setq mouse-yank-at-point nil)
	     ))
      
      ;;
      ;; (The following code applies whether or not we're running X.)
      ;;
      
      
      ;; Change the cursor used when the mouse is over a mode line
      (setq x-mode-pointer-shape "leftbutton")

      ;; Change the cursor used during garbage collection.
      ;;
      ;; Note that this cursor image is rather large as cursors go, and so it
      ;; won't work on some X servers (such as the MIT R5 Sun server) because
      ;; servers may have lamentably small upper limits on cursor size.
      ;;(if (featurep 'xpm)
      ;;   (setq x-gc-pointer-shape
      ;;	 (expand-file-name "trash.xpm" data-directory)))
      
      ;; Here's another way to do that: it first tries to load the cursor
      ;; once and traps the error, just to see if it's possible to load that
      ;; cursor on this system; if it is, then it sets x-gc-pointer-shape,
      ;; because we knows that will work.  Otherwise, it doesn't change that
      ;; variable because we know it will just cause some error messages.
      (if (featurep 'xpm)
	  (let ((file (expand-file-name "recycle.xpm" data-directory)))
	    (if (condition-case error
		    (make-cursor file) ; returns a cursor if successful.
		  (error nil))	    ; returns nil if an error occurred.
		(setq x-gc-pointer-shape file))))
      
      ;; No complex buffers menus
      (setq-default complex-buffers-menu-p nil)

      ;; Clicking on modeline allows one to cycle through buffers
      ;;(setq modeline-click-swaps-buffers t)

      (defun unbury-buffer ()
	"Unbury buffer, switch to the last buffer in the buffer list"
	(interactive)
	(require 'cl)
	(switch-to-buffer (car (last (buffer-list)))))

      ;; Make F1/F2 rotate thru the buffer list
      (define-key global-map '(f1) 'unbury-buffer)
      (define-key global-map '(f2) 'bury-buffer)

      ;; Make grep buffer appear in current window (otherwise if it's
      ;; displayed elsewhere already we may not see it)
      (add-to-list 'same-window-buffer-names "*grep*")
      
      
      ;;
      ;; GNU Emacs has this and Xemacs apparently doesn't so define for Xemacs
      ;; so I can use GNU Emacs gnuclient
      (defun server-edit-files (list)
	"Wrapper around gnuserv-edit-file for GNU Emacs Gnuclient compatability."
	(gnuserv-edit-files '(mswindows) list)
	)

      (defun server-edit-files-quickly (list)
	"Wrapper around gnuserv-edit-file for GNU Emacs Gnuclient compatability."
	(gnuserv-edit-files '(mswindows) list 'quick)
	)
      )
  )
(provide 'my-xemacs-config)

;; End XEmacs Stuff
