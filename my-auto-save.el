;;
;; my-auto-save.el
;;
;; XXX Has user-specifc paths
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if is-xemacs
    (progn
      (setq auto-save-directory (if is-ms-windows
				    (expand-file-name "c:/temp")
				  (expand-file-name "/usr/tmp/vwelch-autosaves/")
				  )
	    auto-save-directory-fallback auto-save-directory
	    auto-save-hash-p nil

	    auto-save-offer-delete t

	    ;; Don't put save lists in ~ to reduce clutter
	    auto-save-list-file-prefix "~/.autosave/saves-"

	    ange-ftp-auto-save t
	    ange-ftp-auto-save-remotely nil
	    ;; Keystrokes between autosaves
	    auto-save-interval 2000

	    ;; Seconds between autosaves
	    auto-save-timeout 300
	    )
      (require 'auto-save)
      )
)
