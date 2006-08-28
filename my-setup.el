;;
;; my-setup.el
;;
;; Set up my workspace
;;
;; $Id$
;;

;; Find where I keep my documents. This is an abstraction for windows.
(setq my-documents-dir
      (concat 
       (cond
	;; Check for ~/My Documents (Windows)
	((file-exists-p (concat home directory-sep-string "My Documents"))
	 (concat home directory-sep-string "My Documents"))
	;; Check for ~/Documents (Mac OS)
	((file-exists-p (concat home directory-sep-string "Documents"))
	 (concat home directory-sep-string "Documents"))
	;; Fall back to home directory
	(t home)
	)
       ;; Make sure it ends with directory seperator
       directory-sep-string)
      )

;; Fire off a new frame with windows for Todo, Dates and diary.
;; (if (file-exists-p (concat my-documents-dir "Todo.txt"))
;;     (progn
;;       (select-frame (make-frame '(height 55)))
;;       (find-file (concat my-documents-dir "Todo.txt"))
;;       ;;
;;       ;; Dates pane
;;       ;; (Split at 1/3 total size for 3 panes)
;;       (select-window (split-window-vertically (/ (frame-height) 3)))
;;       (find-file (concat my-documents-dir "dates.txt"))
;;       (select-window (split-window-vertically))
;;       ;;
;;       ;; Diary pane
;;       (require 'vdiary-mode)
;;       (find-file (concat my-documents-dir "diary.txt"))
;;       )
;; )

(message "Von's setup done")
(provide 'my-setup)
