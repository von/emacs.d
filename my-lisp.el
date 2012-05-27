;;
;; my-lisp.ed
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my-emacs-lisp-init()
  "My Emacs lisp init"

  (make-menubar-local)

  (cond ((= emacs-major-version 19)
	 ;; Make a copy of Lisp popup menu since original seems to be read only
	 (setq mode-popup-menu
	       '("ELisp"
		 ["Byte-compile file"
		  (progn
		    (save-buffer)
		    (byte-compile-file buffer-file-name))
		  t]
		 "---"
		 ["Evaluate last expression" eval-last-sexp t]
		 ["Evaluate region" eval-region (region-exists-p)]
		 ["Evaluate entire buffer" eval-current-buffer t]
		 "---"
		 ["Evaluate this defun" eval-defun t]
		 ["Debug this defun" edebug-defun t]
		 "---"
		 ["Trace a function" trace-function-background t]
		 ["Untrace all functions" untrace-all
		  (fboundp
		   'untrace-all)]
		 "---"
		 ["Debug on error"
		  (setq debug-on-error
			(not debug-on-error))
		  :style toggle :selected debug-on-error]
		 ["Debug on quit"
		  (setq debug-on-quit
			(not debug-on-quit))
		  :style toggle :selected debug-on-quit])
	       )

	 (add-submenu nil mode-popup-menu)
	 )
	)
)

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-init)


;;; Edebug is a source-level debugger for emacs-lisp programs.

(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatically compile elisp files when I save them.
;; http://xahlee.org/emacs/organize_your_dot_emacs.html

(defun auto-recompile-el-buffer ()
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'auto-recompile-el-buffer)
