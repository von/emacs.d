;; vdiary.el --- Von's Diary mode
;;
;; $Id$

;; This file may be freely distributed, modified and used.

(require 'font-lock)

(defvar vdiary-mode-map nil)

(defvar vdiary-datestamp-sep "********************")

(if vdiary-mode-map
    nil
  (setq vdiary-mode-map (make-sparse-keymap))
  (define-key vdiary-mode-map "\^cd" 'vdiary-insert-datestamp)
  )

(defconst vdiary-font-lock-keywords
  (list
   ;; Highlight datestamp separator
   (list (concat "^" (regexp-quote vdiary-datestamp-sep)) 0 'font-lock-keyword-face t)
   )
  )


;;;###autoload
(defun vdiary-mode ()
  "Major mode for a diary file.
\\{vdiary-mode-map}
"
  (interactive)

  (kill-all-local-variables)
  (use-local-map vdiary-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(vdiary-font-lock-keywords nil t))
  (turn-on-font-lock)
  (goto-char (point-max))

  ;; Turn on auto-fill
  (auto-fill-mode 1)

  (run-hooks 'vdiary-mode-hook)
  )

(defun vdiary-insert-datestamp()
  "Insert date string at current point."

  (interactive)
  (let ((date-elements (split-string (current-time-string))))
    (insert (concat
	     "\n" vdiary-datestamp-sep "\n"
	     (nth 0 date-elements) " " ;; Day of week
	     (nth 1 date-elements) " " ;; Month
	     (nth 2 date-elements) " " ;; Date
	     (nth 4 date-elements) " " ;; Year
	     "\n"))
    )
)

(provide 'vdiary-mode)