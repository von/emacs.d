;; vdiary.el --- Von's Diary mode
;;
;; $Id$

;; This file may be freely distributed, modified and used.

(require 'font-lock)
(require 'outline)

(defvar vdiary-mode-map nil)

(if vdiary-mode-map
    nil
  (setq vdiary-mode-map outline-mode-map)
  (define-key vdiary-mode-map "\^cd" 'vdiary-insert-datestamp)
  (define-key vdiary-mode-map "\^ce" 'vdiary-start-entry)
  )

;;;###autoload
(defun vdiary-mode ()
  "Major mode for a diary file.
\\{vdiary-mode-map}
"
  (interactive)

  (kill-all-local-variables)

  (outline-mode)

  (use-local-map vdiary-mode-map)

  (goto-char (point-max))

  ;; Turn on auto-fill
  (auto-fill-mode 1)

  (run-hooks 'vdiary-mode-hook)
  )

(defun vdiary-insert-datestamp()
  "Insert date string at current point."

  (interactive)
  (goto-char (point-max))
  (let ((date-elements (split-string (current-time-string))))
    (insert (concat
	     "\n"
	     "* "
	     (nth 0 date-elements) " " ;; Day of week
	     (nth 1 date-elements) " " ;; Month
	     (nth 2 date-elements) " " ;; Date
	     (nth 4 date-elements) " " ;; Year
	     "\n"))
    )
)

(defun vdiary-start-entry()
  "Start adding new entry."

  (interactive)
  (goto-char (point-max))
  (insert "\n" "** ")
)

(provide 'vdiary-mode)