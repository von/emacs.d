;; vdiary.el --- Von's Diary mode
;;
;; $Id$

;; This file may be freely distributed, modified and used.

(require 'font-lock)
(require 'outline)

(defun vdiary-new-entry()
  "Insert date string at current point."

  (interactive)
  (goto-char (point-max))
  (insert (concat
	   "\n"
	   "* "
	   (current-time-string)
	   ": "
	   ))
)

(defvar vdiary-mode-map nil)

(if vdiary-mode-map
    nil
  (setq vdiary-mode-map outline-mode-map)
  (define-key vdiary-mode-map "\^ce" 'vdiary-new-entry)
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


(provide 'vdiary-mode)