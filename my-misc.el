;;
;; my-misc.el
;;
;; Miscellaneous functions
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-string-between-tags(begin-tag end-tag string)
  "Insert a given string between tags."

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
	 (concat 
	  begin-tag
	  "\\(.*\\)"
	  end-tag
	  )
	 nil t)
	(progn
	  ; Delete old string
	  (delete-region (match-beginning 1) (match-end 1))
	  ; Insert new one
	  (goto-char (match-beginning 1))
	  (insert string)
	  )
	)
    )
)

(defun replace-string-with-file(string filename)
  "Replace a given string with a file's contents."

  (save-excursion
    (goto-char (point-min))
    (while
	(search-forward string nil t)
	(progn
	  (replace-match "" t t)
	  (insert-file-contents filename)
	  )
	)
    )
)
    

(defun replace-string-with-string(from-string to-string)
  "Replace a given string with a file's contents."

  (save-excursion
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match to-string t t)
      )
    )
)

(defun count-regexp (regexp &optional begin-point end-point)
  "Count the number of occurances of a regualer expression.

If given BEGIN-POINT and END-POINT limit the search otherwise the whole
buffer is searched."

  (or begin-point (setq begin-point (point-min)))
  (or end-point (setq end-point (point-max)))
  (let ((count 0)
	)
    (save-excursion
      (goto-char begin-point)
      (while (re-search-forward regexp end-point t)
	(setq count (1+ count))
	)
      )
    count
    )
)

(defun count-open-tags (open-regexp close-regexp &optional begin-point end-point)
  "Count how deep we are in regards to tags. Returns numbers of open tags
found minus the number of closed tags found. Can return negitive if more
close tags are found than open.

If BEGIN-POINT and END-POINT are provided they limit the search, otherwise
the whole buffer is searched."

  (- (count-regexp open-regexp begin-point end-point)
     (count-regexp close-regexp begin-point end-point)
     )
)

  
(defun beginning-of-line-point()

  (save-excursion
    (beginning-of-line)
    (point)
    )
)

(defun end-of-line-point()

  (save-excursion
    (end-of-line)
    (point)
    )
)

(defun positive (num)
  "Return t if NUM is positive (>0)."

  (> num 0)
)

(defun negitive (num)
  "Return t if NUM is negitive (<0)."

  (> 0 num)
)

(defun line-starts-with (regexp)
  "Return t if current line starts with regexp."

  (save-excursion
    (end-of-line)
    (let ((end (point)))
	  (beginning-of-line)
	  (not (null (re-search-forward (concat "^" regexp) end t))))
      )
)


(provide 'my-misc)
