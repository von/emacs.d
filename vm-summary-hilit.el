;;; vm-summary-hilit.el
;;;
;;; Version 1.01 [15Nov95] (tested for VM 5.95 and XEmacs 19.13)
;;;
;;; Summary highlighting for VM in XEmacs
;;;
;;; Copyright (C) 1995 Dirk Lutzebaeck <<EMAIL: PROTECTED>>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; To highlight messages in your VM summary buffer add the following
;;; three items to your .vm:
;;;
;;; 1.
;;;
;;; vm-summary-hilit.el needs to appear somewhere in your load-path to
;;; be found by
;;;
;;; (require `vm-summary-hilit)
;;;
;;; 2.
;;;
;;; Define the variable vm-summary-hilit-alist in your .vm much like you
;;; did for vm-auto-folder-alist but instead of specifying a folder name
;;; specify a color name or a face name. Eg.:
;;;
;;; (setq vm-summary-hilit-alist
;;;   `(("To:\|Cc:"
;;;      ("<EMAIL: PROTECTED>\|<EMAIL: PROTECTED>" . "red")
;;;      ("<EMAIL: PROTECTED>" . bold))
;;;     ("From:"
;;;      ("<EMAIL: PROTECTED>\|<EMAIL: PROTECTED>" . "MediumBlue")
;;;      ("<EMAIL: PROTECTED>" . "DeepPink"))))
;;;
;;; The description of the variable is similar to vm-auto-folder-alist:
;;;
;;; The value of this variable should be a list of the form,
;;;
;;; ((HEADER-NAME-REGEXP
;;;     (REGEXP . COLOR-OR-FACE-NAME) ...)
;;;   ...)
;;;
;;; where HEADER-NAME-REGEXP and REGEXP are strings, and
;;; COLOR-OR-FACE-NAME is a an s-expression that evaluates to a string
;;; which matches a color name or a symbol which matches a face name.  If
;;; any part of the contents of the message header named by
;;; HEADER-NAME-REGEXP is matched by the regular expression REGEXP, VM
;;; will evaluate the corresponding COLOR-OR-FACE-NAME and shows the
;;; message in the summary buffer in this color or face.
;;;
;;; 3.
;;; 
;;; After the definition of this variable you need to add
;;;
;;; (vm-make-summary-hilit-alist-faces)
;;;
;;; in your .vm file which converts the color names to face symbols in
;;; destructively vm-summary-hilit-alist.


(defvar vm-summary-hilit-alist nil
  "*Non-nil value should be an alist that VM will use to choose as a color
or a face to show when messages are displayed in the summary buffer.
The alist should be of the form (similar to vm-auto-folder-alist)
((HEADER-NAME-REGEXP
   (REGEXP . COLOR-OR-FACE-NAME) ...
  ...))
where HEADER-NAME-REGEXP and REGEXP are strings, and COLOR-OR-FACE-NAME is a
an s-expression that evaluates to a string which matches a color name
or a symbol which matches a face name.
If any part of the contents of the message header named by
HEADER-NAME-REGEXP is matched by the regular expression REGEXP, VM will
evaluate the corresponding COLOR-OR-FACE-NAME and shows the message in
the summary buffer in this color or face.")

(defun vm-make-summary-hilit-alist-faces ()
  "Make all color names in vm-summary-hilit-alist to faces. The faces
are named vm-summary-hilit-<NUMBER> where <NUMBER> is simply the counted
appearance of the color name in vm-summary-hilit-alist."
  (let ((al vm-summary-hilit-alist)
	(fn 0))
    (while al
      (let ((ah (cdr (car al))))
	(while ah
	  (let ((a (car ah)))
	    (setq fn (1+ fn))
	    (if (stringp (cdr a))
		(setcdr a (let ((f (intern
				    (concat "vm-summary-hilit-"
					    (number-to-string fn)))))
			    (make-face f)
			    (set-face-foreground f (cdr a))
			    f))))
	  (setq ah (cdr ah))))
      (setq al (cdr al)))))

(defun vm-summary-hilit ()
  "Highlight VM summary buffer according to vm-summary-hilit-alist."
  (interactive)
  (message "Summary hilit...")
  (save-excursion
    (vm-select-folder-buffer)
    (let ((ml vm-message-list))
      (while ml
	(let ((m (car ml)))
	  (vm-summary-message-hilit))
	(setq ml (cdr ml)))))
  (message "Summary hilit...done."))

(defun vm-summary-message-hilit ()	
  "Highlight a message in VM summary buffer."
  ;; Note: this function expects the current message in the variable `m`
  ;; its outer variable scope. This is because vm-update-message-summary in
  ;; vm-summary.el uses this implicitly for vm-summary-update-hook.
  ;; Any ideas for a better solution for this problem?
  (let ((case-fold-search vm-auto-folder-case-fold-search)
	(al vm-summary-hilit-alist)
	(header nil))
    (while al
      (let ((ah (cdar al)))
	(setq header (vm-get-header-contents m (caar al)))
	(while (and ah header)
	  (let ((a (car ah)))
	    (if (string-match (car a) header)
		(progn
		  (vm-summary-set-extent-face
		   (make-extent (+ (vm-su-start-of m) 1)
				(- (vm-su-end-of m) 1)
				vm-summary-buffer)
		   (cdr a))
		  (setq ah nil al nil))))
	  (setq ah (cdr ah))))
      (setq al (cdr al)))))

(defun vm-summary-set-extent-face (exerlay newface)
  "Sets the face used by EXERLAY to NEWFACE."
  (if vmpc-xemacs-p
      (set-extent-face exerlay newface)
    (overlay-put exerlay 'face newface)))
 
(add-hook `vm-summary-redo-hook `vm-summary-hilit)
(add-hook `vm-summary-update-hook `vm-summary-message-hilit)

(provide `vm-summary-hilit)

