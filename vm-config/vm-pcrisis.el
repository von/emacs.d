;;; vm-pcrisis.el --- wide-ranging auto-setup for personalities in VM
;;
;; Copyright (C) 1999 Rob Hodges
;;
;; Package: Personality Crisis for VM
;; Homepage: http://student.uq.edu.au/~s323140/pcrisis/
;; Author: Rob Hodges <s323140@student.uq.edu.au>
;; Maintainer: s323140@student.uq.edu.au
;; Filename: vm-pcrisis.el
;; Version: 0.85 beta
;; Status: This is not an official package; it's just something I wrote.
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;; DOCUMENTATION:
;; -------------
;;
;; Documentation is now in Texinfo and HTML formats.  You should have
;; downloaded one or the other along with this package at the URL
;; above.  

;;; Code:
(eval-when-compile
  (require 'vm-version)
  (require 'vm-message)
  (require 'vm-macro))

(require 'vm-reply)

;; -------------------------------------------------------------------
;; Variables:
;; -------------------------------------------------------------------

(defvar vmpc-conditions ()
  "*List of conditions used by pcrisis to decide what to do when replying or
using `vmpc-automorph'.  For more information, see the Personality Crisis info
file.")

(defvar vmpc-actions ()
  "*List of actions that can be associated with conditions in
`vmpc-conditions' for replying (see `vmpc-replies-alist') or with
`vmpc-automorph' (see `vmpc-automorph-alist').  These are also the actions from
which you can choose when using the newmail features of Personality Crisis, or
the `vmpc-prompt-for-profile' action.  For more information, see the pcrisis
info file.")

(defvar vmpc-replies-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when replying to a message using VM with Personality Crisis.  For 
more information, see the pcrisis info file.")

(defvar vmpc-forwards-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when forwarding a message using VM with Personality Crisis.  For
more information, see the pcrisis info file.")

(defvar vmpc-automorph-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when using the `vmpc-automorph' function.  For more information,
see the Personality Crisis info file.")

(defvar vmpc-newmail-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when composing a new message using VM with Personality
Crisis.  For more information, see the pcrisis info file.")

(defvar vmpc-resend-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when resending a message using VM with Personality Crisis.  For
more information, see the pcrisis info file.")

(defvar vmpc-newmail-prompt-for-profile ()
  "Obsolete and ignored as of Personality Crisis v0.84.  
Instead set up `vmpc-newmail-alist' to taste.  See the pcrisis info
file for more details.")

(defvar vmpc-auto-profiles-file "~/.vmpc-auto-profiles"
  "*File in which to save information used by `vmpc-prompt-for-profile'.  
The user is welcome to change this value.")

(defvar vmpc-auto-profiles-expunge-days 100
  "*Number of days after which to expunge old address-profile associations from
`vmpc-auto-profiles-file'.  Performance may suffer noticeably if this file
becomes enormous, but in other repects it is preferable for this value to be
fairly high.  The value that's right for you will depend on how often you send
email to new addresses using `vmpc-prompt-for-profile' (with the REMEMBER flag
set to 'always or 'prompt).")

(defvar vmpc-current-state ()
  "Don't mess with this.")

(defvar vmpc-current-buffer ()
  "Don't mess with this.")

(defvar vmpc-saved-headers-alist ()
  "Don't mess with this.")

(defvar vmpc-auto-profiles ()
  "Don't mess with this.")

(defvar vmpc-actions-to-run ()
  "Don't mess with this.")

(defvar vmpc-true-conditions ()
  "Don't mess with this.")

;; An "exerlay" is an overlay in FSF Emacs and an extent in XEmacs.
;; It's not a real type; it's just the way I'm dealing with the damn
;; things to produce containers for the signature and pre-signature
;; which can be highlighted etc. and work on both platforms.  

(defvar vmpc-pre-sig-exerlay ()
  "Don't mess with this.")

(defvar vmpc-sig-exerlay ()
  "Don't mess with this.")

(defvar vmpc-pre-sig-face (progn (make-face 'vmpc-pre-sig-face 
	    "Face used for highlighting the pre-signature.")
				 (set-face-foreground
				  'vmpc-pre-sig-face "forestgreen")
				 'vmpc-pre-sig-face)
  "Face used for highlighting the pre-signature.")

(defvar vmpc-sig-face (progn (make-face 'vmpc-sig-face
		"Face used for highlighting the signature.")
			     (set-face-foreground 'vmpc-sig-face 
						  "steelblue")
			     'vmpc-sig-face)
  "Face used for highlighting the signature.")

(defvar vmpc-intangible-pre-sig 'nil
  "Whether to forbid the cursor from entering the pre-signature.")

(defvar vmpc-intangible-sig 'nil
  "Whether to forbid the cursor from entering the signature.")

(defvar vmpc-xemacs-p
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      t
    nil)
  "Don't mess with this.
This variable is automatically initialised to t if p-crisis is
running under XEmacs; otherwise nil.  P-crisis checks its value when
it needs to know what flavour of Emacs this is.")

(defvar vmpc-expect-default-signature 'nil
  "*Set this to 't if you have a signature-inserting function hanging
off a hook that pre-empts Personality Crisis.")


;; -------------------------------------------------------------------
;; Some easter-egg functionality:
;; -------------------------------------------------------------------

(defun vmpc-header-field-for-point ()
  "*Returns a string indicating the mail header field point is in.  
If point is not in a header field, returns nil."
  (save-excursion
    (unless (save-excursion 
	      (re-search-backward (regexp-quote mail-header-separator)
				  (point-min) 't))
      (re-search-backward "^\\([^ \t\n:]+\\):")
      (match-string 1))))

(defun vmpc-tab-header-or-tab-stop (&optional backward)
  "*If in a mail header field, moves to next useful header or body.  
When moving to the message body, calls the `vmpc-automorph' function.
If within the message body, runs `tab-to-tab-stop'.  
If BACKWARD is specified and non-nil, moves to previous useful header
field, whether point is in the body or the headers.
\"Useful header fields\" are currently, in order, \"To\" and
\"Subject\"."
  (interactive)
  (let ((curfield) (nextfield) (useful-headers '("To" "Subject")))
    (if (or (setq curfield (vmpc-header-field-for-point))
	    backward)
	(progn
	  (setq nextfield
		(- (length useful-headers) 
		   (length (member curfield useful-headers))))
	  (if backward 
	      (setq nextfield (nth (1- nextfield) useful-headers))
	    (setq nextfield (nth (1+ nextfield) useful-headers)))
	  (if nextfield
	      (mail-position-on-field nextfield)
	    (mail-text)
	    (vmpc-automorph))
	  )
      (tab-to-tab-stop)
      )))

(defun vmpc-backward-tab-header-or-tab-stop ()
  "*Wrapper for `vmpc-tab-header-or-tab-stop' with BACKWARD set"
  (interactive)
  (vmpc-tab-header-or-tab-stop 't))


;; -------------------------------------------------------------------
;; Stuff for dealing with exerlays:
;; -------------------------------------------------------------------

(defun vmpc-set-overlay-insertion-types (overlay start end)
  "Creates a new copy of OVERLAY with different insertion types at
START and END; returns this overlay.  START and END should be nil or t
-- the marker insertion types at the start and end.  This seems to be
the only way you of changing the insertion types for an overlay --
save the overlay properties that we care about, create a new overlay
with the new insertion types, set its properties to the saved ones.
Overlays suck.  Extents rule.  XEmacs got this right."
  (let* ((useful-props (list 'face 'intangible 'evaporate)) (saved-props) 
	 (i 0) (len (length useful-props)) (startpos) (endpos) (new-ovl))
    (while (< i len)
      (setq saved-props (append saved-props (cons
		       (overlay-get overlay (nth i useful-props)) ())))
      (setq i (1+ i)))
    (setq startpos (overlay-start overlay))
    (setq endpos (overlay-end overlay))
    (delete-overlay overlay)
    (if (and startpos endpos)
	(setq new-ovl (make-overlay startpos endpos (current-buffer)
				    start end))
      (setq new-ovl (make-overlay 1 1 (current-buffer) start end))
      (vmpc-forcefully-detach-exerlay new-ovl))
    (setq i 0)
    (while (< i len)
      (overlay-put new-ovl (nth i useful-props) (nth i saved-props))
      (setq i (1+ i)))
    new-ovl))


(defun vmpc-set-extent-insertion-types (extent start end)
  "Set the insertion types for START and END of EXTENT.
START and END should be either nil or t, indicating the desired value
of the 'start-open and 'end-closed properties of the extent
respectively.  
This is the XEmacs version of `vmpc-set-overlay-insertion-types'."
  ;; pretty simple huh?
  (set-extent-property extent 'start-open start)
  (set-extent-property extent 'end-closed end))


(defun vmpc-set-exerlay-insertion-types (exerlay start end)
  "Sets the insertion types for the extent or overlay named by the
symbol EXERLAY.  In other words, EXERLAY is the name of the overlay or
extent with a quote in front.  START and END are the equivalent of the
marker insertion types for the start and end of the overlay/extent."
  (if vmpc-xemacs-p
      (vmpc-set-extent-insertion-types (symbol-value exerlay) start end)
    (set exerlay (vmpc-set-overlay-insertion-types (symbol-value exerlay)
						   start end))))


(defun vmpc-exerlay-start (exerlay)
  "Returns buffer position of the start of EXERLAY."
  (if vmpc-xemacs-p
      (extent-start-position exerlay)
    (overlay-start exerlay)))


(defun vmpc-exerlay-end (exerlay)
  "Returns buffer position of the end of EXERLAY."
  (if vmpc-xemacs-p
      (extent-end-position exerlay)
    (overlay-end exerlay)))


(defun vmpc-move-exerlay (exerlay new-start new-end)
  "Moves the start and end points of EXERLAY to the buffer positions
NEW-START and NEW-END respectively."
  (if vmpc-xemacs-p
      (set-extent-endpoints exerlay new-start new-end (current-buffer))
    (move-overlay exerlay new-start new-end (current-buffer))))


(defun vmpc-set-exerlay-detachable-property (exerlay newval)
  "Sets the 'detachable or 'evaporate property for EXERLAY to NEWVAL."
  (if vmpc-xemacs-p
      (set-extent-property exerlay 'detachable newval)
    (overlay-put exerlay 'evaporate newval)))


(defun vmpc-set-exerlay-intangible-property (exerlay newval)
  "Sets the 'intangible or 'atomic property for EXERLAY to NEWVAL."
  (if vmpc-xemacs-p
      (progn
	(require 'atomic-extents)
	(set-extent-property exerlay 'atomic newval))
    (overlay-put exerlay 'intangible newval)))


(defun vmpc-set-exerlay-face (exerlay newface)
  "Sets the face used by EXERLAY to NEWFACE."
  (if vmpc-xemacs-p
      (set-extent-face exerlay newface)
    (overlay-put exerlay 'face newface)))


(defun vmpc-forcefully-detach-exerlay (exerlay)
  "Leaves EXERLAY in memory but detaches it from the buffer."
  (if vmpc-xemacs-p
      (detach-extent exerlay)
    (delete-overlay exerlay)))


(defun vmpc-make-exerlay (startpos endpos)
  "Returns a newly created exerlay spanning from STARTPOS to ENDPOS in the
current buffer."
  (if vmpc-xemacs-p
      (make-extent startpos endpos (current-buffer))
    (make-overlay startpos endpos (current-buffer))))


(defun vmpc-create-sig-and-pre-sig-exerlays ()
  "Creates the extents in which the pre-sig and sig can reside.
Or overlays, in the case of GNUmacs.  Thus, exerlays."
  (setq vmpc-pre-sig-exerlay (vmpc-make-exerlay 1 2))
  (setq vmpc-sig-exerlay (vmpc-make-exerlay 3 4))

  (vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay t)
  (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay t)
  (vmpc-forcefully-detach-exerlay vmpc-pre-sig-exerlay)
  (vmpc-forcefully-detach-exerlay vmpc-sig-exerlay)

  (vmpc-set-exerlay-face vmpc-pre-sig-exerlay 'vmpc-pre-sig-face)
  (vmpc-set-exerlay-face vmpc-sig-exerlay 'vmpc-sig-face)

  (vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay
					vmpc-intangible-pre-sig)
  (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay
					vmpc-intangible-sig)
  
  (vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay t nil)
  (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay t nil)

  ;; deal with signatures inserted by other things than p-crisis:
  (if vmpc-expect-default-signature
      (save-excursion
	(let ((p-max (point-max))
	      (body-start (save-excursion (mail-text) (point)))
	      (sig-start nil))
	  (goto-char p-max)
	  (setq sig-start (re-search-backward "\n-- \n" body-start t))
	  (if sig-start
	      (vmpc-move-exerlay vmpc-sig-exerlay sig-start p-max))))))
  

;; -------------------------------------------------------------------
;; Functions for vmpc-actions:
;; -------------------------------------------------------------------

(defun vmpc-composition-buffer-function (func)
  "Runs a composition-buffer-function provided the time is right.
That is to say, runs the function if you're really in a composition
buffer.  This function should not be called directly; only from within
the vmpc-actions list."
  (if (eq vmpc-current-buffer 'composition)
      (eval func)))

(defun vmpc-pre-function (func)
  "Runs a pre-function provided the time is right.
That is to say, runs the function before VM does its thing, whether
that be creating a new mail or a reply.  This function should not be
called directly; only from within the vmpc-actions list."
  (if (and (eq vmpc-current-buffer 'none)
	   (not (eq vmpc-current-state 'automorph)))
      (eval func)))

(defun vmpc-delete-header (hdrfield &optional entire)
  "Delete the contents of a header field in the current mail message.
If ENTIRE is specified and non-nil, deletes the header field as well."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(let ((start) (end))
	  (mail-position-on-field hdrfield)
	  (if entire
	      (setq end (+ (point) 1))
	    (setq end (point)))
	  (re-search-backward ": ")
	  (if entire
	      (setq start (progn (beginning-of-line) (point)))
	    (setq start (+ (point) 2)))
	  (delete-region start end)))))


(defun vmpc-insert-header (hdrfield hdrcont)
  "Insert HDRCONT in HDRFIELD in the current mail message.  
Both arguments are strings.  The field can either be present or not,
but if present, HDRCONT will be appended to the current header
contents."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(mail-position-on-field hdrfield)
	(insert hdrcont))))

(defun vmpc-substitute-header (hdrfield hdrcont)
  "Insert HDRCONT in HDRFIELD in the current mail message.  
Both arguments are strings.  The field can either be present or not.
If the header field is present and already contains something, the
contents will be replaced with the new ones specified in HDRCONT."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(vmpc-delete-header hdrfield)
	(vmpc-insert-header hdrfield hdrcont))))

(defun vmpc-get-current-header-contents (hdrfield &optional clump-sep)
  "Returns the contents of HDRFIELD in the current mail message.
Returns an empty string if the header doesn't exist.  HDRFIELD should
be a string.  If the string CLUMP-SEP is specified, it means to return
the contents of all headers matching the regexp HDRFIELD, separated by
CLUMP-SEP."
  ;; This code is based heavily on vm-get-header-contents and vm-match-header.
  ;; Thanks Kyle :)
  (if (eq vmpc-current-state 'automorph)
      (save-excursion
	(let ((contents nil) (header-name-regexp "\\([^ \t\n:]+\\):")
	      (case-fold-search t) (temp-contents) (end-of-headers) (regexp))
	  ;; find the end of the headers:
	  (goto-char (point-min))
	  (re-search-forward 
	     (concat "^\\(" (regexp-quote mail-header-separator) "\\)$"))
	  (setq end-of-headers (match-beginning 0))
	  ;; now rip through finding all the ones we want:
	  (setq regexp (concat "^\\(" hdrfield "\\)"))
	  (goto-char (point-min))
	  (while (and (or (null contents) clump-sep)
		      (re-search-forward regexp end-of-headers t)
		      (save-excursion
			(goto-char (match-beginning 0))
			(let (header-cont-start header-cont-end)
			  (if (if (not clump-sep)
				  (and (looking-at hdrfield) 
				       (looking-at header-name-regexp))
				(looking-at header-name-regexp))
			      (save-excursion
				(goto-char (match-end 0))
				;; skip leading whitespace
				(skip-chars-forward " \t")
				(setq header-cont-start (point))
				(forward-line 1)
				(while (looking-at "[ \t]")
				  (forward-line 1))
				;; drop the trailing newline
				(setq header-cont-end (1- (point)))))
			  (setq temp-contents 
				(buffer-substring header-cont-start 
						  header-cont-end)))))
	    (if contents
		(setq contents
		      (concat contents clump-sep temp-contents))
	      (setq contents temp-contents)))

	  (if (null contents)
	      (setq contents ""))
	  contents ))))

(defun vmpc-get-current-body-text ()
  "Returns the body text of the mail message in the current buffer."
  (if (eq vmpc-current-state 'automorph)
      (save-excursion
	(goto-char (point-min))
	(let ((start (re-search-forward 
		      (concat "^" (regexp-quote mail-header-separator) "$")))
	      (end (point-max)))
	  (buffer-substring start end)))))


(defun vmpc-get-replied-header-contents (hdrfield &optional clump-sep)
  "Returns the contents of HDRFIELD in the message being replied to.
If that header does not exist, returns an empty string.  If the string
CLUMP-SEP is specified, treat HDRFIELD as a regular expression and
return the contents of all header fields which match that regexp,
separated from each other by CLUMP-SEP."
  (if (and (eq vmpc-current-buffer 'none)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend)))
      (let ((mp (car (vm-select-marked-or-prefixed-messages 1))))
	(or (vm-get-header-contents mp hdrfield clump-sep) ""))))

(defun vmpc-get-replied-body-text ()
  "Returns the body text of the message being replied to."
  (if (and (eq vmpc-current-buffer 'none)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend)))
      (save-excursion
	(let* ((mp (car (vm-select-marked-or-prefixed-messages 1)))
	       (message (vm-real-message-of mp))
	       start end)
	  (set-buffer (vm-buffer-of message))
	  (save-restriction
	    (widen)
	    (setq start (vm-text-of message))
	    (setq end (vm-end-of message))
	    (buffer-substring start end))))))

(defun vmpc-save-replied-header (hdrfield)
  "Saves the contents of HDRFIELD in `vmpc-saved-headers-alist'. 
Does nothing if that header doesn't exist."
  (let ((hdrcont (vmpc-get-replied-header-contents hdrfield)))
  (if (and (eq vmpc-current-buffer 'none)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend))
	   (not (equal hdrcont "")))
      (add-to-list 'vmpc-saved-headers-alist (cons hdrfield hdrcont)))))

(defun vmpc-get-saved-header (hdrfield)
  "Returns the contents of HDRFIELD from `vmpc-saved-headers-alist'.  
The alist in question is created by `vmpc-save-replied-header'."
  (if (and (eq vmpc-current-buffer 'composition)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend)))
      (cdr (assoc hdrfield vmpc-saved-headers-alist))))

(defun vmpc-substitute-replied-header (dest src)
  "Inserts the contents of the header SRC in the message you're
replying to as the contents of the header DEST in your reply.  
For example, if the address you want to send your reply to is the same
as the contents of the \"From\" header in the message you're replying
to, you'd use (vmpc-substitute-replied-header \"To\" \"From\"."  
  (if (or (eq vmpc-current-state 'reply)
	  (eq vmpc-current-state 'forward)
          (eq vmpc-current-state 'resend))
      (progn
	(if (eq vmpc-current-buffer 'none)
	    (vmpc-save-replied-header src))
	(if (eq vmpc-current-buffer 'composition)
	    (vmpc-substitute-header dest (vmpc-get-saved-header src))))))

(defun vmpc-get-header-extents (hdrfield)
  "Return buffer positions (START . END) for the contents of HDRFIELD.
If HDRFIELD does not exist, return nil."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion 
        (let ((header-name-regexp "^\\([^ \t\n:]+\\):") (start) (end))
          (setq end 
                (if (mail-position-on-field hdrfield t) 
                    (point) 
                  nil))
          (setq start 
                (if (re-search-backward header-name-regexp (point-min) t)
                    (match-end 0)
                  nil))
          (and start end (<= start end) (cons start end))))))

(defun vmpc-substitute-within-header 
  (hdrfield regexp to-string &optional append-if-no-match sep)
  "Replace REGEXP in HDRFIELD with TO-STRING.  
HDRFIELD need not exist.  TO-STRING may contain references to groups
within REGEXP, in the same manner as `replace-regexp'.  If REGEXP is
not found in the header contents, and APPEND-IF-NO-MATCH is t,
TO-STRING will be appended to the header contents (with HDRFIELD being
created if it does not exist).  In this case, if the string SEP is
specified, it will be used to separate the previous header contents
from TO-STRING, unless HDRFIELD has just been created or was
previously empty."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
        (let ((se (vmpc-get-header-extents hdrfield)) (found))
          (if se
              ;; HDRFIELD exists
              (save-restriction
                (narrow-to-region (car se) (cdr se))
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  (setq found t)
                  (replace-match to-string))
                (if (and (not found) append-if-no-match)
                    (progn
                      (goto-char (cdr se))
                      (if (and sep (not (equal (car se) (cdr se))))
                          (insert sep))
                      (insert to-string))))
            ;; HDRFIELD does not exist
            (if append-if-no-match
                (progn
                  (mail-position-on-field hdrfield)
                  (insert to-string))))))))


(defun vmpc-insert-signature (sig &optional pos)
  "Inserts SIG at the end of `vmpc-sig-exerlay'.  SIG is a string.  If
it's the name of a file, the file's contents are inserted -- otherwise
the string itself is inserted.  Optional parameter POS means insert
the signature at POS if `vmpc-sig-exerlay' is detached."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	(let ((end (or (vmpc-exerlay-end vmpc-sig-exerlay) pos)))
	  (save-excursion
	    (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay nil t)
	    (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay nil)
	    (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay nil)
	    (unless end
	      (setq end (point-max))
	      (vmpc-move-exerlay vmpc-sig-exerlay end end))
	    (if (and pos (not (vmpc-exerlay-end vmpc-sig-exerlay)))
		(vmpc-move-exerlay vmpc-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n-- \n")
	    (if (and (file-exists-p sig)
		     (file-readable-p sig)
		     (not (equal sig "")))
		(insert-file-contents sig)
	      (insert sig)))
	  (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay
						vmpc-intangible-sig)
	  (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay t)
	  (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay t nil)))))
    

(defun vmpc-delete-signature ()
  "Deletes the contents of `vmpc-sig-exerlay'."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	;; make sure it's not detached first:
	(if (vmpc-exerlay-start vmpc-sig-exerlay)
	    (progn
	      (delete-region (vmpc-exerlay-start vmpc-sig-exerlay)
			     (vmpc-exerlay-end vmpc-sig-exerlay))
	      (vmpc-forcefully-detach-exerlay vmpc-sig-exerlay))))))


(defun vmpc-signature (sig)
  "Removes a current signature if present, and replaces it with SIG.
If the string SIG is the name of a readable file, its contents are
inserted as the signature; otherwise SIG is inserted literally.  If
SIG is the empty string (\"\"), the current signature is deleted if
present, and that's all."
  (if (eq vmpc-current-buffer 'composition)
      (let ((pos (vmpc-exerlay-start vmpc-sig-exerlay)))
	(save-excursion
	  (vmpc-delete-signature)
	  (if (not (equal sig ""))
	      (vmpc-insert-signature sig pos))))))
  

(defun vmpc-insert-pre-signature (pre-sig &optional pos)
  "Inserts PRE-SIG at the end of `vmpc-pre-sig-exerlay'.  
PRE-SIG is a string.  If it's the name of a file, the file's contents
are inserted; otherwise the string itself is inserted.  Optional
parameter POS means insert the pre-signature at position POS if
`vmpc-pre-sig-exerlay' is detached."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	(let ((end (or (vmpc-exerlay-end vmpc-pre-sig-exerlay) pos))
	      (sigstart (vmpc-exerlay-start vmpc-sig-exerlay)))
	  (save-excursion
	    (vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay nil t)
	    (vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay nil)
	    (vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay nil)
	    (unless end
	      (if sigstart
		  (setq end sigstart)
		(setq end (point-max)))
	      (vmpc-move-exerlay vmpc-pre-sig-exerlay end end))
	    (if (and pos (not (vmpc-exerlay-end vmpc-pre-sig-exerlay)))
		(vmpc-move-exerlay vmpc-pre-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n")
	    (if (and (file-exists-p pre-sig)
		     (file-readable-p pre-sig)
		     (not (equal pre-sig "")))
		(insert-file-contents pre-sig)
	      (insert pre-sig))))
	(vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay
					      vmpc-intangible-pre-sig)
	(vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay t)
	(vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay t nil))))


(defun vmpc-delete-pre-signature ()
  "Deletes the contents of `vmpc-pre-sig-exerlay'."
  ;; make sure it's not detached first:
  (if (eq vmpc-current-buffer 'composition)
      (if (vmpc-exerlay-start vmpc-pre-sig-exerlay)
	  (progn
	    (delete-region (vmpc-exerlay-start vmpc-pre-sig-exerlay)
			   (vmpc-exerlay-end vmpc-pre-sig-exerlay))
	    (vmpc-forcefully-detach-exerlay vmpc-pre-sig-exerlay)))))


(defun vmpc-pre-signature (pre-sig)
  "As for `vmpc-insert-pre-signature', except that the last pre-sig
added is removed before the new one is added."
  (if (eq vmpc-current-buffer 'composition)
      (let ((pos (vmpc-exerlay-start vmpc-pre-sig-exerlay)))
	(save-excursion
	  (vmpc-delete-pre-signature)
	  (if (not (equal pre-sig ""))
	      (vmpc-insert-pre-signature pre-sig pos))))))


(defun vmpc-gregorian-days ()
  "Returns the number of days elapsed since December 31, 1 B.C."
  ;; this code stolen from gnus-util.el :)
  (let ((tim (decode-time (current-time))))
    (timezone-absolute-from-gregorian
     (nth 4 tim) (nth 3 tim) (nth 5 tim))))


(defun vmpc-load-auto-profiles ()
  "Initialise vmpc-auto-profiles from vmpc-auto-profiles-file."
  (if (and (file-exists-p vmpc-auto-profiles-file)
	   (file-readable-p vmpc-auto-profiles-file))
      (save-excursion
	(set-buffer (get-buffer-create "*pcrisis-temp*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(insert-file-contents vmpc-auto-profiles-file)
	(goto-char (point-min))
	(setq vmpc-auto-profiles (read (current-buffer)))
	(kill-buffer (current-buffer)))))


(defun vmpc-save-auto-profiles ()
  "Save vmpc-auto-profiles to vmpc-auto-profiles-file."
  (if (file-writable-p vmpc-auto-profiles-file)
      (save-excursion
	(set-buffer (get-buffer-create "*pcrisis-temp*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)      
	(goto-char (point-min))
	(prin1 vmpc-auto-profiles (current-buffer))
	(write-region (point-min) (point-max)
		      vmpc-auto-profiles-file nil 'quietly)
	(kill-buffer (current-buffer)))
    ;; if file is not writable, signal an error:
    (error "Error: P-Crisis could not write to file %s"
	   vmpc-auto-profiles-file)))

(defun vmpc-fix-auto-profiles-file ()
  "Change `vmpc-auto-profiles-file' to the format used by v0.82+"
  (interactive)
  (vmpc-load-auto-profiles)
  (let ((len (length vmpc-auto-profiles)) (i 0) (day))
    (while (< i len)
      (setq day (cddr (nth i vmpc-auto-profiles)))
      (if (consp day)
	  (setcdr (cdr (nth i vmpc-auto-profiles)) (car day)))
      (setq i (1+ i))))
  (vmpc-save-auto-profiles)
  (setq vmpc-auto-profiles ()))


(defun vmpc-get-profile-for-address (addr)
  "This is a support function for vmpc-prompt-for-profile."
  (unless vmpc-auto-profiles
    (vmpc-load-auto-profiles))
  (let ((prof (cadr (assoc addr vmpc-auto-profiles))))
    (if prof
	(let ((today (vmpc-gregorian-days)))
	  ;; if we found for a profile for this address, we are still
	  ;; using it -- so "touch" the record to ensure it stays
	  ;; newer than vmpc-auto-profiles-expunge-days:
	  (setcdr (cdr (assoc addr vmpc-auto-profiles)) today)
	  (vmpc-save-auto-profiles)))
    prof ))


(defun vmpc-save-profile-for-address (prof addr)
  "This is a support function for vmpc-prompt-for-profile."
  (let ((today) (len (length vmpc-auto-profiles)) (i 0))
    (setq today (vmpc-gregorian-days))
    (add-to-list 'vmpc-auto-profiles (append (list addr prof) today))
    (if vmpc-auto-profiles-expunge-days
	;; expunge old stuff from the list:
	(while (< i len)
	  (if (> (- today (cddr (nth i vmpc-auto-profiles)))
		 vmpc-auto-profiles-expunge-days)
	      (delete (nth i vmpc-auto-profiles) vmpc-auto-profiles))
	  (setq i (1+ i))))
    (vmpc-save-auto-profiles)))


(defun vmpc-string-extract-address (str)
  "Finds the first email address in the string STR and returns it.
If no email address in found in STR, returns nil."
  (if (string-match "[^ 	,<]+@[^ 	,>]+" str)
      (match-string 0 str)))


(defun vmpc-prompt-for-profile (&optional remember)
  "Prompts the user for a profile (one of the sets of actions named in
vmpc-actions) and adds it to the list of actions to be performed,
unless it's already in there.  
REMEMBER can be set to 'always or 'prompt.  It figures out who your
message is going to, and saves a record in vmpc-auto-profiles-file
which says to use that profile for messages to that address in the
future, instead of prompting you for a profile the next time.  If set
to 'prompt, it will ask before doing this; otherwise it will do it
automatically."
  (if (and (or (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend))
	   remember)
      (error "You can't have vmpc-prompt-for-profile remember when forwarding or resending."))
  (if (or (and (eq vmpc-current-buffer 'none)
	       (not (eq vmpc-current-state 'automorph)))
	  (eq vmpc-current-state 'automorph))
      (let ((prof) (dest ""))
	;; figure out where this email is going:
	(cond 
	 ((eq vmpc-current-state 'automorph)
	  (setq dest (vmpc-get-current-header-contents "To")))
	 ((and (eq vmpc-current-state 'reply)
	       (eq vmpc-current-buffer 'none))
	  (setq dest (vmpc-get-replied-header-contents "Reply-To"))
	  (if (or (vm-ignored-reply-to dest)
		  (equal dest ""))
	      (setq dest (vmpc-get-replied-header-contents "From")))))
	;; just the email address thanks:
	(unless (eq vmpc-current-state 'newmail)
	  (setq dest (vmpc-string-extract-address dest)))
	;; figure out which profile to use:
	(setq prof
	      (or (let ((p))
		   (setq p (vmpc-get-profile-for-address dest))
		   (if p
		       (setq remember 'already))
		   p)
		  (let ((default (car (car vmpc-actions))) (choice nil))
		    (setq choice (completing-read 
				  (format "Use profile (Default \"%s\"): "
					  default) vmpc-actions nil t))
		    (if (equal choice "")
			default
		      choice))))

	(if (not (eq vmpc-current-state 'automorph))
	    ;; add it to the end of the list:
	    (if (not (member prof vmpc-actions-to-run))
		(setq vmpc-actions-to-run (append vmpc-actions-to-run
						  (cons prof ()))))
	  ;; or in automorph, run it immediately:
	  (let* ((functions (cdr (assoc prof vmpc-actions)))
		 (len (length functions)) (i 0))
	    (while (< i len)
	      (eval (nth i functions))
	      (setq i (1+ i)))))
	
	;; save the association of this profile with this destination
	;; address if applicable:
	(if (or (and (eq remember 'prompt)
		     (y-or-n-p (format "Always use \"%s\" for \"%s\"? "
				       prof dest)))
		(eq remember 'always))
	    (vmpc-save-profile-for-address prof dest)
	    ))))

;; -------------------------------------------------------------------
;; Functions for vmpc-conditions:
;; -------------------------------------------------------------------

(defun vmpc-none-true-yet (&optional &rest exceptions)
  "This is a condition that can appear in vmpc-conditions.  It means that none
of the previous conditions matched.  If EXCEPTIONS are specified, it means none
were true except those.  For example, if you wanted to check whether no
conditions had yet matched with the exception of the two conditions named
\"default\" and \"blah\", you would make the call like this:
  (vmpc-none-true-yet \"default\" \"blah\")
Then it will return true regardless of whether \"default\" and \"blah\" had 
matched."
  (let ((lenex (length exceptions)) (lentc (length vmpc-true-conditions)))
    (cond 
     ((> lentc lenex)
      'nil)
     ((<= lentc lenex)
      (let ((i 0) (j 0) (k 0))
	(while (< i lenex)
	  (setq k 0)
	  (while (< k lentc)
	    (if (equal (nth i exceptions) (nth k vmpc-true-conditions))
		(setq j (1+ j)))
	    (setq k (1+ k)))
	  (setq i (1+ i)))
	(if (equal j lentc)
	    't
	  'nil))))))

(defun vmpc-other-cond (condition)
  "Returns true if the specified CONDITION in vmpc-conditions matched.
CONDITION can only be the name of a condition specified earlier in
vmpc-conditions -- that is to say, any conditions which follow the one
containing vmpc-other-cond will show up as not having matched, because they
haven't yet been checked when this one is checked."
  (member condition vmpc-true-conditions))


(defun vmpc-header-match (hdrfield regexp &optional clump-sep)
  "Returns true if the contents of specified header HDRFIELD match REGEXP.  
For automorph, this means the header in your message; when replying it means the 
header in the message being replied to."
  (cond ((or (eq vmpc-current-state 'reply)
	     (eq vmpc-current-state 'forward)
             (eq vmpc-current-state 'resend))
	 (string-match regexp (vmpc-get-replied-header-contents
			       hdrfield clump-sep)))
	((eq vmpc-current-state 'automorph)
	 (string-match regexp (vmpc-get-current-header-contents 
			       hdrfield clump-sep)))))

(defun vmpc-body-match (regexp)
  "Returns non-nil if the contents of the message body match REGEXP.
For automorph, this means the body of your message; when replying it means the
body of the message being replied to."
  (cond ((and (or (eq vmpc-current-state 'reply)
		  (eq vmpc-current-state 'forward)
                  (eq vmpc-current-state 'resend))
	      (eq vmpc-current-buffer 'none))
	 (string-match regexp (vmpc-get-replied-body-text)))
	((eq vmpc-current-state 'automorph)
	 (string-match regexp (vmpc-get-current-body-text)))))


(defun vmpc-xor (a b &optional &rest cdetc)
  "A generic xor function.  If one and only one argument is true, returns 't."
  (let ((len (length cdetc)) (count 0) (i 0))
    (if a (setq count (1+ count)))
    (if b (setq count (1+ count)))
    (while (< i len)
      (if (nth i cdetc)
	  (setq count (1+ count)))
      (setq i (1+ i)))
    (if (equal count 1)
	't
      'nil )))

;; -------------------------------------------------------------------
;; Support functions for the advices:
;; -------------------------------------------------------------------

(defun vmpc-build-true-conditions-list ()
  (let ((len (length vmpc-conditions)) (i 0))
    (while (< i len)
      (if (eval (car (cdr (nth i vmpc-conditions))))
	  (setq vmpc-true-conditions (append vmpc-true-conditions
		     (cons (car (nth i vmpc-conditions)) ()))))
      (setq i (1+ i)))))

(defun vmpc-build-actions-to-run-list ()
  (let ((alist) (i 0) (actions) (len))
    (cond
     ((eq vmpc-current-state 'automorph) (setq alist vmpc-automorph-alist))
     ((eq vmpc-current-state 'reply) (setq alist vmpc-replies-alist))
     ((eq vmpc-current-state 'forward) (setq alist vmpc-forwards-alist))
     ((eq vmpc-current-state 'newmail) (setq alist vmpc-newmail-alist))
     ((eq vmpc-current-state 'resend) (setq alist vmpc-resend-alist)))
    (setq len (length vmpc-true-conditions))
    (while (< i len)
      (setq actions (cdr (assoc (nth i vmpc-true-conditions) alist)))
      (if actions
	  (setq vmpc-actions-to-run (append vmpc-actions-to-run actions)))
      (setq i (1+ i)))))

(defun vmpc-run-actions ()
  (let ((len (length vmpc-actions-to-run)) (i 0))
    (while (< i len)
      (let* ((functions (cdr (assoc (nth i vmpc-actions-to-run) vmpc-actions))) 
	     (len2 (length functions)) (j 0))
	(while (< j len2)
	  (eval (nth j functions))
	  (setq j (1+ j)))
	(setq i (1+ i))))))


;; ------------------------------------------------------------------------
;; The main functions and advices -- these are the entry points to pcrisis:
;; ------------------------------------------------------------------------

(defadvice vm-do-reply (around vmpc-reply activate)
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'reply
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

(defadvice vm-mail (around vmpc-newmail activate)
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'newmail
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

(defadvice vm-forward-message (around vmpc-forward activate)
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  ;; the rest is almost exactly the same as replying:
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'forward
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

(defadvice vm-resend-message (around vmpc-resend activate)
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  ;; the rest is almost exactly the same as replying:
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'resend
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

;;;###autoload
(defun vmpc-automorph ()
  "*Changes contents of the current mail message based on its own headers.
Headers and signatures can be changed; pre-signatures added; functions called.
For more information, see the Personality Crisis info file."
  (interactive)
  (setq vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'automorph
	vmpc-current-buffer 'composition)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions))

(provide 'vm-pcrisis)

;;; vm-pcrisis.el ends here
;;; vm-pcrisis.el --- wide-ranging auto-setup for personalities in VM
;;
;; Copyright (C) 1999 Rob Hodges
;;
;; Package: Personality Crisis for VM
;; Homepage: http://student.uq.edu.au/~s323140/pcrisis/
;; Author: Rob Hodges <s323140@student.uq.edu.au>
;; Maintainer: s323140@student.uq.edu.au
;; Filename: vm-pcrisis.el
;; Version: 0.85 beta
;; Status: This is not an official package; it's just something I wrote.
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;; DOCUMENTATION:
;; -------------
;;
;; Documentation is now in Texinfo and HTML formats.  You should have
;; downloaded one or the other along with this package at the URL
;; above.  

;;; Code:
(eval-when-compile
  (require 'vm-version)
  (require 'vm-message)
  (require 'vm-macro))

(require 'vm-reply)

;; -------------------------------------------------------------------
;; Variables:
;; -------------------------------------------------------------------

(defvar vmpc-conditions ()
  "*List of conditions used by pcrisis to decide what to do when replying or
using `vmpc-automorph'.  For more information, see the Personality Crisis info
file.")

(defvar vmpc-actions ()
  "*List of actions that can be associated with conditions in
`vmpc-conditions' for replying (see `vmpc-replies-alist') or with
`vmpc-automorph' (see `vmpc-automorph-alist').  These are also the actions from
which you can choose when using the newmail features of Personality Crisis, or
the `vmpc-prompt-for-profile' action.  For more information, see the pcrisis
info file.")

(defvar vmpc-replies-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when replying to a message using VM with Personality Crisis.  For 
more information, see the pcrisis info file.")

(defvar vmpc-forwards-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when forwarding a message using VM with Personality Crisis.  For
more information, see the pcrisis info file.")

(defvar vmpc-automorph-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when using the `vmpc-automorph' function.  For more information,
see the Personality Crisis info file.")

(defvar vmpc-newmail-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when composing a new message using VM with Personality
Crisis.  For more information, see the pcrisis info file.")

(defvar vmpc-resend-alist ()
  "*An alist which associates conditions in `vmpc-conditions' with actions in
`vmpc-actions' when resending a message using VM with Personality Crisis.  For
more information, see the pcrisis info file.")

(defvar vmpc-newmail-prompt-for-profile ()
  "Obsolete and ignored as of Personality Crisis v0.84.  
Instead set up `vmpc-newmail-alist' to taste.  See the pcrisis info
file for more details.")

(defvar vmpc-auto-profiles-file "~/.vmpc-auto-profiles"
  "*File in which to save information used by `vmpc-prompt-for-profile'.  
The user is welcome to change this value.")

(defvar vmpc-auto-profiles-expunge-days 100
  "*Number of days after which to expunge old address-profile associations from
`vmpc-auto-profiles-file'.  Performance may suffer noticeably if this file
becomes enormous, but in other repects it is preferable for this value to be
fairly high.  The value that's right for you will depend on how often you send
email to new addresses using `vmpc-prompt-for-profile' (with the REMEMBER flag
set to 'always or 'prompt).")

(defvar vmpc-current-state ()
  "Don't mess with this.")

(defvar vmpc-current-buffer ()
  "Don't mess with this.")

(defvar vmpc-saved-headers-alist ()
  "Don't mess with this.")

(defvar vmpc-auto-profiles ()
  "Don't mess with this.")

(defvar vmpc-actions-to-run ()
  "Don't mess with this.")

(defvar vmpc-true-conditions ()
  "Don't mess with this.")

;; An "exerlay" is an overlay in FSF Emacs and an extent in XEmacs.
;; It's not a real type; it's just the way I'm dealing with the damn
;; things to produce containers for the signature and pre-signature
;; which can be highlighted etc. and work on both platforms.  

(defvar vmpc-pre-sig-exerlay ()
  "Don't mess with this.")

(defvar vmpc-sig-exerlay ()
  "Don't mess with this.")

(defvar vmpc-pre-sig-face (progn (make-face 'vmpc-pre-sig-face 
	    "Face used for highlighting the pre-signature.")
				 (set-face-foreground
				  'vmpc-pre-sig-face "forestgreen")
				 'vmpc-pre-sig-face)
  "Face used for highlighting the pre-signature.")

(defvar vmpc-sig-face (progn (make-face 'vmpc-sig-face
		"Face used for highlighting the signature.")
			     (set-face-foreground 'vmpc-sig-face 
						  "steelblue")
			     'vmpc-sig-face)
  "Face used for highlighting the signature.")

(defvar vmpc-intangible-pre-sig 'nil
  "Whether to forbid the cursor from entering the pre-signature.")

(defvar vmpc-intangible-sig 'nil
  "Whether to forbid the cursor from entering the signature.")

(defvar vmpc-xemacs-p
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      t
    nil)
  "Don't mess with this.
This variable is automatically initialised to t if p-crisis is
running under XEmacs; otherwise nil.  P-crisis checks its value when
it needs to know what flavour of Emacs this is.")

(defvar vmpc-expect-default-signature 'nil
  "*Set this to 't if you have a signature-inserting function hanging
off a hook that pre-empts Personality Crisis.")


;; -------------------------------------------------------------------
;; Some easter-egg functionality:
;; -------------------------------------------------------------------

(defun vmpc-header-field-for-point ()
  "*Returns a string indicating the mail header field point is in.  
If point is not in a header field, returns nil."
  (save-excursion
    (unless (save-excursion 
	      (re-search-backward (regexp-quote mail-header-separator)
				  (point-min) 't))
      (re-search-backward "^\\([^ \t\n:]+\\):")
      (match-string 1))))

(defun vmpc-tab-header-or-tab-stop (&optional backward)
  "*If in a mail header field, moves to next useful header or body.  
When moving to the message body, calls the `vmpc-automorph' function.
If within the message body, runs `tab-to-tab-stop'.  
If BACKWARD is specified and non-nil, moves to previous useful header
field, whether point is in the body or the headers.
\"Useful header fields\" are currently, in order, \"To\" and
\"Subject\"."
  (interactive)
  (let ((curfield) (nextfield) (useful-headers '("To" "Subject")))
    (if (or (setq curfield (vmpc-header-field-for-point))
	    backward)
	(progn
	  (setq nextfield
		(- (length useful-headers) 
		   (length (member curfield useful-headers))))
	  (if backward 
	      (setq nextfield (nth (1- nextfield) useful-headers))
	    (setq nextfield (nth (1+ nextfield) useful-headers)))
	  (if nextfield
	      (mail-position-on-field nextfield)
	    (mail-text)
	    (vmpc-automorph))
	  )
      (tab-to-tab-stop)
      )))

(defun vmpc-backward-tab-header-or-tab-stop ()
  "*Wrapper for `vmpc-tab-header-or-tab-stop' with BACKWARD set"
  (interactive)
  (vmpc-tab-header-or-tab-stop 't))


;; -------------------------------------------------------------------
;; Stuff for dealing with exerlays:
;; -------------------------------------------------------------------

(defun vmpc-set-overlay-insertion-types (overlay start end)
  "Creates a new copy of OVERLAY with different insertion types at
START and END; returns this overlay.  START and END should be nil or t
-- the marker insertion types at the start and end.  This seems to be
the only way you of changing the insertion types for an overlay --
save the overlay properties that we care about, create a new overlay
with the new insertion types, set its properties to the saved ones.
Overlays suck.  Extents rule.  XEmacs got this right."
  (let* ((useful-props (list 'face 'intangible 'evaporate)) (saved-props) 
	 (i 0) (len (length useful-props)) (startpos) (endpos) (new-ovl))
    (while (< i len)
      (setq saved-props (append saved-props (cons
		       (overlay-get overlay (nth i useful-props)) ())))
      (setq i (1+ i)))
    (setq startpos (overlay-start overlay))
    (setq endpos (overlay-end overlay))
    (delete-overlay overlay)
    (if (and startpos endpos)
	(setq new-ovl (make-overlay startpos endpos (current-buffer)
				    start end))
      (setq new-ovl (make-overlay 1 1 (current-buffer) start end))
      (vmpc-forcefully-detach-exerlay new-ovl))
    (setq i 0)
    (while (< i len)
      (overlay-put new-ovl (nth i useful-props) (nth i saved-props))
      (setq i (1+ i)))
    new-ovl))


(defun vmpc-set-extent-insertion-types (extent start end)
  "Set the insertion types for START and END of EXTENT.
START and END should be either nil or t, indicating the desired value
of the 'start-open and 'end-closed properties of the extent
respectively.  
This is the XEmacs version of `vmpc-set-overlay-insertion-types'."
  ;; pretty simple huh?
  (set-extent-property extent 'start-open start)
  (set-extent-property extent 'end-closed end))


(defun vmpc-set-exerlay-insertion-types (exerlay start end)
  "Sets the insertion types for the extent or overlay named by the
symbol EXERLAY.  In other words, EXERLAY is the name of the overlay or
extent with a quote in front.  START and END are the equivalent of the
marker insertion types for the start and end of the overlay/extent."
  (if vmpc-xemacs-p
      (vmpc-set-extent-insertion-types (symbol-value exerlay) start end)
    (set exerlay (vmpc-set-overlay-insertion-types (symbol-value exerlay)
						   start end))))


(defun vmpc-exerlay-start (exerlay)
  "Returns buffer position of the start of EXERLAY."
  (if vmpc-xemacs-p
      (extent-start-position exerlay)
    (overlay-start exerlay)))


(defun vmpc-exerlay-end (exerlay)
  "Returns buffer position of the end of EXERLAY."
  (if vmpc-xemacs-p
      (extent-end-position exerlay)
    (overlay-end exerlay)))


(defun vmpc-move-exerlay (exerlay new-start new-end)
  "Moves the start and end points of EXERLAY to the buffer positions
NEW-START and NEW-END respectively."
  (if vmpc-xemacs-p
      (set-extent-endpoints exerlay new-start new-end (current-buffer))
    (move-overlay exerlay new-start new-end (current-buffer))))


(defun vmpc-set-exerlay-detachable-property (exerlay newval)
  "Sets the 'detachable or 'evaporate property for EXERLAY to NEWVAL."
  (if vmpc-xemacs-p
      (set-extent-property exerlay 'detachable newval)
    (overlay-put exerlay 'evaporate newval)))


(defun vmpc-set-exerlay-intangible-property (exerlay newval)
  "Sets the 'intangible or 'atomic property for EXERLAY to NEWVAL."
  (if vmpc-xemacs-p
      (progn
	(require 'atomic-extents)
	(set-extent-property exerlay 'atomic newval))
    (overlay-put exerlay 'intangible newval)))


(defun vmpc-set-exerlay-face (exerlay newface)
  "Sets the face used by EXERLAY to NEWFACE."
  (if vmpc-xemacs-p
      (set-extent-face exerlay newface)
    (overlay-put exerlay 'face newface)))


(defun vmpc-forcefully-detach-exerlay (exerlay)
  "Leaves EXERLAY in memory but detaches it from the buffer."
  (if vmpc-xemacs-p
      (detach-extent exerlay)
    (delete-overlay exerlay)))


(defun vmpc-make-exerlay (startpos endpos)
  "Returns a newly created exerlay spanning from STARTPOS to ENDPOS in the
current buffer."
  (if vmpc-xemacs-p
      (make-extent startpos endpos (current-buffer))
    (make-overlay startpos endpos (current-buffer))))


(defun vmpc-create-sig-and-pre-sig-exerlays ()
  "Creates the extents in which the pre-sig and sig can reside.
Or overlays, in the case of GNUmacs.  Thus, exerlays."
  (setq vmpc-pre-sig-exerlay (vmpc-make-exerlay 1 2))
  (setq vmpc-sig-exerlay (vmpc-make-exerlay 3 4))

  (vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay t)
  (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay t)
  (vmpc-forcefully-detach-exerlay vmpc-pre-sig-exerlay)
  (vmpc-forcefully-detach-exerlay vmpc-sig-exerlay)

  (vmpc-set-exerlay-face vmpc-pre-sig-exerlay 'vmpc-pre-sig-face)
  (vmpc-set-exerlay-face vmpc-sig-exerlay 'vmpc-sig-face)

  (vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay
					vmpc-intangible-pre-sig)
  (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay
					vmpc-intangible-sig)
  
  (vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay t nil)
  (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay t nil)

  ;; deal with signatures inserted by other things than p-crisis:
  (if vmpc-expect-default-signature
      (save-excursion
	(let ((p-max (point-max))
	      (body-start (save-excursion (mail-text) (point)))
	      (sig-start nil))
	  (goto-char p-max)
	  (setq sig-start (re-search-backward "\n-- \n" body-start t))
	  (if sig-start
	      (vmpc-move-exerlay vmpc-sig-exerlay sig-start p-max))))))
  

;; -------------------------------------------------------------------
;; Functions for vmpc-actions:
;; -------------------------------------------------------------------

(defun vmpc-composition-buffer-function (func)
  "Runs a composition-buffer-function provided the time is right.
That is to say, runs the function if you're really in a composition
buffer.  This function should not be called directly; only from within
the vmpc-actions list."
  (if (eq vmpc-current-buffer 'composition)
      (eval func)))

(defun vmpc-pre-function (func)
  "Runs a pre-function provided the time is right.
That is to say, runs the function before VM does its thing, whether
that be creating a new mail or a reply.  This function should not be
called directly; only from within the vmpc-actions list."
  (if (and (eq vmpc-current-buffer 'none)
	   (not (eq vmpc-current-state 'automorph)))
      (eval func)))

(defun vmpc-delete-header (hdrfield &optional entire)
  "Delete the contents of a header field in the current mail message.
If ENTIRE is specified and non-nil, deletes the header field as well."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(let ((start) (end))
	  (mail-position-on-field hdrfield)
	  (if entire
	      (setq end (+ (point) 1))
	    (setq end (point)))
	  (re-search-backward ": ")
	  (if entire
	      (setq start (progn (beginning-of-line) (point)))
	    (setq start (+ (point) 2)))
	  (delete-region start end)))))


(defun vmpc-insert-header (hdrfield hdrcont)
  "Insert HDRCONT in HDRFIELD in the current mail message.  
Both arguments are strings.  The field can either be present or not,
but if present, HDRCONT will be appended to the current header
contents."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(mail-position-on-field hdrfield)
	(insert hdrcont))))

(defun vmpc-substitute-header (hdrfield hdrcont)
  "Insert HDRCONT in HDRFIELD in the current mail message.  
Both arguments are strings.  The field can either be present or not.
If the header field is present and already contains something, the
contents will be replaced with the new ones specified in HDRCONT."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(vmpc-delete-header hdrfield)
	(vmpc-insert-header hdrfield hdrcont))))

(defun vmpc-get-current-header-contents (hdrfield &optional clump-sep)
  "Returns the contents of HDRFIELD in the current mail message.
Returns an empty string if the header doesn't exist.  HDRFIELD should
be a string.  If the string CLUMP-SEP is specified, it means to return
the contents of all headers matching the regexp HDRFIELD, separated by
CLUMP-SEP."
  ;; This code is based heavily on vm-get-header-contents and vm-match-header.
  ;; Thanks Kyle :)
  (if (eq vmpc-current-state 'automorph)
      (save-excursion
	(let ((contents nil) (header-name-regexp "\\([^ \t\n:]+\\):")
	      (case-fold-search t) (temp-contents) (end-of-headers) (regexp))
	  ;; find the end of the headers:
	  (goto-char (point-min))
	  (re-search-forward 
	     (concat "^\\(" (regexp-quote mail-header-separator) "\\)$"))
	  (setq end-of-headers (match-beginning 0))
	  ;; now rip through finding all the ones we want:
	  (setq regexp (concat "^\\(" hdrfield "\\)"))
	  (goto-char (point-min))
	  (while (and (or (null contents) clump-sep)
		      (re-search-forward regexp end-of-headers t)
		      (save-excursion
			(goto-char (match-beginning 0))
			(let (header-cont-start header-cont-end)
			  (if (if (not clump-sep)
				  (and (looking-at hdrfield) 
				       (looking-at header-name-regexp))
				(looking-at header-name-regexp))
			      (save-excursion
				(goto-char (match-end 0))
				;; skip leading whitespace
				(skip-chars-forward " \t")
				(setq header-cont-start (point))
				(forward-line 1)
				(while (looking-at "[ \t]")
				  (forward-line 1))
				;; drop the trailing newline
				(setq header-cont-end (1- (point)))))
			  (setq temp-contents 
				(buffer-substring header-cont-start 
						  header-cont-end)))))
	    (if contents
		(setq contents
		      (concat contents clump-sep temp-contents))
	      (setq contents temp-contents)))

	  (if (null contents)
	      (setq contents ""))
	  contents ))))

(defun vmpc-get-current-body-text ()
  "Returns the body text of the mail message in the current buffer."
  (if (eq vmpc-current-state 'automorph)
      (save-excursion
	(goto-char (point-min))
	(let ((start (re-search-forward 
		      (concat "^" (regexp-quote mail-header-separator) "$")))
	      (end (point-max)))
	  (buffer-substring start end)))))


(defun vmpc-get-replied-header-contents (hdrfield &optional clump-sep)
  "Returns the contents of HDRFIELD in the message being replied to.
If that header does not exist, returns an empty string.  If the string
CLUMP-SEP is specified, treat HDRFIELD as a regular expression and
return the contents of all header fields which match that regexp,
separated from each other by CLUMP-SEP."
  (if (and (eq vmpc-current-buffer 'none)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend)))
      (let ((mp (car (vm-select-marked-or-prefixed-messages 1))))
	(or (vm-get-header-contents mp hdrfield clump-sep) ""))))

(defun vmpc-get-replied-body-text ()
  "Returns the body text of the message being replied to."
  (if (and (eq vmpc-current-buffer 'none)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend)))
      (save-excursion
	(let* ((mp (car (vm-select-marked-or-prefixed-messages 1)))
	       (message (vm-real-message-of mp))
	       start end)
	  (set-buffer (vm-buffer-of message))
	  (save-restriction
	    (widen)
	    (setq start (vm-text-of message))
	    (setq end (vm-end-of message))
	    (buffer-substring start end))))))

(defun vmpc-save-replied-header (hdrfield)
  "Saves the contents of HDRFIELD in `vmpc-saved-headers-alist'. 
Does nothing if that header doesn't exist."
  (let ((hdrcont (vmpc-get-replied-header-contents hdrfield)))
  (if (and (eq vmpc-current-buffer 'none)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend))
	   (not (equal hdrcont "")))
      (add-to-list 'vmpc-saved-headers-alist (cons hdrfield hdrcont)))))

(defun vmpc-get-saved-header (hdrfield)
  "Returns the contents of HDRFIELD from `vmpc-saved-headers-alist'.  
The alist in question is created by `vmpc-save-replied-header'."
  (if (and (eq vmpc-current-buffer 'composition)
	   (or (eq vmpc-current-state 'reply)
	       (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend)))
      (cdr (assoc hdrfield vmpc-saved-headers-alist))))

(defun vmpc-substitute-replied-header (dest src)
  "Inserts the contents of the header SRC in the message you're
replying to as the contents of the header DEST in your reply.  
For example, if the address you want to send your reply to is the same
as the contents of the \"From\" header in the message you're replying
to, you'd use (vmpc-substitute-replied-header \"To\" \"From\"."  
  (if (or (eq vmpc-current-state 'reply)
	  (eq vmpc-current-state 'forward)
          (eq vmpc-current-state 'resend))
      (progn
	(if (eq vmpc-current-buffer 'none)
	    (vmpc-save-replied-header src))
	(if (eq vmpc-current-buffer 'composition)
	    (vmpc-substitute-header dest (vmpc-get-saved-header src))))))

(defun vmpc-get-header-extents (hdrfield)
  "Return buffer positions (START . END) for the contents of HDRFIELD.
If HDRFIELD does not exist, return nil."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion 
        (let ((header-name-regexp "^\\([^ \t\n:]+\\):") (start) (end))
          (setq end 
                (if (mail-position-on-field hdrfield t) 
                    (point) 
                  nil))
          (setq start 
                (if (re-search-backward header-name-regexp (point-min) t)
                    (match-end 0)
                  nil))
          (and start end (<= start end) (cons start end))))))

(defun vmpc-substitute-within-header 
  (hdrfield regexp to-string &optional append-if-no-match sep)
  "Replace REGEXP in HDRFIELD with TO-STRING.  
HDRFIELD need not exist.  TO-STRING may contain references to groups
within REGEXP, in the same manner as `replace-regexp'.  If REGEXP is
not found in the header contents, and APPEND-IF-NO-MATCH is t,
TO-STRING will be appended to the header contents (with HDRFIELD being
created if it does not exist).  In this case, if the string SEP is
specified, it will be used to separate the previous header contents
from TO-STRING, unless HDRFIELD has just been created or was
previously empty."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
        (let ((se (vmpc-get-header-extents hdrfield)) (found))
          (if se
              ;; HDRFIELD exists
              (save-restriction
                (narrow-to-region (car se) (cdr se))
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  (setq found t)
                  (replace-match to-string))
                (if (and (not found) append-if-no-match)
                    (progn
                      (goto-char (cdr se))
                      (if (and sep (not (equal (car se) (cdr se))))
                          (insert sep))
                      (insert to-string))))
            ;; HDRFIELD does not exist
            (if append-if-no-match
                (progn
                  (mail-position-on-field hdrfield)
                  (insert to-string))))))))


(defun vmpc-insert-signature (sig &optional pos)
  "Inserts SIG at the end of `vmpc-sig-exerlay'.  SIG is a string.  If
it's the name of a file, the file's contents are inserted -- otherwise
the string itself is inserted.  Optional parameter POS means insert
the signature at POS if `vmpc-sig-exerlay' is detached."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	(let ((end (or (vmpc-exerlay-end vmpc-sig-exerlay) pos)))
	  (save-excursion
	    (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay nil t)
	    (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay nil)
	    (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay nil)
	    (unless end
	      (setq end (point-max))
	      (vmpc-move-exerlay vmpc-sig-exerlay end end))
	    (if (and pos (not (vmpc-exerlay-end vmpc-sig-exerlay)))
		(vmpc-move-exerlay vmpc-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n-- \n")
	    (if (and (file-exists-p sig)
		     (file-readable-p sig)
		     (not (equal sig "")))
		(insert-file-contents sig)
	      (insert sig)))
	  (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay
						vmpc-intangible-sig)
	  (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay t)
	  (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay t nil)))))
    

(defun vmpc-delete-signature ()
  "Deletes the contents of `vmpc-sig-exerlay'."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	;; make sure it's not detached first:
	(if (vmpc-exerlay-start vmpc-sig-exerlay)
	    (progn
	      (delete-region (vmpc-exerlay-start vmpc-sig-exerlay)
			     (vmpc-exerlay-end vmpc-sig-exerlay))
	      (vmpc-forcefully-detach-exerlay vmpc-sig-exerlay))))))


(defun vmpc-signature (sig)
  "Removes a current signature if present, and replaces it with SIG.
If the string SIG is the name of a readable file, its contents are
inserted as the signature; otherwise SIG is inserted literally.  If
SIG is the empty string (\"\"), the current signature is deleted if
present, and that's all."
  (if (eq vmpc-current-buffer 'composition)
      (let ((pos (vmpc-exerlay-start vmpc-sig-exerlay)))
	(save-excursion
	  (vmpc-delete-signature)
	  (if (not (equal sig ""))
	      (vmpc-insert-signature sig pos))))))
  

(defun vmpc-insert-pre-signature (pre-sig &optional pos)
  "Inserts PRE-SIG at the end of `vmpc-pre-sig-exerlay'.  
PRE-SIG is a string.  If it's the name of a file, the file's contents
are inserted; otherwise the string itself is inserted.  Optional
parameter POS means insert the pre-signature at position POS if
`vmpc-pre-sig-exerlay' is detached."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	(let ((end (or (vmpc-exerlay-end vmpc-pre-sig-exerlay) pos))
	      (sigstart (vmpc-exerlay-start vmpc-sig-exerlay)))
	  (save-excursion
	    (vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay nil t)
	    (vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay nil)
	    (vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay nil)
	    (unless end
	      (if sigstart
		  (setq end sigstart)
		(setq end (point-max)))
	      (vmpc-move-exerlay vmpc-pre-sig-exerlay end end))
	    (if (and pos (not (vmpc-exerlay-end vmpc-pre-sig-exerlay)))
		(vmpc-move-exerlay vmpc-pre-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n")
	    (if (and (file-exists-p pre-sig)
		     (file-readable-p pre-sig)
		     (not (equal pre-sig "")))
		(insert-file-contents pre-sig)
	      (insert pre-sig))))
	(vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay
					      vmpc-intangible-pre-sig)
	(vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay t)
	(vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay t nil))))


(defun vmpc-delete-pre-signature ()
  "Deletes the contents of `vmpc-pre-sig-exerlay'."
  ;; make sure it's not detached first:
  (if (eq vmpc-current-buffer 'composition)
      (if (vmpc-exerlay-start vmpc-pre-sig-exerlay)
	  (progn
	    (delete-region (vmpc-exerlay-start vmpc-pre-sig-exerlay)
			   (vmpc-exerlay-end vmpc-pre-sig-exerlay))
	    (vmpc-forcefully-detach-exerlay vmpc-pre-sig-exerlay)))))


(defun vmpc-pre-signature (pre-sig)
  "As for `vmpc-insert-pre-signature', except that the last pre-sig
added is removed before the new one is added."
  (if (eq vmpc-current-buffer 'composition)
      (let ((pos (vmpc-exerlay-start vmpc-pre-sig-exerlay)))
	(save-excursion
	  (vmpc-delete-pre-signature)
	  (if (not (equal pre-sig ""))
	      (vmpc-insert-pre-signature pre-sig pos))))))


(defun vmpc-gregorian-days ()
  "Returns the number of days elapsed since December 31, 1 B.C."
  ;; this code stolen from gnus-util.el :)
  (let ((tim (decode-time (current-time))))
    (timezone-absolute-from-gregorian
     (nth 4 tim) (nth 3 tim) (nth 5 tim))))


(defun vmpc-load-auto-profiles ()
  "Initialise vmpc-auto-profiles from vmpc-auto-profiles-file."
  (if (and (file-exists-p vmpc-auto-profiles-file)
	   (file-readable-p vmpc-auto-profiles-file))
      (save-excursion
	(set-buffer (get-buffer-create "*pcrisis-temp*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(insert-file-contents vmpc-auto-profiles-file)
	(goto-char (point-min))
	(setq vmpc-auto-profiles (read (current-buffer)))
	(kill-buffer (current-buffer)))))


(defun vmpc-save-auto-profiles ()
  "Save vmpc-auto-profiles to vmpc-auto-profiles-file."
  (if (file-writable-p vmpc-auto-profiles-file)
      (save-excursion
	(set-buffer (get-buffer-create "*pcrisis-temp*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)      
	(goto-char (point-min))
	(prin1 vmpc-auto-profiles (current-buffer))
	(write-region (point-min) (point-max)
		      vmpc-auto-profiles-file nil 'quietly)
	(kill-buffer (current-buffer)))
    ;; if file is not writable, signal an error:
    (error "Error: P-Crisis could not write to file %s"
	   vmpc-auto-profiles-file)))

(defun vmpc-fix-auto-profiles-file ()
  "Change `vmpc-auto-profiles-file' to the format used by v0.82+"
  (interactive)
  (vmpc-load-auto-profiles)
  (let ((len (length vmpc-auto-profiles)) (i 0) (day))
    (while (< i len)
      (setq day (cddr (nth i vmpc-auto-profiles)))
      (if (consp day)
	  (setcdr (cdr (nth i vmpc-auto-profiles)) (car day)))
      (setq i (1+ i))))
  (vmpc-save-auto-profiles)
  (setq vmpc-auto-profiles ()))


(defun vmpc-get-profile-for-address (addr)
  "This is a support function for vmpc-prompt-for-profile."
  (unless vmpc-auto-profiles
    (vmpc-load-auto-profiles))
  (let ((prof (cadr (assoc addr vmpc-auto-profiles))))
    (if prof
	(let ((today (vmpc-gregorian-days)))
	  ;; if we found for a profile for this address, we are still
	  ;; using it -- so "touch" the record to ensure it stays
	  ;; newer than vmpc-auto-profiles-expunge-days:
	  (setcdr (cdr (assoc addr vmpc-auto-profiles)) today)
	  (vmpc-save-auto-profiles)))
    prof ))


(defun vmpc-save-profile-for-address (prof addr)
  "This is a support function for vmpc-prompt-for-profile."
  (let ((today) (len (length vmpc-auto-profiles)) (i 0))
    (setq today (vmpc-gregorian-days))
    (add-to-list 'vmpc-auto-profiles (append (list addr prof) today))
    (if vmpc-auto-profiles-expunge-days
	;; expunge old stuff from the list:
	(while (< i len)
	  (if (> (- today (cddr (nth i vmpc-auto-profiles)))
		 vmpc-auto-profiles-expunge-days)
	      (delete (nth i vmpc-auto-profiles) vmpc-auto-profiles))
	  (setq i (1+ i))))
    (vmpc-save-auto-profiles)))


(defun vmpc-string-extract-address (str)
  "Finds the first email address in the string STR and returns it.
If no email address in found in STR, returns nil."
  (if (string-match "[^ 	,<]+@[^ 	,>]+" str)
      (match-string 0 str)))


(defun vmpc-prompt-for-profile (&optional remember)
  "Prompts the user for a profile (one of the sets of actions named in
vmpc-actions) and adds it to the list of actions to be performed,
unless it's already in there.  
REMEMBER can be set to 'always or 'prompt.  It figures out who your
message is going to, and saves a record in vmpc-auto-profiles-file
which says to use that profile for messages to that address in the
future, instead of prompting you for a profile the next time.  If set
to 'prompt, it will ask before doing this; otherwise it will do it
automatically."
  (if (and (or (eq vmpc-current-state 'forward)
               (eq vmpc-current-state 'resend))
	   remember)
      (error "You can't have vmpc-prompt-for-profile remember when forwarding or resending."))
  (if (or (and (eq vmpc-current-buffer 'none)
	       (not (eq vmpc-current-state 'automorph)))
	  (eq vmpc-current-state 'automorph))
      (let ((prof) (dest ""))
	;; figure out where this email is going:
	(cond 
	 ((eq vmpc-current-state 'automorph)
	  (setq dest (vmpc-get-current-header-contents "To")))
	 ((and (eq vmpc-current-state 'reply)
	       (eq vmpc-current-buffer 'none))
	  (setq dest (vmpc-get-replied-header-contents "Reply-To"))
	  (if (or (vm-ignored-reply-to dest)
		  (equal dest ""))
	      (setq dest (vmpc-get-replied-header-contents "From")))))
	;; just the email address thanks:
	(unless (eq vmpc-current-state 'newmail)
	  (setq dest (vmpc-string-extract-address dest)))
	;; figure out which profile to use:
	(setq prof
	      (or (let ((p))
		   (setq p (vmpc-get-profile-for-address dest))
		   (if p
		       (setq remember 'already))
		   p)
		  (let ((default (car (car vmpc-actions))) (choice nil))
		    (setq choice (completing-read 
				  (format "Use profile (Default \"%s\"): "
					  default) vmpc-actions nil t))
		    (if (equal choice "")
			default
		      choice))))

	(if (not (eq vmpc-current-state 'automorph))
	    ;; add it to the end of the list:
	    (if (not (member prof vmpc-actions-to-run))
		(setq vmpc-actions-to-run (append vmpc-actions-to-run
						  (cons prof ()))))
	  ;; or in automorph, run it immediately:
	  (let* ((functions (cdr (assoc prof vmpc-actions)))
		 (len (length functions)) (i 0))
	    (while (< i len)
	      (eval (nth i functions))
	      (setq i (1+ i)))))
	
	;; save the association of this profile with this destination
	;; address if applicable:
	(if (or (and (eq remember 'prompt)
		     (y-or-n-p (format "Always use \"%s\" for \"%s\"? "
				       prof dest)))
		(eq remember 'always))
	    (vmpc-save-profile-for-address prof dest)
	    ))))

;; -------------------------------------------------------------------
;; Functions for vmpc-conditions:
;; -------------------------------------------------------------------

(defun vmpc-none-true-yet (&optional &rest exceptions)
  "This is a condition that can appear in vmpc-conditions.  It means that none
of the previous conditions matched.  If EXCEPTIONS are specified, it means none
were true except those.  For example, if you wanted to check whether no
conditions had yet matched with the exception of the two conditions named
\"default\" and \"blah\", you would make the call like this:
  (vmpc-none-true-yet \"default\" \"blah\")
Then it will return true regardless of whether \"default\" and \"blah\" had 
matched."
  (let ((lenex (length exceptions)) (lentc (length vmpc-true-conditions)))
    (cond 
     ((> lentc lenex)
      'nil)
     ((<= lentc lenex)
      (let ((i 0) (j 0) (k 0))
	(while (< i lenex)
	  (setq k 0)
	  (while (< k lentc)
	    (if (equal (nth i exceptions) (nth k vmpc-true-conditions))
		(setq j (1+ j)))
	    (setq k (1+ k)))
	  (setq i (1+ i)))
	(if (equal j lentc)
	    't
	  'nil))))))

(defun vmpc-other-cond (condition)
  "Returns true if the specified CONDITION in vmpc-conditions matched.
CONDITION can only be the name of a condition specified earlier in
vmpc-conditions -- that is to say, any conditions which follow the one
containing vmpc-other-cond will show up as not having matched, because they
haven't yet been checked when this one is checked."
  (member condition vmpc-true-conditions))


(defun vmpc-header-match (hdrfield regexp &optional clump-sep)
  "Returns true if the contents of specified header HDRFIELD match REGEXP.  
For automorph, this means the header in your message; when replying it means the 
header in the message being replied to."
  (cond ((or (eq vmpc-current-state 'reply)
	     (eq vmpc-current-state 'forward)
             (eq vmpc-current-state 'resend))
	 (string-match regexp (vmpc-get-replied-header-contents
			       hdrfield clump-sep)))
	((eq vmpc-current-state 'automorph)
	 (string-match regexp (vmpc-get-current-header-contents 
			       hdrfield clump-sep)))))

(defun vmpc-body-match (regexp)
  "Returns non-nil if the contents of the message body match REGEXP.
For automorph, this means the body of your message; when replying it means the
body of the message being replied to."
  (cond ((and (or (eq vmpc-current-state 'reply)
		  (eq vmpc-current-state 'forward)
                  (eq vmpc-current-state 'resend))
	      (eq vmpc-current-buffer 'none))
	 (string-match regexp (vmpc-get-replied-body-text)))
	((eq vmpc-current-state 'automorph)
	 (string-match regexp (vmpc-get-current-body-text)))))


(defun vmpc-xor (a b &optional &rest cdetc)
  "A generic xor function.  If one and only one argument is true, returns 't."
  (let ((len (length cdetc)) (count 0) (i 0))
    (if a (setq count (1+ count)))
    (if b (setq count (1+ count)))
    (while (< i len)
      (if (nth i cdetc)
	  (setq count (1+ count)))
      (setq i (1+ i)))
    (if (equal count 1)
	't
      'nil )))

;; -------------------------------------------------------------------
;; Support functions for the advices:
;; -------------------------------------------------------------------

(defun vmpc-build-true-conditions-list ()
  (let ((len (length vmpc-conditions)) (i 0))
    (while (< i len)
      (if (eval (car (cdr (nth i vmpc-conditions))))
	  (setq vmpc-true-conditions (append vmpc-true-conditions
		     (cons (car (nth i vmpc-conditions)) ()))))
      (setq i (1+ i)))))

(defun vmpc-build-actions-to-run-list ()
  (let ((alist) (i 0) (actions) (len))
    (cond
     ((eq vmpc-current-state 'automorph) (setq alist vmpc-automorph-alist))
     ((eq vmpc-current-state 'reply) (setq alist vmpc-replies-alist))
     ((eq vmpc-current-state 'forward) (setq alist vmpc-forwards-alist))
     ((eq vmpc-current-state 'newmail) (setq alist vmpc-newmail-alist))
     ((eq vmpc-current-state 'resend) (setq alist vmpc-resend-alist)))
    (setq len (length vmpc-true-conditions))
    (while (< i len)
      (setq actions (cdr (assoc (nth i vmpc-true-conditions) alist)))
      (if actions
	  (setq vmpc-actions-to-run (append vmpc-actions-to-run actions)))
      (setq i (1+ i)))))

(defun vmpc-run-actions ()
  (let ((len (length vmpc-actions-to-run)) (i 0))
    (while (< i len)
      (let* ((functions (cdr (assoc (nth i vmpc-actions-to-run) vmpc-actions))) 
	     (len2 (length functions)) (j 0))
	(while (< j len2)
	  (eval (nth j functions))
	  (setq j (1+ j)))
	(setq i (1+ i))))))


;; ------------------------------------------------------------------------
;; The main functions and advices -- these are the entry points to pcrisis:
;; ------------------------------------------------------------------------

(defadvice vm-do-reply (around vmpc-reply activate)
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'reply
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

(defadvice vm-mail (around vmpc-newmail activate)
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'newmail
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

(defadvice vm-forward-message (around vmpc-forward activate)
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  ;; the rest is almost exactly the same as replying:
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'forward
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

(defadvice vm-resend-message (around vmpc-resend activate)
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  ;; the rest is almost exactly the same as replying:
  (setq vmpc-saved-headers-alist ()
	vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'resend
	vmpc-current-buffer 'none)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (setq vmpc-current-buffer 'composition)
  (vmpc-run-actions))

;;;###autoload
(defun vmpc-automorph ()
  "*Changes contents of the current mail message based on its own headers.
Headers and signatures can be changed; pre-signatures added; functions called.
For more information, see the Personality Crisis info file."
  (interactive)
  (setq vmpc-actions-to-run ()
	vmpc-true-conditions ())
  (setq vmpc-current-state 'automorph
	vmpc-current-buffer 'composition)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions))

(provide 'vm-pcrisis)

;;; vm-pcrisis.el ends here
