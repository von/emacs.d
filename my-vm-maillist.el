;;
;; my-vm-maillist.el
;;
;; Stuff for walking through sorted maillists.
;; I haven't used this in a while.
;;
;; These functions based off of originals by
;; Neal Young <Neal.Young@dartmouth.edu>
;; found at
;; http://www.cs.hmc.edu/~smikes/emacs/vm-procmail.html

(defvar vm-maillists-directory "maillists/"
  "Directories were maillist spool files are kept in order they should be visited")

(defvar vm-maillist-folders '(
			       "hpc-wire"
			       "dsstar"
			       "globus-announce"
			       "grid-forum-announce"
			       "access-online"
			       "headline-news"
			       "crypto-grams"
			       "globus-security"
			       "grid-forum-security-wg"
			       "globus-gsiftp"
			       "globus-developers"
			       "gsi-wg"
			       "vmr-wg"
			       "krbdev"
			       "krb-protocol"
			       "nev-dull"
			       "emerge-tech"
			       "linux-users"
			       "thinkpad"
			       "ncsa-security"
			       "ncsa-irst"
			       "comppol"
			       "globus-discuss"
			       "globus-cray"
			       "hpcportals"
			       "portals"
			       "ietf-krb-wg"
			       "kerberos"
			       "java-kerberos"
			       "pc-kerberos"
			       "grid-forum-accounts-wg"
			       "grid-forum-data-wg"
			       "globus-support"
			       "cic-swg"
			       "cic-rpg"
			       "first-teams"
			       "xemacs"
			       "fvwm"
			       "wuftpd"
			       "perl"
			       "ssh-afs"
			       "hippi"
			       "vbns-techs"
			       "cam-reports"
			       )
  "Folders to visit with vm-visit-maillists")

(defun vm-add-maillists-menu()
  "Add a menu for maillists"
  ;; add-submenu() unfortunately just keeps appending to the same menu
  ;; so we'll delete and add each item individually.
  (interactive)
  (require 'vm)
  (vm-session-initialization)
;;  (add-submenu nil '("Maillists" :filter vm-maillists-menu-filter))
  (mapcar
   (function
    (lambda(folder)
      (let ((path '("Maillists" folder))
	    (item (vector
		   folder
		   (list 'vm-visit-maillist folder)
		   ':included
		   (list 'vm-maillist-mail-waiting-p folder)
		   )
		  )
	    )
	(delete-menu-item path)
	(add-menu-button '("Maillists") item)
	)
      )
    )
   vm-maillist-folders
   )
)

(defun vm-generate-maillists-menu(list)
  "Generate a maillist menu from the given list."
  (mapcar
   (function
    (lambda(item)
      (if (listp item)
	  (list (car item) (vm-generate-maillists-menu (cdr item)))
	(let ((entry (vector
		      item
		      (list 'vm-visit-maillist item)
		      ':active
		      (list 'vm-maillist-mail-waiting-p item)
		      )
		     )
	      )
	  entry
	  )
	)
      )
    )
   list
   )
  )

(defun vm-visit-maillist(maillist)
  "Visit a maillist"
  (vm-visit-folder (concat vm-maillists-directory maillist))
)

(defun vm-maillist-folder(maillist)
  "Given a maillist, return it's folder."
  (concat vm-maillists-directory maillist)
)

(defun vm-maillist-spool-file(maillist)
  "Given a maillist, return it's spool file."
  (concat vm-folder-directory (vm-maillist-folder maillist) ".spool")
)

(defun vm-visit-maillists()
  "Visit all the maillists with mail waiting."
  (interactive)
  (require 'vm)
  (vm-session-initialization)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil)
	(folder
	 (let* ((waiting (vm-maillist-spool-files-with-mail-waiting))
		(default (car waiting))
		(table (mapcar 'list waiting))
		(predicate nil)
		(require-match nil)
		(initial-contents (and default (cons default 0))
				  (history '(waiting . 0))
		)
		(completing-read "Run vm on folder: " table
			    predicate require-match initial-contents
			    history)
	   )
	   ))
	(vm-visit-maillist folder))
    )
  )


(defun vm-maillist-spool-files-with-mail-waiting()
  (let ((spool-files (mapcar
		     (function
		      (lambda (folder)
			(vm-maillist-spool-file folder)
			)
		      )
		     vm-maillist-folders
		     ))
	)
	(delete nil
		(mapcar
		 (function
		  (lambda (file)
		    (if (vm-mail-waiting-p file)
			(replace-in-string (file-name-nondirectory file)
					   ".spool$" "")
		      )
		    )
		  )
		 spool-files
		 )
		)
	)
  )

(defun vm-maillists-menu-filter(list)
  (delete nil
	  (mapcar
	   (function
	    (lambda (vec)
	      (if (vm-maillist-mail-waiting-p (elt vec 0))
		  vec
		)

	      )
	    )
	   list
	   )
	  )
  )

;; Not used
(defun vm-maillist-spool-files()
  (directory-files
   (concat vm-folder-directory vm-maillist-spool-directory)
   t ;; Full path
   ".spool$" ;; Regex to match
   t ;; don't sort
   t ;; files only
   )
)

(defun vm-maillist-mail-waiting-p (maillist)
  (vm-mail-waiting-p (vm-maillist-spool-file maillist))
)


(provide 'my-vm-maillist)
