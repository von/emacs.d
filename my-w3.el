;;
;; my-w3.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; W3 is a browser for the World Wide Web, and takes advantage of the very
;;; latest redisplay features in XEmacs.  You can access it simply by typing 
;;; 'M-x w3'; however, if you're unlucky enough to be on a machine that is 
;;; behind a firewall, you will have to do something like this first:

;; Delay loading images
(setq

 ;;; Broken at the moment
 w3-delay-image-loads t

 w3-default-homepage "http://computer.ncsa.uiuc.edu/Von"

 w3-hotlist-file "~/www/bookmarks.html"

 ;;; Don't let documents set background color
 w3-user-colors-take-precedence t

 ;;; Don't let documents set font
 w3-user-fonts-take-precedence t
 )

(defun vons-w3-init()
  "My W3 initialization."

  (delete-menu-item '("Edit"))
  )

(add-hook 'w3-mode-hook 'vons-w3-init)
