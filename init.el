;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; $Id$
;;;

;;; This file is meant to be used as .emacs and load the rest of my
;;; configuration.
;;;
;;;----------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up Emac's load path

;; Use directory-file-name here to strip any trailing directory
;; separator
(defvar home (directory-file-name (user-home-directory))
  "My home directory.")

;; Prepend load stuff...
(setq directory-sep-string (char-to-string directory-sep-char))

(setq my-lib-dir (concat home directory-sep-string "lib"))

(setq my-emacs-config-dir (concat home
				  directory-sep-string
				  ".xemacs"
				  directory-sep-string))
(if (file-accessible-directory-p my-emacs-config-dir)
     (setq load-path (cons my-emacs-config-dir load-path))
)

(setq my-lisp-dir (concat my-lib-dir directory-sep-string "lisp"))
(if (file-accessible-directory-p my-lisp-dir)
   (setq load-path (cons my-lisp-dir load-path))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define some basic functions for determining things about
;; our environment
;;

(defvar is-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Are we running xemacs?")

(defvar is-gnu-emacs (string-match "GNU" (emacs-version))
  "Are we running GNU emacs?")

(defvar is-ms-windows (string= "windows-nt" (symbol-name system-type))
  "Are we running under MS windows?")

(defvar is-x-windows (eq window-system 'x)
  "Are we running under X windows?")


;;; Older versions of emacs did not have these variables
;;; (emacs-major-version and emacs-minor-version.)
;;; Let's define them if they're not around, since they make
;;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))

;; Get the short form of our hostname
(setq hostname (if (eq (string-match "\\." (system-name)) 0)
		   (system-name)
		 (substring (system-name) 0 (string-match "\\." (system-name)))
		 )
      )

(if is-x-windows (progn 
		   (setq x-display (getenv "DISPLAY"))
		   (setq x-display-host
			 (substring x-display 0 (string-match ":" x-display)))
		   (if (string= x-display-host "")
		       (setq x-display-host (system-name)))
		   )
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Options
;;

(defvar modify-menu is-xemacs
	"Should we modify the toolbar menus?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enable the commands `narrow-to-region' ("C-x n n") and 
;; `eval-expression' ("M-ESC", or "ESC ESC").  Both are useful
;; commands, but they can be confusing for a new user, so they're
;; disabled by default.

(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;			Support routines
;;;

(defun make-menubar-local()
  "Make the menubar of the current buffer local."

  (set-buffer-menubar (copy-sequence current-menubar))
)

(defun set-buffer-frame-title-format(format)
  "Set the frame title for this buffer."

  (make-local-variable 'frame-title-format)
  (setq frame-title-format format)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Customization of Specific Packages		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load all the my-*.el files in my config dir
(mapcar
 (function
  (lambda (file)
    (load-file file)
    ))
 (directory-files my-emacs-config-dir t "^my-.*\.el$" nil t)
)

;; Don't indent all lines of paragraph like first
(setq adaptive-fill-mode nil)

;; Do auto-fill in text mode
(toggle-text-mode-auto-fill)

;; Don't use tabs by default
(setq indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous stuff
;;

(setq auto-mode-alist
      (append '(("\\.shtml$" . html-mode)
		("buildall.conf" . sh-mode)
		)
	      auto-mode-alist))



(defvar gmake-directory ""
  "Directory to run gmake in.")

(defun compile-with-gmake(dir)
  "Run compile with gmake in some directory."
  
  (interactive 
   (list (read-directory-name "gmake directory: " gmake-directory)))

  (setq gmake-directory dir)
  (make-local-variable 'compile-command)
  (setq compile-command (concat "(cd " gmake-directory "; gmake -k )"))
  (compile compile-command)
)

(global-set-key "\C-\M-m" 'compile-with-gmake)

;; Put directory component in buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Turn on permanent buffers
(permanent-buffers-mode t)

;; Turn on paren highlighting
(paren-set-mode 'paren)

;; Make wsdl files invoke xml mode
(setq auto-mode-alist
      (append '(("\\.wsdl" . xml-mode)
		)
	      auto-mode-alist))


(setq minibuffer-max-depth nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Printer stuff

(setq ps-print-color-p nil)
(setq printer-name "//cab-server/clw14 basement")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dictionary support

;; Load support for dictionary lookups
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)

(global-set-key [(control c) s] 'dictionary-search)
(global-set-key [(control c) m] 'dictionary-match-words)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MS Windows Stuff
;;

(if is-ms-windows
    (progn
      (setq completion-ignore-case t)
      ;; This also effects email alias completions
      ;;(setq completion-regexp-list
      ;;	    (append '("regex")
      ;;		    completion-regexp-list))
      ))

