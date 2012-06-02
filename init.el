;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; This file is meant to be used as .emacs and load the rest of my
;;; configuration.
;;;
;;;----------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enter debugger on any error in this file
;;

(setq debug-on-error t)

;; Set up my load-path
(load (expand-file-name "~/emacs-config/setup-load-path"))

;; Compile all my configuration files that need it
(byte-recompile-directory
 (expand-file-name "~/emacs-config/")
 0 ;; Compile any not compiled
 )

;; And recompile any of my packages that need it
(byte-recompile-directory
 (expand-file-name "~/lib/lisp/")
 0 ;; Compile any not compiled
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up exec path

(add-to-list 'exec-path "/usr/local/bin" t)
(add-to-list 'exec-path "/usr/local/git/bin" t)
(add-to-list 'exec-path "/sw/bin" t)

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

(defvar is-darwin (string= "darwin" (symbol-name system-type))
  "Are we running under Darwin/Mac OS?")

;;; Older versions of emacs did not have these variables
;;; (emacs-major-version and emacs-minor-version.)
;;; Let's define them if they're not around, since they make
;;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
         (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
          (string-to-number (substring emacs-version
                                       (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
         (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
          (string-to-number (substring emacs-version
                                       (match-beginning 1) (match-end 1)))))

;; Get the short form of our hostname
(setq hostname (if (eq (string-match "\\." (system-name)) 0)
                   (system-name)
                 (substring (system-name) 0 (string-match "\\." (system-name)))
                 )
      )

(if is-x-windows (progn
                   (defvar x-display (getenv "DISPLAY")
                     "X display")
                   (defvar x-display-host
                         (substring x-display 0 (string-match ":" x-display))
                         "X host")
                   (if (string= x-display-host "")
                       (setq x-display-host (system-name)))
                   )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;			Support routines
;;;

(defun make-menubar-local()
  "Make the menubar of the current buffer local."

  ;; XXX Fix for Emacs
  (if (fboundp 'set-buffer-menubar)
      (set-buffer-menubar (copy-sequence current-menubar)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Options
;;

(defvar modify-menu t
	"Should we modify the toolbar menus?")

(defvar my-use-vm nil
  "Configure VM?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load all the my-*.el files in my config dir
;;

(mapc
 (function
  (lambda (file)
    (load-file file)
    ))
 (directory-files my-emacs-config-dir t "^my-.*\.el$" nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous stuff
;;

;; Prompt when loading files larger than this value
(setq large-file-warning-threshold 100000000) ; 100 MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load platform-specific configuration
;;

;; MS Windows
(if is-ms-windows
    (load (expand-file-name "~/emacs-config/ms-windows"))
  )

;; GNU Emacs
(if is-gnu-emacs
    (load (expand-file-name "~/emacs-config/ms-windows"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Don't ask about killing process on exit
;; Kudos: http://stackoverflow.com/a/2706660/197789

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; End entering debugger on any error in this file
;;

(setq debug-on-error nil)

;;
;; End init.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
