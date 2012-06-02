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
;;		Customization of Specific Packages		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load all the my-*.el files in my config dir
(mapc
 (function
  (lambda (file)
    (load-file file)
    ))
 (directory-files my-emacs-config-dir t "^my-.*\.el$" nil)
)

;; Don't indent all lines of paragraph like first
(setq adaptive-fill-mode nil)

;; Do auto-fill in text mode
(toggle-text-mode-auto-fill)

;; Prompt when asking files larger than this value
(setq large-file-warning-threshold 100000000) ; 100 MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Desktop saving configuration
;;
;; Saves all the open files when exiting for restoration on next start.

;; Turn on desktop saving
(desktop-save-mode t)

;; Always save, don't ask
(setq desktop-save t)

;; Where to save desktop file
(setq desktop-dirname home)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Egg

(if (eq emacs-major-version 22)
    (require 'dominating-file))

(require 'egg)

;; Hack: Invoke egg- methods instead of vc- methods
;;       Not sure why this is needed
(global-set-key "\C-xvs" 'egg-status)
(global-set-key "\C-xvl" 'egg-log)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Markdown

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(set-variable 'markdown-command "Markdown.pl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mako
;; https://bitbucket.org/pjenvey/mmm-mako

(require 'mmm-mako)

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

;; XXX - Mako mode doesn't quite work, Emacs doesn't recognize
;;       opening Mako blocks and gets confused complaining about
;;       mismatched tags.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell
;;
;; Requests aspell. On a mac, install with:
;;   sudo port install aspell
;;   sudo port install aspell-dict-en

(add-hook 'text-mode-hook 'flyspell-mode)

;;* flyspell comments and strings in programming modes
;; From: "Stefan Monnier <foo @ acm.com>"

(defun flyspell-generic-progmode-verify ()
   "Used for `flyspell-generic-check-word-p' in programming modes."
   (let ((f (get-text-property (point) 'face)))
     (memq f '(font-lock-comment-face font-lock-string-face))))

(defun flyspell-prog-mode ()
   "Turn on `flyspell-mode' for comments and strings."
   (interactive)
   (setq flyspell-generic-check-word-p 'flyspell-generic-progmode-verify)
   (flyspell-mode 1))

(add-hook 'c-mode-common-hook         'flyspell-prog-mode t)
(add-hook 'java-mode-common-hook         'flyspell-prog-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ipython
;;

;; This breaks tab indentation in nxml
;;(require 'ipython)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous stuff
;;

;; Prompt when loading files larger than this value
(setq large-file-warning-threshold 100000000) ; 100 MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dictionary support

(load "dictionary-init")

(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start GnuServ
;;

(if is-gnu-emacs
    ;; For GNU Emacs use server-start and emacsclient
    ;; To use gnuserv with gnu-emaacs I have used the following before
    ;;(require 'gnuserv-compat))
    (server-start)
  ;; For XEmacs, use gnuserv-start and gnudoit
  (progn
    (require 'gnuserv)
    (if (fboundp 'gnuserv-start)
	(gnuserv-start)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load nxhtml mode

(load "nxhtml/autostart.el")

;; Stop nxhtml from messing with my background
;; Kudos: http://stackoverflow.com/a/3042760/197789
(setq mumamo-background-colors nil)

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
