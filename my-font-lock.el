;;
;; my-font-lock.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;l

;;; ********************
;;; Font-Lock is a syntax-highlighting package.  When it is enabled and you
;;; are editing a program, different parts of your program will appear in
;;; different fonts or colors.  For example, with the code below, comments
;;; appear in red italics, function names in function definitions appear in
;;; blue bold, etc.  The code below will cause font-lock to automatically be
;;; enabled when you edit C, C++, Emacs-Lisp, and many other kinds of
;;; programs.
;;;
;;;
(cond (is-xemacs
       ;;
       ;; Don't use default stuff. These must come before font-lock
       ;; is loaded.
       ;;
       ;;(setq-default font-lock-use-colors nil)
       ;;(setq-default font-lock-use-fonts nil)

       (require 'font-lock)
       )

      (is-gnu-emacs
       (global-font-lock-mode 1)
       )
      )

;;; Set colors and fonts
;;; Windows note: Windows craps out on some fonts if we don't copy them
;;;               first. *shrug*

(cond (t
       ;;
       ;;	COLORS
       ;;
       ;; Standard text
       (set-face-foreground 'default "white")
       (set-face-background 'default "black")
       (set-cursor-color "red")
       ;;
       ;; Modeline
       (set-face-foreground 'modeline "black")
       (set-face-background 'modeline "grey")
       ;;
       ;; Bold
       (copy-face 'default 'bold)
       (set-face-foreground 'bold "gold")
       ;;
       ;; Bold-italic
       (copy-face 'default 'bold-italic)
       (set-face-foreground 'bold-italic "gold")
       ;;
       ;; Italic
       (copy-face 'default 'italic)
       (set-face-foreground 'italic "green")
       ;;
       ;; Function names
       (copy-face 'default 'font-lock-function-name-face)
       (set-face-foreground 'font-lock-function-name-face "yellow")
       ;;
       ;; Preprocessor variable names
       (copy-face 'default 'font-lock-variable-name-face)
       (set-face-foreground 'font-lock-variable-name-face "orange")
       ;;
       ;; Comments
       (copy-face 'default 'font-lock-comment-face)
       (set-face-foreground 'font-lock-comment-face "cyan")
       ;;
       ;; String
       (copy-face 'default 'font-lock-string-face)
       (set-face-foreground 'font-lock-string-face "grey")
       ;;
       ;; Keywords (case statements)
       (copy-face 'default 'font-lock-keyword-face)
       (set-face-foreground 'font-lock-keyword-face "yellow")
       ;;
       ;; Stuff in HTML
       (copy-face 'default 'font-lock-type-face)
       (set-face-foreground 'font-lock-type-face "green")
       ;;
       ;; Highlight background
       (set-face-background 'highlight "grey")
       ;;
       ;; Links in HTML mode
       (copy-face 'default 'font-lock-reference-face)
       (set-face-foreground 'font-lock-reference-face "pink")

       (cond (is-xemacs
	      ;;
	      ;; Preprocessor Directives
	      (copy-face 'default 'font-lock-preprocessor-face)
	      (set-face-foreground 'font-lock-preprocessor-face "green")
	      ;;
	      ;; String
	      (set-face-foreground 'font-lock-doc-string-face "grey")
	      ;;
	      ;; Search Results
	      (set-face-background 'isearch "grey")
	      )
       )

       (add-hook 'emacs-lisp-mode-hook	'turn-on-font-lock)
       (add-hook 'lisp-mode-hook	'turn-on-font-lock)
       (add-hook 'c-mode-hook		'turn-on-font-lock)
       (add-hook 'c++-mode-hook		'turn-on-font-lock)
       (add-hook 'perl-mode-hook	'turn-on-font-lock)
       (add-hook 'tex-mode-hook		'turn-on-font-lock)
       (add-hook 'texinfo-mode-hook	'turn-on-font-lock)
       (add-hook 'postscript-mode-hook	'turn-on-font-lock)
       (add-hook 'dired-mode-hook	'turn-on-font-lock)
       (add-hook 'ada-mode-hook		'turn-on-font-lock)
       (add-hook 'java-mode-hook	'turn-on-font-lock)
       (add-hook 'xml-mode-hook         'turn-on-font-lock)
      )
)

