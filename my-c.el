;;
;; my-c.el
;;
;; Everything having to do with C coding.
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.hh$" . c++-mode)
		("\\.cpp$" . c++-mode)
		("\\.CPP$" . c++-mode)
		("\\.c$"  . c-mode)
		("\\.h$"  . c-mode)
		("\\.hin$"  . c-mode)
		("\\.c.in$" . c-mode)
		("\\.h.in$" . c-mode)
		("\\.y$" . c-mode)
		)
	      auto-mode-alist))

;; Make tab behave normmally
(setq c-tab-always-indent nil)

(defun my-c-mode-init()
  "My C Mode init"

  (make-local-variable 'c-basic-offset)
  (make-local-variable 'tab-width)

  (setq

   ;;
   ;; Indentation Control
   c-basic-offset 4

   ;; Make tab behave normmally
   c-tab-always-indent nil

   ;; Make tabs always 8
   tab-width 8

   ;; Make style stuff local
   c-style-variables-are-local-p t

   ;; Don't use tabs for indentation
   indent-tabs-mode nil

   ;; End Indentation Stuff

   )

  
  (c-toggle-auto-hungry-state 1)

  
  (if modify-menu
      (progn
	(add-menu-button '("C")
			 ["Edit Comment" c-comment-edit (within-c-comment-p)])

	(add-submenu '("C") '("Tags"
			      ["Find Tag" find-tag t]
			      ["Find Tag other window" find-tag-other-window t]
			      )
		     )
	(add-submenu '("C") '("Identation"
			      ["2" (setq c-basic-offset 2)
			       :style radio
			       :selected (eq c-basic-offset 2)]
			      ["4" (setq c-basic-offset 4)
			       :style radio
			       :selected (eq c-basic-offset 4)]
			      ["8" (setq c-basic-offset 8)
			       :style radio
			       :selected (eq c-basic-offset 8)]
			      )
		     )
	(add-menu-button '("C")
			 ["Add Changelog entry"
			  add-change-log-entry-other-window t]
			 )
	)
    )
)

(add-hook 'c-mode-hook 'my-c-mode-init)

;;;
;;; C-Comment Editing stuff
;;;

(defun my-c-comment-mode-init()
  "My C Comment Mode init"

  (cond (modify-menu
	 (make-menubar-local)
	 
	 (delete-menu-item '("File"))
	 
	 (setq mode-popup-menu '("C-Comment"
				 ["End" c-comment-edit-end t]
				 ["Abort" c-comment-edit-abort t]
				 )
	       )
	 
	 (add-submenu nil mode-popup-menu)

	 (setq tab-width 4)
	 ))

  ;; Turn on auto-fill
  (auto-fill-mode 1)

  (define-key mode-specific-map "\C-c" 'c-comment-edit-end)

  ;; Split the window and display the original C code in the other window.
  ;; XXX - if the edit is aborted this leaves a split window.
  (split-window)
  (pop-to-buffer (buffer-file-name))
  (other-window 1)
)

(setq c-comment-edit-hook 'my-c-comment-mode-init)

;; At some point between 20 and 21 "c-comment" changes to "c-comment-edit"
(cond ((>= emacs-major-version 21)
       (autoload 'within-c-comment-p "c-comment-edit")
       (autoload 'c-comment-edit "c-comment-edit")
       )
      (t
       (autoload 'within-c-comment-p "c-comment")
       (autoload 'c-comment-edit "c-comment")
       )
      )

;; For WinRsh
(c-add-style "winrsh" 
	     '((c-basic-offset . 2)
	       (c-offsets-alist
		(defun-open . -)
		(substatement-open . 0)
		(case-label . +)
		)
	       )
	     )

;; All this taken from Brian Toonen
(c-add-style "globus"
	     '((c-basic-offset . 4)
	       (c-comment-only-line-offset . (0 . 0))
	       (c-indent-comments-syntactically-p t)
	       (c-hanging-comment-starter-p nil)
	       (c-hanging-comment-ender-p nil)
	       (c-comment-continuation-stars "* ")
	       (comment-column . 40)
	       (fill-column . 79)
	       (c-cleanup-list '(defun-close-semi list-close-comma))
	       (c-hanging-colon-alist '(
					(case-label . (before after))
					(label . (before after))
					))
	       (c-offsets-alist
		(label . 2)
		(access-label . 2)
		(case-label . 2)
		(statement-case-open . 2)
		(statement-case-intro . 2)
		(topmost-intro-cont . 0)
		(statement-cont . +)
		(substatement-open . 0)
		(substatement . -10000)
		(inher-intro . 0)
		(member-init-intro . 0)
		(arglist-intro . +)
		(arglist-close . 0)
		(inline-open . 0)
		)
	       (c-hanging-braces-alist
		(defun-open . (before after))
		(defun-close . (before after))
		(class-open . (before after))
		(class-close . (before after))
		(inline-open . (before after))
		(inline-close . (before after))
		(block-open . (before after))
		(block-close . (before after))
		(substatement-open . (before after))
		(statement-case-open . (before after))
		(brace-list-open . (before after))
		(brace-list-close . (before after))
		(brace-list-intro . (before after))
		(brace-list-entry . (before after))
		)))

(setq c-default-style "globus")
