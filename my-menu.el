;;
;; my-menu.el
;;
;; General menu enhancements
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun toggle(variable)
  "Toggle a variable passed in by symbol."
  
  (if (symbol-value variable) (set variable nil)
    (set variable t)))

(cond
 ((and modify-menu is-xemacs)
  ;; Remove Default Options menu
  (delete-menu-item '("Options"))
       
  ;; Remove Default Apps menu
  (delete-menu-item '("Apps"))
       
  ;; Remove Default Edit menu
  (delete-menu-item '("Edit"))
       
  ;; Add my own Options menu
  (add-submenu nil '("Options"
		     ["Read Only" (toggle-read-only)
		      :style toggle :selected buffer-read-only]
		     ["Overwrite" overwrite-mode
		      :style toggle :selected overwrite-mode]
		     ["Word Wrap" auto-fill-mode
		      :style toggle :selected auto-fill-function]
		     ["Adaptive Fill" (toggle 'adaptive-fill-mode)
		      :style toggle :selected adaptive-fill-mode]
;; XXX mouse-yank-at-point undefined here
;;		     ["Paste at cursor" (toggle 'mouse-yank-at-point)
;;		      :style toggle :selected move-yank-at-point]
		     ("Paren Highlighting"
		      ["Off" (paren-set-mode -1)
		       :style radio :selected (not paren-mode)]
		      ["On" (paren-set-mode 'paren)
		       :style radio :selected (eq paren-mode 'paren)]
		      ["Blinking" (paren-set-mode 'blink-paren)
		       :style radio :selected (eq paren-mode 'blink-paren)]
		      ["Expression" (paren-set-mode 'sexp)
		       :style radio :selected (eq paren-mode 'sexp)]
		      )
		     ("Tabs"
		      ["Use Spaces" (setq indent-tabs-mode nil)
		       :style radio
		       :selected (eq indent-tabs-mode nil)]
		      ["Use Tabs" (setq indent-tabs-mode t)
		       :style radio
		       :selected (eq indent-tabs-mode t)]
		      ["Width 2" (setq tab-width 2)
		       :style radio
		       :selected (eq tab-width 2)]
		      ["Width 4" (setq tab-width 4)
		       :style radio
		       :selected (eq tab-width 4)]
		      ["Width 8" (setq tab-width 8)
		       :style radio
		       :selected (eq tab-width 8)]
		      )
		     ("Mode"
		      ["C" (c-mode)
		       :style radio
		       :selected (string-equal major-mode "c-mode")]
		      ["Fundamental" (fundamental-mode)
		       :style radio
		       :selected (string-equal major-mode "fundamental-mode")]
		      ["Perl" (perl-mode)
		       :style radio
		       :selected (string-equal major-mode "perl-mode")]
		      ["Lisp" (lisp-mode)
		       :style radio
		       :selected (string-equal major-mode "lisp-mode")]
		      ["Emacs-Lisp" (emacs-lisp-mode)
		       :style radio
		       :selected (string-equal major-mode "emacs-lisp-mode")]
		      ["Ksh" (ksh-mode)
		       :style radio
		       :selected (string-equal major-mode "ksh-mode")]
		      ["HTML" (html-mode)
		       :style radio
		       :selected (string-equal major-mode "html-mode")]
		      )
		     )
	       "Buffers"
	       )
       
  ;; Add stuff to Tools menu
  (add-menu-button '("Tools") ["Man" manual-entry t])
  (add-menu-button '("Tools") ["Calendar" calendar t])
  (add-menu-button '("Tools") ["BBDB" bbdb t])
  (add-menu-button '("Tools") ["----" nil nil] "Calendar")
  ;; Formating
  (add-submenu '("Tools") '("Format"
			    ["Paragraph" fill-paragraph t]
			    ["Region" fill-region (region-active-p)]
			    ["Region as Pragraph"
			     fill-region-as-paragraph (region-active-p)]
			    ["Prefix region" prefix-region (region-active-p)]
			    "---"
			    ["Set prefix" set-fill-prefix t]
			    ["Clear prefix"
			     (save-excursion (beginning-of-line)
					     (set-fill-prefix)) t]
			    ["Set column" set-fill-column t]
			    "---"
			    ["Center Line" center-line t]
			    )
	       "Man"
	       )
  ;; Spell Check
  (add-submenu '("Tools") '("Spell"
			    ["Buffer" ispell-buffer t]
			    ["Region" ispell-region (region-active-p)]
			    ["Word" ispell-word t]
			    "---"
			    ["Continue" ispell-continue t]
			    "---"
			    ["Complete Word" ispell-complete-word t]
			    ["Complete Word Frag" ispell-complete-word-frag t]
			    )
	       "Man"
	       )
  ;; Tabify
  (add-submenu '("Tools") '("Tabify"
			    ["Tabify Region" tabify (region-active-p)]
			    ["Untabify Regfion" untabify (region-active-p)]
			    )
	       "Man"
	       )
  ;; Macro Recording
  (add-submenu '("Tools") '("Macro Recording"
			    ["Start"
			     start-kbd-macro (not defining-kbd-macro)]
			    ["End" end-kbd-macro defining-kbd-macro]
			    ["Execute" call-last-kbd-macro last-kbd-macro]
			    )
	       "Man")
  ;; Searching
  (add-submenu '("Tools") '("Search"
			    ["Forward" isearch-forward t]
			    ["Reverse" isearch-backward t]
			    ["Replace" query-replace t]
			    "---"
			    ["Regexp Forward" isearch-forward-regexp t]
			    ["Regexp Reverse" isearch-backward-regexp t]
			    ["Regexp Replace" query-replace-regexp t]
			    )
	       "Man")
       
  ;; Add stuff to File menu
  (add-menu-button '("File")
		   ["Recover File" recover-file t] "Insert File...")
  (add-menu-button '("File") ["Dired" dired t] "Recover File")
  (add-menu-button '("File") ["Insert Buffer..." insert-buffer t]
		   "Insert File...")

  (add-menu-button '("Tools") ["Shell" shell t] "Shell Command...")
  )
 )
