;;
;; my-sh.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my-sh-mode-init()
  "My Shell Script Mode init"

  (make-menubar-local)

  (setq

   sh-indentation 2
   )

  (if modify-menu
      (progn
	(add-submenu '("Shell-Script") '("Identation"
					 ["2" (setq sh-indentation 2)
					  :style radio
					  :selected (eq sh-indentation 2)]
					 ["4" (setq sh-indentation 4)
					  :style radio
					  :selected (eq sh-indentation 4)]
					 ["8" (setq sh-indentation 8)
					  :style radio
					  :selected (eq sh-indentation 8)]
					 )
		     )
	)
    )
)

(add-hook 'sh-mode-hook 'my-sh-mode-init)

