;;
;; my-man.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-man-mode-init()
  "My Man Mode init"

  (copy-face 'default 'man-heading)
  (set-face-foreground 'man-heading "yellow")
  (set-face-foreground 'man-xref "red")
  (set-face-foreground 'man-italic "green")
  (set-face-foreground 'man-bold "blue")
  (make-face-unbold 'man-bold)

  (local-set-key "\M-p" 'cperl-perldoc)
)

(add-hook 'Manual-mode-hook 'my-man-mode-init)
