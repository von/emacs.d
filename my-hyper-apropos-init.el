;;
;; my-hyper-apropos-init.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(copy-face 'default 'hyper-apropos-heading)
(set-face-foreground 'hyper-apropos-heading "green")
(set-face-background 'hyper-apropos-heading "black")

(copy-face 'default 'hyper-apropos-major-heading)
(set-face-foreground 'hyper-apropos-major-heading "green")
(set-face-background 'hyper-apropos-major-heading "black")

(copy-face 'default 'hyper-apropos-section-heading)
(set-face-foreground 'hyper-apropos-section-heading "green")
(set-face-background 'hyper-apropos-section-heading "black")

(defun my-hyper-apropos-mode-init()
  "My Hyper Apropos mode init"

  (make-face-unbold 'hyper-apropos-heading)
  (make-face-unbold 'hyper-apropos-major-heading)
  (make-face-unbold 'hyper-apropos-section-heading)
)

(add-hook 'hyper-apropos-mode-hook 'my-hyper-apropos-mode-init)
