;;
;; my-dired.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-dired-mode-init()
  "My Dired Mode init"

  (make-face-unbold 'dired-face-directory)
  (set-face-foreground 'dired-face-directory "blue")
)

(add-hook 'dired-mode-hook 'my-dired-mode-init)
