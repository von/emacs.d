;;
;; my-change-log.el
;;
;; $Id$

(defun my-change-log-mode-init
  "My change-log mode init"

  ;; Turn on auto-fill
  (auto-fill-mode 1)
  )

(add-hook 'change-log-mode-hook 'my-change-log-mode-init)
