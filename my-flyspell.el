;;;
;;; flyspell
;;;
;;; Requires aspell. On a mac, install with:
;;;   sudo port install aspell
;;;   sudo port install aspell-dict-en

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
