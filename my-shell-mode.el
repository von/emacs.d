;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Make arrow keys work as I would expect in shell-mode
;; Kudos: Eli Daniel
;;   http://lists.gnu.org/archive/html/help-emacs-windows/2002-10/msg00059.html

;; move cursor to the previous line or get previous history item, depending
;; on whether we're at a shell mode prompt

(defun ewd-comint-up (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input arg)
    (previous-line arg)))

;; move cursor to the next line or get next history item, depending
;; on whether we're at a shell mode prompt
(defun ewd-comint-down (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input arg)
    (next-line arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My shell-mode-hook

(add-hook 'shell-mode-hook
          (lambda ()
	    (define-key shell-mode-map "C-u" 'comint-delete-input)

	    ;; bind my special functions to the up and down keys in shell-mode
            (define-key shell-mode-map [up] 'ewd-comint-up)
            (define-key shell-mode-map [down] 'ewd-comint-down)

	    ;; Kudos for the following to
	    ;; http://snarfed.org/why_i_dont_run_shells_inside_emacs
	    ;; always insert at the bottom
	    (setq comint-scroll-to-bottom-on-input t)
	    ;; always add output at the bottom
	    (setq comint-scroll-to-bottom-on-output t)
	    ;; scroll to show max possible output
	    (setq comint-scroll-show-maximum-output t)
	    ))
