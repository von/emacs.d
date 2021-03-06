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

(defun my-shell-mode-hook()
  (define-key shell-mode-map "C-u" 'comint-delete-input)

  ;; Unbind this from prefix command so I can type a capital C...
  (define-key shell-mode-map "C" nil)
  
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
  ;; Ignore duplicates in history
  (setq comint-input-ignoredups t)
)

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create shell if it doesn't exist or switch to it if it does

(defun create-or-switch-to-shell()
  (interactive)
  (if (get-buffer "*shell*")
      (switch-to-buffer "*shell*")
    (shell)
    )
  )

(global-set-key "\M-ss" 'create-or-switch-to-shell)
