;;;
;;; my-key-bindings.el
;;;
;;; $Id$
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change the control keys for exiting (C-x C-c is too easy to hit)
;;

(global-set-key "\C-x\C-c" 'delete-frame)
(global-set-key "\C-xC" 'save-buffers-kill-emacs)

(defun kill-buffer-and-window()
  "Kill current buffer and remove it's window."
  (interactive)

  ;; Note the current window so that we can tell if it get's deleted when
  ;; we delete the buffer.
  (setq current-window (selected-window))

  ;; If we sucessfully kill the buffer then we delete the window if (1) it's
  ;; not the only buffer in the frame and (2) the selected window is still
  ;; the same as it was before we deleted the buffer (if it's changed that
  ;; probably means the window and/or frame was deleted with the buffer -
  ;; vm does this).

  (if (kill-buffer nil)
      (if (and (not (one-window-p))
	       (equal current-window (selected-window)))
	  (delete-window)))
  )

(define-key global-map [(control x)(control k)] 'kill-buffer-and-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allow me to enter a C-x c for extended command since I can't
;; use meta on the PC

(global-set-key "\C-xc" 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;				   

(global-set-key "\eg" 'goto-line)
(global-set-key "\em" 'compile)
(global-set-key "\C-xp" 'set-fill-prefix)
(global-set-key "\e&" 'query-replace-regexp)

(global-set-key "\e#" 'fill-paragraph)


;;;
;;; Define some stuff to scroll the other window
;;;
(defun scroll-other-window-one-line()
  "Scroll other window up one line."
  (interactive)
  (scroll-other-window 1)
)

(defun scroll-other-window-down-one-line()
  "Scroll other window down one line."
  (interactive)
  (scroll-other-window -1)
)

(cond (is-xemacs

       (global-set-key '(control /) 'undo)
       (global-set-key '(control u) 'undo)
       (global-set-key '(meta right) 'scroll-other-window)
       (global-set-key '(meta down) 'scroll-other-window-one-line)
       (global-set-key '(meta left) 'scroll-other-window-down)
       (global-set-key '(meta up) 'scroll-other-window-down-one-line)

       ;; Control-x up pages up
       (global-set-key "\C-x\OA" 'scroll-down)
       ;; Control-x down pages down
       (global-set-key "\C-x\OB" 'scroll-up)

       ;; Make backspace and delete behavior correctly
       (global-set-key '(delete) 'delete-char)
       (global-set-key '(backspace) 'delete-backward-char)

       ;; And make backspace work right from my PC through telnet
       (global-set-key "\C-h" 'delete-backward-char)

       ;; Make control-delete delete all whitespace
       (global-set-key '(control delete) 'delete-horizontal-space)

       ;; Make shift-tab always tab (XXX Doesn't work)
       (define-key global-map '(shift tab) 'self-insert-command)

       (define-key global-map '(shift button2) 'popup-buffer-menu)

       (global-set-key '(alt m) 'manual-entry)
       (global-set-key "\C-xm" 'manual-entry)

       ))

