;;;
;;; Look and feel of the Emacs GUI
;;;

;; Put directory component in buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Display column number on modeline
(cond
 ((fboundp 'display-column-mode)
  (display-column-mode t))

 ((fboundp 'column-number-mode)
  (column-number-mode t))
 )

;; Display line number on modeline
(cond
 ((fboundp 'line-number-mode)
  (line-number-mode t))
)

;; Get rid of toolbar
(tool-bar-mode -1)

;; Use visible bell instead of audible bell
(setq visible-bell t)

;; Set frame size
(if window-system
    (progn
      (set-frame-size (selected-frame) 80 50)
      (add-to-list 'default-frame-alist '(height . 50))
      (add-to-list 'default-frame-alist '(width . 80))
      ))

;; Copy on select
(setq x-select-enable-clipboard t)

;; Turn on paren highlighting
(cond
 ((fboundp 'paren-set-mode)
  (paren-set-mode 'paren))
 ((fboundp 'show-paren-mode)
  (show-paren-mode t))
)
