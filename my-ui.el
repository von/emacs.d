;;;
;;; Look and feel of the Emacs UI
;;;

(defun my-graphic-frame-init(new-frame)
  "Initialize a graphic frame"
  ;; Get rid of toolbar
  (tool-bar-mode -1)
)

(defun my-terminal-frame-init(new-frame)
  "Initialize a non-graphic frame"
  ;; Get rid of toolbar
  (tool-bar-mode -1)
  ;; Make 'delete' key delete backwars
  ;; Kudos: http://emacswiki.org/emacs/AquamacsEmacsCompatibilitySettings
  (normal-erase-is-backspace-mode nil)
  ;; Make modeline inverted (might be a better way?)
  (set-face-foreground 'modeline "white" new-frame)
  (set-face-background 'modeline "black" new-frame)

)

(defun my-frame-init(new-frame)
  "Initialize a frame"
  ;; Kudos: https://lists.gnu.org/archive/html/help-gnu-emacs/2011-11/msg00253.html
  (select-frame new-frame)
  ;; Graphical vs terminal frame
  ;; Kudos: http://stackoverflow.com/a/5801740/197789
  (if (display-graphic-p) (my-graphic-frame-init new-frame)
    (my-terminal-frame-init new-frame))
)
(add-hook 'after-make-frame-functions 'my-frame-init)

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

;; Turn off tabbar in Aquamacs
(cond
 ((fboundp 'tabbar-mode)
  (tabbar-mode -1))
)
