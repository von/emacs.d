;;;
;;; Mako
;;; https://bitbucket.org/pjenvey/mmm-mako

;; XXX - Mako mode doesn't quite work, Emacs doesn't recognize
;;       opening Mako blocks and gets confused complaining about
;;       mismatched tags.

(require 'mmm-mako)

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)
