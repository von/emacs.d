;;
;; my-html.el
;;
;; Set up autoloading of my-html-autoload.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'html-mode-hook 'my-html-mode-init)
(autoload 'my-html-mode-init "my-html-mode-init")
