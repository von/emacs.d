;;
;; Whitespace mode configuration

;; This by itself will show trailing whitespace on lines
(require 'whitespace)

;; Clean up trailing whitespace and tabs
(global-set-key "\C-cw" 'whitespace-cleanup)

;; Turn on whitespace display
(global-set-key "\C-cW" 'whitespace-mode)

(setq-default

 ;; Show trailing whitesapce
 show-trailing-whitespace t

 ;; Don't use tabs by default
 indent-tabs-mode nil)
