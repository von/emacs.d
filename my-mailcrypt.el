;;
;; my-mailcrypt.el
;;
;; $Id$


;; Preload mailcrypt so we can set the version
(load-library "mailcrypt")

(mc-setversion "gpg")

;; Encrypt all outgoing messages with my key
(setq mc-encrypt-for-me t)
