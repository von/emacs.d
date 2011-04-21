;;
;; my-mailcrypt.el
;;
;; $Id$


(if
    my-use-vm
    (progn
      ;; Preload mailcrypt so we can set the version
      (load-library "mailcrypt")

      (mc-setversion "gpg")

      (setq mc-gpg-path "/usr/local/bin/gpg")

      ;; Encrypt all outgoing messages with my key
      (setq mc-encrypt-for-me t)
      )
  )
