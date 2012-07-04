;;;
;;; Load and configure Egg (GIT interface)
;;;


(if (eq emacs-major-version 22)
    (require 'dominating-file))

;; Change egg prefix from "C-x v"
;; This must be set before egg is loaded to have affect.
;;(custom-set-variables '(egg-mode-key-prefix "C-c v"))

(require 'egg)
