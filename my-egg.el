;;;
;;; Load and configure Egg (GIT interface)
;;;


(if (eq emacs-major-version 22)
    (require 'dominating-file))

(require 'egg)

;; Hack: Invoke egg- methods instead of vc- methods
;;       Not sure why this is needed
(global-set-key "\C-xvs" 'egg-status)
(global-set-key "\C-xvl" 'egg-log)
