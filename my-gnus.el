;;
;; my-gnus.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
      ;;
      ;; Go to next non-empty group automatically
      gnu-auto-select-next "true"
      ;;
      ;; Don't confirm catchup XXX- doesn't seem to work in article mode
      gnus-interactive-catchup nil
      ;;
      ;; Don't confirm exit
      gnus-interactive-exit nil
      ;;
      ;; Domain name
      gnus-local-domain "ncsa.uiuc.edu"
      ;;
      ;; Organization
      gnus-local-organization "National Center for Supercomputing Applications"
      ;;
      ;; .newsrc file
      gnus-startup-file "~/.private/.newsrc"
      ;;
      ;; Put toolbar at top
      gnus-toolbar-orientation 'top
)
