;;
;; my-bbdb.el
;;

(require 'bbdb)
(bbdb-initialize)

(setq bbdb-default-area-code 217)

;; Show brief listings by default
(setq bbdb-elided-display t)

;; Build all mail aliases now
(bbdb-define-all-aliases)

(cond ((boundp 'merge-mail-aliases)
       ;; Now load mail aliases from ~/.mail_aliases
       (merge-mail-aliases (concat (getenv "HOME") "/.mail_aliases"))
       ))

(defun my-bbdb-after-change-hook (record)
  "My hook to do stuff after I change a bbdb record."

  ;; Rebuild all mail aliases
  (bbdb-define-all-aliases)
)

(add-hook 'bbdb-after-change-hook 'my-bbdb-after-change-hook)

(add-menu-button '("Tools")
		 ["BBDB Snarf Region" bbdb-snarf-region (region-active-p)])
