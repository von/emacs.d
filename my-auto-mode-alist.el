;;
;; Additions to auto-mode-alist


(setq auto-mode-alist
      (append '(("\\.shtml$" . html-mode)
                ("buildall.conf" . sh-mode)
                ("\\.mhtml" . xml-mode)
                ("\\.wsdl" . xml-mode)
                ("\\.js\\'" . javascript-mode)
                )
              auto-mode-alist))
