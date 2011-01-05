;;
;; From: http://www.chickenwingsoftware.com/scratches/python/how-i-edit-django-templates
;;
;; May need latest verison of mmm-mode:
;; http://mmm-mode.sourceforge.net/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS-Mode
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use hm--html-mode for files that end in .tmpl (Django templates)
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . hm--html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Major Modes.
(require 'mmm-vars)
(require 'mmm-mode)
(require 'mmm-sample)
(setq mmm-global-mode 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom MMM classes for Django templates
(mmm-add-classes
 '((my-django-expr
    :submode python-mode
    :face mmm-declaration-submode-face
    :front "{%"
    :back "%}"
    :include-front t
    :include-back t)))

(mmm-add-classes
 '((my-django-var
    :submode python
    :face mmm-output-submode-face
    :front "{{"
    :back "}}"
    :include-front t
    :include-back t)))

(mmm-add-mode-ext-class nil "\\.tmpl\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.tmpl\\'" 'my-django-var)
(mmm-add-mode-ext-class nil "\\.tmpl\\'" 'my-django-expr)
(mmm-add-mode-ext-class nil "\\.tmpl\\'" 'html-js)

;; Use different colors for different sub-modes.
(setq mmm-submode-decoration-level 2)
;; Make the code submode a little more readable.
(set-face-background 'mmm-code-submode-face "#EEEEFF")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My face tweaks
;;

;; {% %} blocks
(set-face-background 'mmm-declaration-submode-face "Black")
(set-face-foreground 'mmm-declaration-submode-face "Plum")

;; {{ }} blocks
(set-face-background 'mmm-output-submode-face "Black")
(set-face-foreground 'mmm-output-submode-face "Green")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auto-closing tags
;; http://www.chickenwingsoftware.com/scratches/python/auto-closing-django-tags

(defvar django-closable-tags
  '("for" "block" "comment" "filter" "ifchanged" "ifequal"
    "ifnotequal" "spaceless" "if"))

(defvar django-tag-re
  (concat "{%\\s *\\(end\\)?\\("
          (mapconcat 'identity django-closable-tags "\\|")
          "\\)[^%]*%}"))

(defun django-find-open-tag ()
  (if (search-backward-regexp django-tag-re nil t)
      (if (match-string 1) ; If it's an end tag
          (if (not (string= (match-string 2) (django-find-open-tag)))
              (error "Unmatched Django tag")
            (django-find-open-tag))
        (match-string 2)) ; Otherwise, return the match
    nil))

(defun django-close-tag ()
  (interactive)
  (let ((open-tag (save-excursion (django-find-open-tag))))
    (if open-tag
        (insert "{% end" open-tag " %}")
      (error "Nothing to close"))))

    
;;(define-key html-mode-map "\C-c]" 'django-close-tag)


