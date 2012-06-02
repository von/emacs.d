;;;
;;; Load and configure Markdown mode
;;;

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(set-variable 'markdown-command "Markdown.pl")
