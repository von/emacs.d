;;;
;;; Load and configure the desktop package.
;;;
;;; Saves all the open files when exiting for restoration on next start.

;; Turn on desktop saving
(desktop-save-mode t)

;; Always save, don't ask
(setq desktop-save t)

;; Where to save desktop file
(setq desktop-dirname home)
