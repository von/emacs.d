;;
;; my-nroff.el
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nroff-to-man()
  "Process a nroff buffer to a man page"

  (interactive)

  ;; Get/create my buffer
  (setq my-buffer-name "*Nroff-to-Man*")
  (setq my-buffer (get-buffer-create my-buffer-name))
  ;; Make sure it is writable
  (save-excursion
    (progn (pop-to-buffer my-buffer-name)
	   (setq buffer-read-only nil)
	   )
    )
  ;; Copy nroff text over to buffer and change to it
  (copy-to-buffer my-buffer (point-min) (point-max))
  (pop-to-buffer my-buffer-name)
  ;; Ok, pass the buffer through nroff
  (call-process-region (point-min) (point-max)
		       "nroff" nil t nil "-man")
  ;; Delete the nroff test
  (delete-region (point) (point-max))
  ;; Ok, clean up output and run Manual stuff
  (autoload 'Manual-nuke-nroff-bs "man")
  (Manual-nuke-nroff-bs)
  (Manual-mode)
  (goto-line 0)
  (view-buffer my-buffer-name)
)

(defun my-nroff-mode-init()
  "My Nroff Mode init"

  (local-set-key "\em" 'nroff-to-man)
)

(add-hook 'nroff-mode-hook 'my-nroff-mode-init)
