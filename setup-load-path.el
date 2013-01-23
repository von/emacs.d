;;
;; Set up Emac's load path
;;
;; Having this in a separate file allows inclusion by compile.sh
;;
;; See: http://emacswiki.org/emacs/LoadPath

;; Use directory-file-name here to strip any trailing directory
;; separator
(defvar home (directory-file-name (getenv "HOME"))
  "My home directory.")

;; Prepend load stuff...
(unless (boundp 'directory-sep-char) (setq directory-sep-char ?/))
(setq directory-sep-string (char-to-string directory-sep-char))

(setq my-lib-dir (concat home directory-sep-string "lib"))

(setq my-emacs-config-dir (concat home
				  directory-sep-string
				  "emacs-config"
				  directory-sep-string))
(if (file-accessible-directory-p my-emacs-config-dir)
     (setq load-path (cons my-emacs-config-dir load-path))
)

(setq my-lisp-dir (concat my-lib-dir directory-sep-string "lisp"))
(if (file-accessible-directory-p my-lisp-dir)
    (setq load-path (cons my-lisp-dir load-path))
)

;; Add all subdirectories of my-lisp-dir to my load path
(let ((default-directory my-lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))

;; Add lib/emacs-<majorversion>
(setq my-emacs-dir (concat my-lib-dir directory-sep-string
			   "emacs-" (int-to-string emacs-major-version)))
(if (file-accessible-directory-p my-emacs-dir)
    (progn
      (setq load-path (cons my-emacs-dir load-path))
      (let ((default-directory my-emacs-dir))
	(normal-top-level-add-subdirs-to-load-path))
      ))

;; Add /usr/local/share/emacs/site-lisp/ if it exists
(if (file-accessible-directory-p "/usr/local/share/emacs/site-lisp/")
    (setq load-path (cons "/usr/local/share/emacs/site-lisp/" load-path))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up Emac's load path
;;
;; See: http://emacswiki.org/emacs/LoadPath

;; Use directory-file-name here to strip any trailing directory
;; separator
(defvar home (directory-file-name (getenv "HOME"))
  "My home directory.")

(setq my-lib-dir (concat home directory-sep-string "lib"))

(setq my-emacs-config-dir (concat home
				  directory-sep-string
				  "emacs-config"
				  directory-sep-string))
(if (file-accessible-directory-p my-emacs-config-dir)
     (setq load-path (cons my-emacs-config-dir load-path))
)

(setq my-lisp-dir (concat my-lib-dir directory-sep-string "lisp"))
(if (file-accessible-directory-p my-lisp-dir)
    (setq load-path (cons my-lisp-dir load-path))
)

;; Add all subdirectories of my-lisp-dir to my load path
(let ((default-directory my-lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))

;; Add lib/emacs-<majorversion>
(setq my-emacs-dir (concat my-lib-dir directory-sep-string
			   "emacs-" (int-to-string emacs-major-version)))
(if (file-accessible-directory-p my-emacs-dir)
    (progn
      (setq load-path (cons my-emacs-dir load-path))
      (let ((default-directory my-emacs-dir))
	(normal-top-level-add-subdirs-to-load-path))
      ))

;; Add /usr/local/share/emacs/site-lisp/ if it exists
(if (file-accessible-directory-p "/usr/local/share/emacs/site-lisp/")
    (setq load-path (cons "/usr/local/share/emacs/site-lisp/" load-path))
)
