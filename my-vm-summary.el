;;
;; my-vm-summary.el
;;
;; $Id$

;; Turn threading off by default
(setq-default vm-summary-show-threads nil)

;; Set summary format (should have CR in string)
(setq-default vm-summary-format "%4n %2M/%02d %*%a %-17.17F %I\"%s\" (%c)
")

;; Don't use subject for threading
(setq-default vm-thread-using-subject nil)

;; Auto-center summary?
(setq-default vm-auto-center-summary nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; vm-summary-hilit
;;

(require `vm-summary-hilit)

;;
;; First match in list below takes precedence
;; Note that the header regex requires parens - e.g. (To:|Cc:)
;;
(setq vm-summary-hilit-alist
      '(
	("From:"
	 ("welch@mcs.anl.gov" . "grey")
	 )
	("Sender:"
	 ;;
	 ;; Essentially me
	 ("owner-security-internal@globus.org" . "white")
	 ;;
	 ;; ESG
	 ("esg-admin@earthsystemgrid.org" . "DarkRed")
	 ;;
	 ;; Misc ANL/UC
	 ("\\(cs-admin\\|staff-admin\\)@cs.uchicago.edu\\|\\(owner-mcs\\|owner-camp-seminars\\|owner-seminars\\|owner-disc\\|owner-unix-users\\)@mcs.anl.gov\\|owner-argonne-today@achilles.ctd.anl.gov" . "DarkRed")
	 ;;
	 ;; PPDG
	 ("\\(ppdg-admin\\|ppdg-jdl-admin\\)@ppdg.net" . "DarkRed")
	 ("ppdg-siteaa-admin@ppdg.net\\|ppdg-cara-admin@ppdg.net" . "red")
	 ;;
	 ;; NMI
	 ("owner-team@grids-center.org" . "DarkRed")
	 ;;
	 ;; IETF
	 ("owner-ietf-krb-wg@achilles.ctd.anl.gov\\|owner-aaaarch@fokus.gmd.de\\|owner-ietf-pkix@mail.imc.org\\|owner-ietf-sacred@mail.imc.org\\|cfrg-admin@ietf.org" . "DarkRed")
	 ;;
	 ;; Internet2
	 ("owner-mace@internet2.edu\\|owner-hepki-tag@internet2.edu\\|owner-mace-opensaml-users@internet2.edu\\|owner-mw-announce@internet2.edu" . "DarkRed")
	 ;;
	 ;; Misc
	 ("owner-wu-ftpd@wugate.wustl.edu" . "DarkRed")
	 ("owner-access-online@ncsa.uiuc.edu" . "DarkRed")
	 ("lap-interest-owner@projectliberty.org" . "DarkRed")
	 ;;
	 ;; High-interest misc lists
	 ("owner-announce@cs.uiuc.edu\\|owner-cryptography@wasabisystems.com" . "red")
	 ;;
	 ;; DSL
	 ("owner-dsl@mcs.anl.gov\\|owner-dsl-core@mcs.anl.gov\\|owner-dsl-uc@mcs.anl.gov\\|dsl-admin@cs.uchicago.edu" . "pink")
	 ;;
	 ;; DOESG
	 ("owner-doe-\\(sg\\|pma\\|ca\\)@george.lbl.gov" . "#FF00FF")
	 ;;
	 ;; Management lists
	 ("owner-management@globus.org\\|owner-dsl-management@mcs.anl.gov\\|owner-ogsa-management@globus.org\\|owner-globus-ogsa-management@globus.org" . "orange")
	 ;;
	 ;; High-interest Globus lists
	 ("\\(owner-python-discuss\\|owner-developers\\|owner-developer-discuss\\|owner-ogsa-alpha\\|owner-ogsa-developers\\|owner-gt3-developers-internal\\|owner-gsi-openssh\\|owner-ogsa-security\\|owner-ogsa-discuss\\)@globus.org" . "blue")
	 ;;
	 ;; Other Globus lists
	 ("globus.org" . "DarkBlue")
	 ;;
	 ;; GGF High-interest lists
	 ("\\(owner-security-wg\\|owner-ogsi-wg\\|owner-ogsa-sec-wg\\)@gridforum.org" . "green")
	 ;;
	 ;; Other GGF lists
	 ("gridforum.org\\|ggf-testscripts-admin@cs.uchicago.edu" . "DarkGreen")
	 )
	;; This regex still doesn't work...
	("\\(To:\\|Cc:\\|cc:\\)"
	 ;;
	 ;; Stuff without Sender field
	 ("lists.oasis-open.org" . "DarkRed")
	 ;;
	 ;; Me
	 ("welch@mcs.anl.gov" . "white")
	 )
	)
      )

;; Calling this function modifies the above list
(vm-make-summary-hilit-alist-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-summary)
