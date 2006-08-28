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
;; Make faces for vm-hiliting

;; Spam
(copy-face 'default 'summary-spam-face)
(set-face-foreground 'summary-spam-face "DarkRed")
(set-face-background 'summary-spam-face "DarkGrey")

;; To me
(copy-face 'default 'summary-me-face)

;; From me
(copy-face 'default 'summary-from-me-face)
(set-face-foreground 'summary-from-me-face "grey")

;; Non-critical mail lists
(copy-face 'default 'summary-non-critical-face)
(set-face-foreground 'summary-non-critical-face "DarkGreen")
(set-face-background 'summary-non-critical-face "DarkGrey")

;; Interesting mail lists
(copy-face 'default 'summary-interesting-face)
(set-face-foreground 'summary-interesting-face "red")

;; Medium interesting lists
(copy-face 'default 'summary-med-interest-face)
(set-face-foreground 'summary-med-interest-face "#FF00FF")


;; Low-interest mail lists
(copy-face 'default 'summary-low-interest-face)
(set-face-foreground 'summary-low-interest-face "DarkGreen")
(set-face-background 'summary-low-interest-face "DarkGrey")

;; DSL Email
(copy-face 'default 'summary-dsl-face)
(set-face-foreground 'summary-dsl-face "pink")

;; Management email
(copy-face 'default 'summary-management-face)
(set-face-foreground 'summary-management-face "orange")

;; High-interest Globus lists
(copy-face 'default 'summary-high-interest-globus-face)
(set-face-foreground 'summary-high-interest-globus-face "blue")

;; General Globus lists
(copy-face 'default 'summary-globus-face)
(set-face-foreground 'summary-globus-face "DarkGreen")
(set-face-background 'summary-globus-face "DarkGrey")

;; High-interest GGF lists
(copy-face 'default 'summary-high-interest-ggf-face)
(set-face-foreground 'summary-high-interest-ggf-face "green")

;; General GGF lists
(copy-face 'default 'summary-ggf-face)
(set-face-foreground 'summary-ggf-face "DarkGreen")
(set-face-background 'summary-ggf-face "DarkGrey")

;;
;; First match in list below takes precedence
;; Note that the header regex requires parens - e.g. (To:|Cc:)
;;
(setq vm-summary-hilit-alist
      '(
	;;
	;; Catch Spam
	("X-Spam-Flag:"
	 ("YES" . summary-spam-face)
	 )
	;; Catch stuff to me
	("\\(To\\|CC\\|Cc\\|cc\\):"
	 ("welch@mcs.anl.gov\\|vwelch@ncsa.uiuc.edu\\|von@vwelch.com" . summary-me-face)
	 )
	;; Stuff from me
	("From:"
	 ("welch@mcs.anl.gov" . summary-from-me-face)
	 ("vwelch@ncsa.uiuc.edu" . summary-from-me-face)
	 ("von@vwelch.com" . summary-from-me-face)
	 )
	("Subject:"
	 ;; Catch stuff about proxy certificates
	 ("proxy" . summary-me-face)
	 )
	("Sender:"
	 ;;
	 ;; Essentially me
	 ("owner-security-internal@globus.org" . summary-me-face)
	 ;;
	 ;; Management lists
	 ("owner-management@globus.org\\|owner-dsl-management@mcs.anl.gov\\|owner-ogsa-management@globus.org\\|owner-globus-ogsa-management@globus.org\\|owner-dsl-project-leaders@mcs.anl.gov" . summary-management-face)
	 ;;
	 ;; ESG
	 ("earthsystemgrid.org" . summary-non-critical-face)
	 ;;
	 ;; Misc ANL/UC
	 ("mcs.anl.gov\\|owner-argonne-today@achilles.ctd.anl.gov\\|lcrc.anl.gov" . summary-non-critical-face)
	 ;;
	 ;; PPDG
	 ("ppdg-siteaa-admin@ppdg.net\\|ppdg-cara-admin@ppdg.net" . summary-interesting-face)
	 ("ppdg.net" . summary-non-critical-face)
	 ;;
	 ;; NMI
	 ("owner-team@grids-center.org" . summary-med-interest-face)
	 ;;
	 ;; IETF
	 ("owner-ietf-pkix@mail.imc.org" . summary-non-critical-face)
	 ("kitten-bounces@lists.ietf.org" . summary-non-critical-face)
	 ("cfrg-bounces@ietf.org" . summary-non-critical-face)
	 ("easycert-bounces@machshav.com" . summary-non-critical-face)
	 ("owner-ietf-krb-wg@mailhost.anl.gov\\|owner-ietf-krb-wg@achilles.ctd.anl.gov\\|owner-aaaarch@fokus.gmd.de\\|owner-ietf-sacred@mail.imc.org\\|cfrg-admin@ietf.org\\|owner-ietf-cat-wg@lists@Standford.edu" . summary-low-interest-face)
	 ;;
	 ;; Internet2
	 ("internet2.edu" . summary-low-interest-face)
	 ;;
	 ;; Misc
	 ("owner-wu-ftpd@wugate.wustl.edu" . summary-low-interest-face)
	 ("owner-access-online@ncsa.uiuc.edu" . summary-low-interest-face)
	 ("projectliberty.org" . summary-low-interest-face)
	 ("discuss-gnuradio" . summary-low-interest-face)
	 ("owner-aaaarch@fokus.fraunhofer.de" . summary-low-interest-face)
	 ("owner-macos-users@ncsa.uiuc.edu" . summary-low-interest-face)
	 ("owner-tagpma-general@mail.canarie.ca" . summary-low-interest-face)
	 ;;
	 ;; Med Misc
	 ("owner-ip@v2.listbox.com" . summary-med-interest-face)
	 ("owner-comp-pol@ncsa.uiuc.edu" . summary-med-interest-face)
	 ;;
	 ;; High-interest misc lists
	 ("owner-announce@cs.uiuc.edu\\|owner-cryptography@wasabisystems.com\\|owner-cryptography@metzdowd.com" . summary-interesting-face)
	 ;;
	 ;; TeraGrid Lists
	 ("owner-ncsatg@ncsa.uiuc.edu" . summary-interesting-face)
	 ("@teragrid.org" . summary-interesting-face)
	 ;;
	 ;; Open Science Grid lists
	 ("OPENSCIENCEGRID.ORG" . summary-interesting-face)
	 ;;
	 ;; DSL
	 ("owner-dsl@mcs.anl.gov\\|owner-dsl-developers@mcs.anl.gov\\|owner-dsl-core@mcs.anl.gov\\|owner-dsl-uc@mcs.anl.gov\\|dsl-admin@cs.uchicago.edu\\|owner-dsl-uchicago-staff@mcs.anl.gov" . summary-dsl-face)
	 ;;
	 ;; DOESG
	 ;;("owner-doe-\\(sg\\|pma\\|ca\\)@george.lbl.gov" . summary-doesg-face)
	 ;;
	 ;; High-interest Globus lists
	 ("\\(owner-developers\\|owner-ogsa-developers\\|owner-gt3-developers-internal\\|owner-gsi-openssh\\|owner-ogsa-security\\|owner-ogsa-discuss\\|owner-announce\\|owner-security-announce\\|owner-cas-discuss\\|perf\\|owner-board\\)@globus.org" . summary-high-interest-globus-face)
	 ;;
	 ;; Other Globus lists
	 ("globus.org" . summary-globus-face)
	 ;;
	 ;; GGF High-interest lists
	 ("\\(owner-security-wg\\|owner-authz-wg\\|owner-ogsa-sec-wg\\|owner-ogsa-authz\\|owner-arrg-rg\\|owner-security-area\\|owner-gf-chairs\\|owner-wg-all\\)@gridforum.org" . summary-high-interest-ggf-face)
	 ;;
	 ;; Other GGF lists
	 ("ggf.org\\|gridforum.org\\|ggf-testscripts-admin@cs.uchicago.edu" . summary-ggf-face)
	 ("egrid@egrid.org" .summary-ggf-face)
	 ;;
	 ;; Emsl stuff
	 ("owner-hpcs2-users@emsl.pnl.gov" . summary-low-interest-face)
	 ;;
	 ;; security@ncsa
	 ("owner-security@ncsa.uiuc.edu" . summary-med-interest-face)
	 )
	;;
	;; A lot of spam seems to use my capitalized NCSA address
	("To:"
	 ("VWELCH@NCSA.UIUC.EDU" . summary-spam-face)
	 )
	;; Oasis stuff seems to use this header
	("Delivered-To:"
	 ("\\(was\\|security-services\\|xacml\\|saml-dev\\)@lists.oasis-open.org" . summary-low-interest-face)
	 ("lists.oasis-open.org" . summary-low-interest-face)
	 )
	;; And Internet2 uses List-Id:
	("List-Id:"
	 ("shibboleth-\\(dev\\|announce\\).internet2.edu" . summary-interesting-face)
	 ("mace.internet2.edu" . summary-interesting-face)
	 ("mw-e2ed-core.internet2.edu" . summary-interesting-face)
	 ("internet2.edu" . summary-low-interest-face)
	 ;; As does IP
	 ("ip@v2.listbox.com" . summary-med-interest-face)
	 )
	;; This regex still doesn't work...
	("\\(To\\|Cc\\|cc\\):"
	 ;;
	 ;; Stuff without Sender field
	 ("pma-discuss@gridpma.org" . summary-non-critical-face)
	 ;;
	 ;; Me
	 ("welch@mcs.anl.gov" . summary-me-face)
	 )
	;;
	;; Maillist bounces
	("To:"
	 ("glbs-owner-" . summary-low-interest-face)
	 ("\\(owner-cas-discuss\\|owner-security-announce\\|owner-gsi-openssh\\)@globus.org" . summary-low-interest-face)
	 )
	;;
	;; Catch approval requests
	("Subject:"
	 ("APPROVE" . summary-me-face)
	 )
	;;
	;; Subscription messages
	("From:"
	 ("\\(Majordomo@Globus.org\\|Majordomo@gridforum.org\\)" . summary-low-interest-face)
	 )
	;;
	;; Perl stuff w/o sender field
	("From:"
	 ("elists-admin@oreillynet.com" . summary-low-interest-face)
	 )
	)
      )

;; Calling this function modifies the above list
(vm-make-summary-hilit-alist-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-vm-summary)
