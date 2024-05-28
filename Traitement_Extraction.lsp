; ****************************************************************************
; *                         TRAITEMENT_EXTRACTION.lsp                        *
; *          Transforme les objets venant de l'extraction cadastrale         *
; *                       Configuré pour Bricscad 2023                       *
; *                             Mickael FRANCOIS                             *
; *                                2023-04-19                                *
; *              Requiert: unzip-et.lsp; extract-et.lsp; DTR.lsp             *
; ****************************************************************************

(defun c:Traitement_Extraction(/ SS-STR SS-FLUSH SSPLINE NAME-PLINE NO-PLINE SS-SEG1 SS-SEG2
                                 SS-LOT COMPT ELMNT SS-DIM SS-RENV SSLINE SSARC test)
  
  (setvar "cmdecho" 0)
  (setq EC (/ 1 (getvar "cannoscalevalue")))
  
  (setvar "ANGBASE" (dtr 90))
  (setvar "ANGDIR"  1)
  (setvar "AUNITS"  1)
  (setvar "AUPREC"  3)
  (setvar "LUNITS"  2)
  (setvar "LUPREC"  4)
  ;;; insertion du prototype;;
  ;(command "._erase" (ssget "X" '((0 . "INSERT") (2 . "Dessin modele")))"")
  (command "._purge" "_a" "*" "_N")
  (command "_-purge" "_BL" "Dessin modele" "_N")
  (command "._insert" "C:\\BricsEcceterra\\Gabarit de dessin\\Dessin modele.dwg" "0,0,0" "1" "1" "")
  (setq test nil)
  
  	(setq test (tblsearch "layer" "E-FON-CADAS-OFF-LOT"))
  	(if (not test) (command "._-layer" "_N" "E-FON-CADAS-OFF-LOT" "_C" "132" "E-FON-CADAS-OFF-LOT" "_L" "LOT_CADAS_QUEBEC" "E-FON-CADAS-OFF-LOT" ""))
  	  (setq test nil)
  	(setq test (tblsearch "layer" "E-FON-CADAS-OFF-DIM"))
  	(if (not test) (command "._-layer" "_N" "E-FON-CADAS-OFF-DIM" "_C" "132" "E-FON-CADAS-OFF-DIM" ""))
        (setq test nil)
  	(setq test (tblsearch "layer" "E-FON-CADAS-OFF-NUM"))
  	(if (not test) (command "._-layer" "_N" "E-FON-CADAS-OFF-NUM" "_C" "_white" "E-FON-CADAS-OFF-NUM" ""))
  ; (print)(princ " C")
  
  (setq HT (* 3 EC))
  (command "._-style" "LOT" "Tahoma" HT "1.0" 0 "_N" "_N")
  (setq HT (* 1.3 EC))
  (command "._style" "TX1" "romans" HT "1.2" "" "" "" "")
  
  ;;;supression des texte de structure TR
  (if (not(null (ssget "X" '((0 . "TEXT")(1 . "TR")(8 . "STR_NO_LOT")))))
      (command "._erase" (ssget "X" '((0 . "TEXT")(1 . "TR")(8 . "STR_NO_LOT"))) "")
      )
  (if(not(null(ssget "X" '((0 . "TEXT")(1 . "TR")(8 . "CON_NO_LOT_STR")))))
      (command "._erase" (ssget "X" '((0 . "TEXT")(1 . "TR")(8 . "CON_NO_LOT_STR")))"")
      )
  ;;; transformation des texte de structure des couche STR_NO_LOT et CON_NO_LOT_STR en OFF_NO_LOT de style LOT
  ;;; avant la suppression des texte de structure
  (setq SS-GRA (ssget "X" '((-4 . "<OR")
                            (8 . "Str_no_lot")
                            (8 . "Con_no_lot_str")
                            (-4 . "OR>"))))
  ;(print SS-GRA)
  (if (not(null SS-GRA))
      (progn
        (if (not (tblsearch "LAYER" "OFF_NO_LOT"))
            (command "._layer" "_N" "OFF_NO_LOT" ""))
        (command "._change" SS-GRA "" "_P" "_LA" "OFF_NO_LOT" "")
        (setq COMPT 0)
        (setq ELMNT-GRA (ssname SS-GRA compt))
        (while ELMNT-GRA
               (command "._change" ELMNT-GRA "" "" "" "LOT" "" "" )
               (setq compt (1+ compt))
               (setq ELMNT-GRA (ssname SS-GRA compt)) 
               )
        )
      )
  
  ;;; Suppression des texte de structures;;;
  (setq SS-STR (ssget "X" '((0 . "TEXT")(7 . "STR"))))
  (if (not(null SS-STR))
      (command "._erase" SS-STR "" ))
  
  ;;;Supression des element non pertinent au plan de travail;;;
  (setq SS-FLUSH (ssget "X" '((-4 . "<OR")
                              (8 . "Gra_co_ech_rep")
                              (8 . "Gra_nm_feu_hab")
                              (8 . "Gra_nm_feu")
                              (8 . "GRA_CON_FEU")
                              (8 . "AGR_BUL_REF")
                              (8 . "AGR_CON_LOC")
                              (8 . "AGR_CON_REF")
                              (8 . "GRA_REN_AGR_BUL")
                              (8 . "CON_NO_LOT_GRA")
                              (8 . "*_STR*")
                              (8 . "*STR_*")
                              (-4 . "OR>"))))
  
  (if (not(null SS-FLUSH))
      (command "._erase" SS-FLUSH "" ))
  ;;;Explosion des polylines;;;
  
  
  (setq SSpLINE (ssget "X" '((-4 . "<OR")
                             (0 . "LWPOLYLINE")
                             (0 . "POLYLINE")
                             (-4 . "OR>")
                             (8 . "*_seg_*"))))
  
  
  (if (not(null sspLINE))
      (progn
        (setq NAME-PLINE (ssname SSpLINE 0))
        (setq NO-PLINE 0)
        (while NAME-PLINE
               (command "._explode" NAME-PLINE )
               (setq no-pline (1+ NO-PLINE))
               (setq NAME-PLINE (ssname SSpLINE NO-PLINE))
               );fin while
        );fin progn
      );fin if
  (setq SS-SEG1 (ssget "X" '((-4 . "<OR")
                             (8 . "Off_seg_lot")
                             (8 . "Con_seg_lot")
                             (-4 . "OR>"))))     
   (if (not(null SS-SEG1))                                                    
   (command "._change" SS-SEG1 "" "_P" "_C" "bylayer" "_LT" "bylayer" "_LA" "E-FON-CADAS-OFF-LOT" ""))
  
  
  (setq SS-SEG2 (ssget "X" '((-4 . "<OR")
                             (8 . "Off_seg_tnc")
                             (8 . "Off_seg_cf")
                             (8 . "Off_seg_cf_tnc")
                             (8 . "Con_seg_tnc")
                             (8 . "Con_seg_cf")
                             (8 . "Con_seg_tnc")
                             (8 . "Con_seg_cf_tnc")
                             (8 . "Gre_seg_cir_fon")
                             (-4 . "OR>"))))
  (if (not(null SS-SEG2))
      (command "._change" SS-SEG2 "" "_P" "_C" "140" "_LT" "bylayer" "_LA" "E-FON-CADAS-OFF-LOT" ""))
  ;;;Modification des propriete. des nos de lot;;;
  
  (setq SS-LOT (ssget "X" '(
                            (-4 . "<AND")
                            (0 . "TEXT")
                            (-4 . "<OR")
                            (8 . "Off_no_lot")
                            (8 . "GRA_NO_LOT")
                            (8 . "Con_no_lot") 
                            (-4 . "OR>")
                            (-4 . "AND>")
                            )
                      )
        )
  (if (not(null SS-LOT)) 
      (progn
        (command "._change" SS-LOT "" "_P" "_C" "bylayer" "_LA" "E-FON-CADAS-OFF-NUM" "")
        (setq COMPT 0)
        (setq ELMNT (ssname SS-LOT compt))
        (while ELMNT
               (command "._change" ELMNT "" "" "" "LOT" "" "" )
               (setq compt (1+ compt))
               (setq ELMNT (ssname SS-LOT compt)) 
               );fin while
        );fin progn
      );fin if
  
  ;;;Modification des porp. des superficies et des dimensions;;;
  (setq SS-DIM (ssget "X" '(
                            (-4 . "<AND")
                            (0 . "TEXT")
                            (-4 . "<OR")
                            (7 . "SUP")
                            (7 . "DIM")
                            (7 . "PCA")
                            (7 . "CIR")
                            (7 . "IDE")
                            (7 . "HYD")
                            (7 . "ODO")
                            (7 . "CAD")
                            (8 . "Off_lo_lig")
                            (8 . "Off_lo_ray")
                            (8 . "Off_sp_lot")
                            (8 . "CON_INF_GEN")
                            (8 . "PCR_SP_LOT")
                            (8 . "STR_SP_LOT")
                            (8 . "PCR_LO_LIG")
                            (8 . "PCR_LO_RAY")
                            (8 . "GRA_NM_PLA_COM")
                            (8 . "GRA_NM_CIR_FON")
                            (8 . "GRA_NM_IDE_TER")
                            (8 . "GRA_NM_TOP_HYD")
                            (8 . "GRA_NM_ODONM")
                            (-4 . "OR>")
                            (-4 . "AND>")
                            )
                      )
        );fin setq ss-dim
  (if (not(null SS-DIM)) 
      (progn
        (command "._change" SS-DIM "" "_P" "_C" "bylayer" "_LA" "E-FON-CADAS-OFF-DIM" "")
        
        (setq COMPT 0)
        (setq ELMNT (ssname SS-DIM compt))
        (while ELMNT
               (command "._change" ELMNT "" "" "" "TX1" "" "")
               (setq compt (1+ compt))
               (setq ELMNT (ssname SS-DIM compt)) 
               );fin while
        );fin progn
      );fin if
  
  ;;; Modification des renvois;;;
  (setq SS-RENV (ssget "X" '((-4 . "<OR")
                             (8 . "Gra_ren_no_lot")
                             (8 . "Gra_ren_sup_lot")
                             (8 . "Gra_ren_lo_lig")
                             (8 . "Gra_ren_nm_plan")
                             (8 . "Pcr_ren_sp_lot")
                             (8 . "Pcr_ren_lo_lig")
                             (8 . "Gra_pte_fle")
                             (8 . "Pcr_pte_fle")
                             (8 . "Gra_ren_nm_hydr")
                             (8 . "Gra_ren_nm_odon")
                             (8 . "Gra_ren_agr_bul")
                             (8 . "Gra_ren_ide_ter")
                             (8 . "Con_ren_no_lot")
                             (8 . "Con_pte_fle")
                             (8 . "Con_ren_nm_plan")
                             (8 . "Con_inf_gen")
                             (8 . "GRA_REN_SP_LOT")
                             (8 . "GRA_REN_NM_PLAN")
                             (8 . "GRA_REN_IDE_TER")
                             (-4 . "OR>"))))
  (if (not(null SS-RENV))
      (command "._change" SS-RENV "" "_P" "_C" "bylayer" "_LA" "E-FON-CADAS-OFF-DIM" ""))
  (command "._purge" "_LA" "*_*" "_N")
  ;;; doublons ;;;
  
  (command "_-overkill" "_all" "" "")
  );fin defun 
