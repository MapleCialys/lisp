
*; ****************************************************************************
; *                                LOTISS.lsp                                *
; *              Ecris rapidement les dimensions d'une parcelle              *
; *                       Développer pour Bricscad v23                       *
; *                             FRANCOIS Mickael                             *
; *                                2023-07-04                                *
; ****************************************************************************
;ec = Echelle en cours
;K = Variable de constante qui gère l'angle entre le SCU view et general
;oldecho = Variable echo de commande
;OSM = Variable osmode
;OLAY = Variable calque courant
;CSTYL = Variable style de texte
;HPBound_var =Variable HPbound
;Orthomode_var = Variable Orthomode
;entpoly =Entname de la polyligne
;superficie = Superficie de la parcelle
;vl-poly = Objet VL de la polyligne
;pr = Param actuel du parcours de la polyligne
;turndist = Mesure de l'objet
;turnmid = Coordonées du milieu de l'objet
;TXTOR = Orientatiojn du texte de meesure
;TXTINNER = Angle dirigeant vers l'intérieur de la forme géometrique
;opp = Coordonées de l'opposé du point d'insertion du texte de mesure
;txtpos = Distance entre le point mid de l'objet et le texte de mesure
;rpos = Valeure d'ajout pour le placement des textes de rayon

(defun c:lotiss (/ ec K oldecho OSM OLAY CSTYL
                   HPBound_var Orthomode_var entpoly
                   superficie vl-poly pr turndist turnmid
                   TXTOR TXTINNER temp opp txtpos)
  
 ;;;Error handler personalisé pour rétablire les variables
(defun *error* (msg)
    (if oldecho (setvar 'cmdecho oldecho))
    (if OSM (setvar 'osmode OSM))
    (if OLAY (setvar 'clayer OLAY))
    (if CSTYL (setvar 'textstyle CSTYL))
    (princ)
    )
 
  (setq oldecho (getvar "cmdecho"))
  (setq OSM (getvar "OSMODE")

    )
  (setq OLAY(getvar "CLAYER"))
  (setq CSTYL (getvar "TEXTSTYLE"))
  (setq HPBound_var (getvar "HPBOUND"))
  (setq Orthomode_var(getvar "orthomode"))
 
  ;;;Chargement et executions des fonctions de calcul d'angle
  (load "3pcircle")
  (load "addcoord")
  (load "ajustPrecision")
  (load "collinear-p")
  (if (not Konst)(load "Konst"))
  (if (not RTD)(load "RTD"))
  (if (not angle-base)(load "angle-base"))
  (konst)
  (setq K (atof(rtos (rtd K) 2 3)))
  (cond ((= K 360.0)(setq K 0.0)))
  (vl-load-com)
  (if (not TX2)(load "TX2"))
  (c:TX2)
  (if (not TX1)(load "TX1"))
  (c:TX1)
  
  ;;;Allumage des calques et prise de l'échelle
  (setvar "CMDECHO" 0)
  (setq ec (/ 1 (getvar "cannoscalevalue")))
  (vl-cmdf "_-layer" "_on" "E-TXT-LOT-SUPERFICIE" "")
  (vl-cmdf "_-layer" "_on" "E-TXT-LOT-MESURE" "")
  (setvar "clayer" "E-TXT-LOT-MESURE")
  (setvar "TEXTSTYLE" "tx2")
  
  ;;;Execution utilisateur
  (command-s  "._ucs" "GE" )
  (setvar "osmode" 0)
  (setq p2 (getpoint "\nIndiquer un point intérieur: "))
  (command "._-boundary" "a" "i" "n" "" p2 "")
  (command "._AREA" "_o"(entlast))
  (setq entpoly (entlast))
  (setq superficie1 (getvar "AREA")
        vl-poly (vlax-ename->vla-object entpoly)
        pr -1)
  (repeat (fix (vlax-curve-getEndParam vl-poly))
    (setq
      pr (1+ pr)
      turndist (- (vlax-curve-GetDistAtParam vl-poly (1+ pr))
             (vlax-curve-GetDistAtParam vl-poly pr))
      turnmid (vlax-curve-getPointAtDist vl-poly (+ (vlax-curve-getDistAtParam vl-poly pr) (/ turndist 2 )))
      )
       (If (and (/= 0.0 (getvar "userr5"))(/= 1.0 (getvar "userr5")))
      (progn
         (setq FComb (rtos (getvar "userr5") 2 8))
         (setq turndist (* turndist (/ 1 (getvar "userr5"))))
      )
        (setq FComb NIL)
      )
    (setq TXTOR (MF:ajustOrientation (vlax-curve-getPointAtParam vl-poly (1+ pr)) (vlax-curve-getPointAtParam vl-poly pr)))
    (setq opp (polar turnmid (- TXTINNER pi) (* 2 ec)))
    (if (= (rtos turndist 2 2) "0.00")
        nil
    (if (ssget "_c" (MF:addCoord opp -1) (MF:addCoord opp 1) '((0 . "TEXT")))
        nil
        (if (LM:collinear-p (vlax-curve-getPointAtParam vl-poly pr) turnmid (vlax-curve-getPointAtParam vl-poly (1+ pr)))
         (command "-texte" "c" (polar turnmid TXTINNER txtpos) TXTOR (MF:ajustPrecision turndist))
         (progn
         (command "-texte" "c" (polar turnmid TXTINNER txtpos) TXTOR (strcat "A:" (MF:ajustPrecision turndist)))
         (command "-texte" "c" (polar turnmid TXTINNER (* (* 2 rpos) txtpos)) TXTOR (strcat "R:" (MF:ajustPrecision (cadr (LM:3pcircle (vlax-curve-getPointAtParam vl-poly pr) turnmid (vlax-curve-getPointAtParam vl-poly (1+ pr)))))))
       ))
    ))
  )
  (entdel entpoly)
  (if FComb
         (setq superficie1 (/ superficie1 (expt (getvar "userr5")2 ))))
  (if (not sep-mill-0)(load "sep-mill-0"))
  (if (not sep-mill-1)(load "sep-mill-1"))
  (if (not sep-mill-2)(load "sep-mill-2"))
   
  (cond
    ((or (= UNITE "MM")(not UNITE))
     (setq AREA (sep-mill-1 superficie1))                      ;EN METRE
     (if (not CAD)
         (setq sup (strcat "S: " area " m²" ))
         ;(setq sup (strcat area " m²" ))
         (setq sup (strcat "S: " area ))
         )
     )
    
    ((or (= UNITE "M")(= UNITE 0))
     ;(setq AREA (sep-mill-0 (ATOI (Rtos (getvar "area")2 0))))                    ;EN METRE AVEC SEPARATEUR MILLIER   
     (setq AREA (Rtos superficie1 2 0))                                       ;EN METRE SANS SEPARATEUR MILLIER   
     (if (not CAD)
         (setq sup (strcat "S:" area "m²" ))
         ;(setq sup (strcat area "m²" ))
         (setq sup (strcat "S: " area ))
         )
     )
    
    ((= UNITE "P")
     (setq AREA (sep-mill-0 (/ superficie1 0.09290304)))           ;EN PIEDS
     ;(setq sup (strcat "(" area  " pi )"))   ;avec parenthese
     (setq sup (strcat   "S: " area  " pi²"))    ;sans parenthese
	    ;(setq sup area)
     )
    
   + ((= UNITE "PP")
     (setq AREA (sep-mill-1 (/ superficie1 0.09290304)))           ;EN PIEDS
     ;(setq sup (strcat "(" area  " pi )"))   ;avec parenthese
     (setq sup (strcat   "S: " area  " pi²"))    ;sans parenthese
     
     )
    ((or (= UNITE "A")(= UNITE "a"))
     (setq AREA (sep-mill-2 (/ superficie1 4046.856422)))           ;EN ACRES
     (setq sup (strcat "S: " area  " acres"))
     )
    ((or (= UNITE "H")(= UNITE "h"))
     (setq AREA (sep-mill-0 (/ superficie1 10000.0)))           ;EN ACRES
     (setq sup (strcat "S: " area  " ha"))
     )
    
    
    
    );fin cond

  
  (command-s  "._ucs" "V" )
  (load "verif_layer")
  (verif_layer "E-TXT-LOT-SUPERFICIE")
  (vl-cmdf "_-layer" "_on" "E-TXT-LOT-SUPERFICIE" "")
  (setvar "clayer" "E-TXT-LOT-SUPERFICIE")
  
  (command-S "._text" "_m" (trans p2 0 1) 90 sup)
  
  (SETQ S2 superficie1)
  

  (setq s2pi (* S2 10.7639))
  (princ)
  (princ (rtos S2 2 1))
  (princ " m2 , ")
  (princ (rtos s2pi 2 1))
  (princ " pi2")
  (princ " - FC: ")
  (princ fcomb)
  
  (setvar "HPBOUND" HPBound_var)
  (setvar "CMDECHO" oldecho)
  (setvar "osmode" osm)
  (setvar "CLAYER" OLAY)
  (setvar "TEXTSTYLE" CSTYL)
  (princ) 
  )

(defun MF:ajustOrientation(P1 P2 / TXTOR )
  "Ajuste l'orientation en fonction du SCU"
 (setq P1P2 (angle P1 P2))
 (setq TXTOR (atof (angtos P1P2 0 3))
       TXTINNER (- (angtof (rtos TXTOR 2 4)) (/ pi 2)))
 (If (< 360.0 (+ K 180.0)) (setq K+180 (- K 180.0))
   (setq K+180 (+ K 180.0)))
 (setq rpos 1)
 (cond ((<= K 180.0)
    (if (or (>= TXTOR K+180) (< TXTOR K))
        (progn
        (setq TXTOR (+ TXTOR 180.0))
        (setq txtpos (* 1.6 ec))
        (setq rpos 1.6))
        (setq txtpos (* 3.2 ec))
    ))  
 ((> K 180.0)
    (if (and (>= TXTOR K+180) (< TXTOR K))
        (progn
        (setq TXTOR (+ TXTOR 180))
        (setq txtpos (* 1.6 ec))
        (setq rpos 1.6))
        (setq txtpos (* 3.2 ec))
    ))
 )
 (setq TXTOR TXTOR)
)
