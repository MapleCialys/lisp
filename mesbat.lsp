
; ****************************************************************************
; *                                MESBAT.lsp                                *
; *          Inscrit rapidement les mesures intérieur d'un batiment          *
; *                        Développé pour Bricscad v23                       *
; *                             FRANCOIS Mickael                             *
; ****************************************************************************


(defun c:mesbat (/ ec K oldecho OSM OLAY CSTYL
                   HPBound_var Orthomode_varentpoly
                   superficie vl-poly pr turndist turnmid
                   TXTOR TXTINNER temp opp sel)
  
 ;;;Error handler personalisé pour rétablire les variables
(defun *error* (msg)
    (if oldecho (setvar 'cmdecho oldecho))
    (if OSM (setvar 'osmode OSM))
    (if OLAY (setvar 'clayer OLAY))
    (if CSTYL (setvar 'textstyle CSTYL))
    (if sel (vl-cmdf "_layuniso"))
    (princ)
    )
 
  (setq oldecho (getvar "cmdecho"))
  (setq OSM (getvar "OSMODE"))
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
  (if (not TX1)(load "TX1"))
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
  (vl-cmdf "_-layer" "_on" "E-TXT-DIMENSION" "")
  (setvar "clayer" "E-TXT-DIMENSION")
  (setvar "TEXTSTYLE" "tx1")
  
  ;;;Récupération du calque bâtiment et mise en place des variables
  (command-s  "._ucs" "GE" )
  (setvar "osmode" 0)
  (print "Sélectionner les objets a isoler:")
  (setvar 'nomutt 1)
  (if (not (setq sel (ssget)))
      (progn
        (setvar 'nomutt 0)
        (alert "Aucun objet trouvé.")
        (command)))
  (setvar 'nomutt 0)
  (vl-cmdf "_layiso" sel "")
  (vl-cmdf "_-layer" "_on" "E-TXT-DIMENSION" "")
  (setvar "clayer" "E-TXT-DIMENSION")
  ;;;Action utilisateur
  (setq p2 (getpoint "\nIndiquer un point intérieur: "))
  (command "._-boundary" "a" "i" "n" "" p2 "")
  (command "._AREA" "_o"(entlast))
  ;;;Algo principale
  (setq entpoly (entlast)
        vl-poly (vlax-ename->vla-object entpoly)
        pr -1)
  (repeat (fix (vlax-curve-getEndParam vl-poly))
    (setq
      pr (1+ pr)
      turndist (- (vlax-curve-GetDistAtParam vl-poly (1+ pr))
             (vlax-curve-GetDistAtParam vl-poly pr))
      turnmid (vlax-curve-getPointAtDist vl-poly (+ (vlax-curve-getDistAtParam vl-poly pr) (/ turndist 2 )))
      )
    (setq TXTOR (MF:ajustOrientation (vlax-curve-getPointAtParam vl-poly (1+ pr)) (vlax-curve-getPointAtParam vl-poly pr)))
    (setq opp (polar turnmid (- TXTINNER pi) (* 2 ec)))
  ;;;Inscription des textes
    (if (setq temp(ssget "_c" (MF:addCoord opp -1) (MF:addCoord opp 1) '((0 . "TEXT"))))
        nil
        (if (LM:collinear-p (vlax-curve-getPointAtParam vl-poly pr) turnmid (vlax-curve-getPointAtParam vl-poly (1+ pr)))
         (command "-texte" "m" (polar turnmid TXTINNER (* 2 ec)) TXTOR (MF:ajustPrecision turndist))
         (progn
         (command "-texte" "m" (polar turnmid TXTINNER (* 2 ec)) TXTOR (strcat "A:" (MF:ajustPrecision turndist)))
         (command "-texte" "m" (polar turnmid TXTINNER (* 4.5 ec)) TXTOR (strcat "R:" (MF:ajustPrecision (cadr (LM:3pcircle (vlax-curve-getPointAtParam vl-poly pr) turnmid (vlax-curve-getPointAtParam vl-poly (1+ pr)))))))
       ))
    )
  )
  (entdel entpoly)
  (vl-cmdf "_layuniso")
  (vl-cmdf "_-layer" "_on" "E-TXT-DIMENSION" "")
  (command-s  "._ucs" "V" )
  (setvar "HPBOUND" HPBound_var)
  (setvar "CMDECHO" oldecho)
  (setvar "osmode" OSM)
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

 (cond ((<= K 180.0)
    (if (or (>= TXTOR K+180) (< TXTOR K))
        (setq TXTOR (+ TXTOR 180.0)))
    )  
 ((> K 180.0)
    (if (and (>= TXTOR K+180) (< TXTOR K))
        (setq TXTOR (+ TXTOR 180)))
    )
 )
 (setq TXTOR TXTOR)
)
