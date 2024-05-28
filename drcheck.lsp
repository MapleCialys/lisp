; ****************************************************************************
; *                                DRCHECK.lsp                               *
; *           Effectue plusieurs vérification a la fin d'un dessin           *
; *                        Développé pour Bricscad v23                       *
; *                             Mickael FRANCOIS                             *
; *                                2023-06-07                                *
; ****************************************************************************


(defun c:drcheck( / msg cross ent_mesure rep sset)
  ;------------------------------------
  ;Chargement des fonctions nécessaires
  (vl-load-com)
  (load "crossingBox")
  (load "generationTalus")
  (load "scaleText")
  (load "ltscaleto1")
  ;-----------------------------------
  ;Déclaration des variables utilisées
  (setq OSM (getvar 'osmode))(setvar 'osmode 0)
  (setvar 'cmdecho 1)
  (setvar 'ctab "Model")
  (vl-cmdf "scu" "GE")
  (setq msg "Les problèmes suivant ont été trouvés:")
  (setq cross 0)
  ;---------------------------------
  ;Place les repères au premier plan
  (if (setq rep (ssget "_x" '((-4 . "<AND")
                                (410 . "Model")
                                (-4 . "<OR")
                                  (8 . "E-REP-IMPLANTATION")
                                  (8 . "E-REP-ARPENTAGE")
                                (-4 . "OR>")
                              (-4 . "AND>"))))
  (vl-cmdf "_draworder" rep ""  "A"))
;---------------------------------
;Mets les hachures en arrière plan
	(vl-cmdf "_hatchtoback")
;----------------------------------------------------------
;Vérifie si une flèche du nord est présente dans le dossier
	(if (not(ssget "_x" '((8 . "PAP-NORD") (2 . "nord-c"))))
     (setq msg (strcat  msg "\n-Aucune flèche du nord trouvée.")))
;----------------------------------------
;Vérifie si la trame de fond est présente
  (if (not (ssget "_x" '((8 . "PAP-HACHURE") (62 . 254))))
      (setq msg (strcat msg "\n-Aucune trame de fond de terrain trouvée.")))
;-------------------------------------------
;Vérifie si les lignes de lots sont présentes
  (if (not (ssget "_A" '((8 . "E-FON-PROPRIETE"))))
      (setq msg (strcat msg "\n-Aucune ligne E-FON-PROPRIETE trouvée.")))
;---------------------------------------------------
;Vérifie si les trames des servitudes sont présentes
  (if (ssget "_A" '((-4 . "<AND")
                      (8 . "E-FON-SERVITUDE")
                      (-4 . "<OR")
                         (0 . "LWPOLYLINE")
                         (0 . "LINE")
                      (-4 . "OR>")
                    (-4 . "AND>")))
      (if (not (ssget "_A" '((8 . "PAP-HACHURE") (2 . "DOTS2"))))
          (setq msg (strcat msg "\n-Aucune trame de servitude trouvée.")))
      )
;---------------------------------------------
;Vérifie la superposition des textes de mesure
(if (setq sset (ssget "_A" '((8 . "E-TXT-LOT-MESURE") (7 . "TX2") (100 . "AcDbText"))))
(progn
(while (setq ent_mesure (ssname sset 0))
       (if (MF:crossingBox ent_mesure)
           (progn
             (setq cross (+ 1 cross))
             (redraw ent_mesure 3)
           ))
       (ssdel ent_mesure sset)
)
(if (> cross 0)
  (setq msg (strcat msg "\n-Des superpositions ont étés trouvées dans les textes de mesures."))))
(setq msg (strcat msg "\n-Les textes de mesures n'ont pas étés trouvés.")))
;--------------------------------------------------
;Désactive la génération du type de ligne des talus
(if (setq sset (ssget "_A" '((-4 . "<AND")
                              (-4 . "<OR")
                                 (8 . "E-TOP-TALUS-HAUT")
                                 (8 . "E-TOP-TALUS-BAS")
                              (-4 . "OR>")
                              (0 . "LWPOLYLINE")
                              (-4 . "AND>"))))
(MF:generationTalus sset)
)
;----------------------------
;Vérifie l'echelle des textes
(if (MF:scaleText (ssget "_A" '((0 . "TEXT") (410 . "Model"))))
    (setq msg (strcat msg "\n-Certains textes ne sont pas à la bonne échelle."))
)
;-----------------------------------------------------------------
;Mets l'échelle de type de ligne a 1 pour les lignes et polylignes
(if (setq sset (ssget "_A" '((-4 . "<OR")
                              (0 . "LINE")
                              (0 . "LWPOLYLINE")
                             (-4 . "OR>"))))
  (MF:ltscaleto1 sset)
)
;------------------------------
;Impression du message d'erreur
(if (= msg "Les problèmes suivant ont été trouvés:")
  (alert "Aucun problème trouvé!")
  (alert msg)
)
;-----------------------------
;Remise en ordre des variables
(setvar 'osmode OSM)
(vl-cmdf "scu" "p")
  (princ)
)
(c:drcheck)