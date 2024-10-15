; 
; Auteur: Mickael FRANCOIS (mfrancois@ecceterra.com)
; zoomedWindow.lsp (c) 2024
; Desc: Crée une fenêtre d'agrandissement dynamique dans l'espace papier.
; Créé:  2024-06-06T18:41:35.114Z
; Modifié: 2024-08-19
; 
; c:zoomedWindow
; ---------------------
; Cette fonction permet de sélectionner une zone dans l'espace modèle et de créer une fenêtre d'agrandissement 
; dans l'espace papier en ajustant dynamiquement l'échelle à l'aide des touches + et -.
;
; @param point1 (Point) : Premier point sélectionné par l'utilisateur pour définir la zone d'agrandissement.
; @param point2 (Point) : Deuxième point pour définir la zone d'agrandissement.
; @param deltaX (Number) : Différence en X entre les points sélectionnés.
; @param deltaY (Number) : Différence en Y entre les points sélectionnés.
; @param deltaNegX (Number) : Négatif de la différence en X.
; @param deltaNegY (Number) : Négatif de la différence en Y.
; @param coordtmpframe (List) : Coordonnées pour créer un rectangle temporaire.
; @param loop (Boolean) : Contrôle la boucle de sélection de l'échelle.
; @param variateur (Number) : Indice pour changer l'échelle.
; @param framescale (Number) : Facteur d'échelle pour la fenêtre.
; @param scalelist (List) : Liste des échelles disponibles pour l'agrandissement.
; @param gr (List) : Résultat de l'entrée graphique (`grread`).
; @param tmpframe (EntityName) : Entité du cadre temporaire créé pour la prévisualisation.
; @param scaletext (EntityName) : Entité du texte affichant l'échelle.
; @param osm (Number) : Valeur originale de la variable `osmode` (mode de capture d'objets).
;
; *error*
; ---------------------
; Cette fonction gère les erreurs en rétablissant la valeur d'origine de `osmode` et en affichant le message d'erreur.
;
; @param msg (String) : Le message d'erreur à afficher.
;
; Utilisation :
; Exécute c:zoomedWindow pour créer une fenêtre d'agrandissement dynamique en sélectionnant une zone dans l'espace modèle.

(defun c:zoomedWindow( / point1 point2 deltaX deltaY deltaNegX deltaNegY
                      coordtmpframe loop variateur framescale scalelist
                      gr tmpframe scaletext osm)
;;Error handler
  (defun *error* (msg)
    (setvar 'osmode osm)
    (print msg)
  )
;;Chargement des dépendances
  (load"grmaker")
  (load"addcoord")
;;Configuration des variables
  (setq osm (getvar 'osmode))
  (setvar 'osmode 0)
  (setq scalelist (list "5000" "4000" "3000" "2500" "2000" "1500" "1200" "1000" "800" "600" "500" "400" "300" "250" "200" "150" "100" "50"))
;;Séléction de la zone a agrandire
  (vl-cmdf "scu" "v"); Au cas ou le scu n'est pas orienté général
  (setq point1 (getpoint "Sélectionnez la zone de l'agrandissement"))
  (setq point2 (getcorner point1))
;;Enregistrement des variables pour création de réctangle
  (setq deltaX (abs (-(car point1)(car point2))))
  (setq deltaY (abs (-(cadr point1)(cadr point2))))
  (setq deltaNegX (* -1 (-(car point1)(car point2))))
  (setq deltaNegY (* -1 (-(cadr point1)(cadr point2))))
;;Création du réctangle dans l'éspace modèle
  (setvar 'clayer "PAP-FEUIL-CADRE")
  (vl-cmdf "rectang" point1 point2)
;;Placement dynamique de la fenêtre d'agrandissement
  (setvar "ctab" (last (layoutlist)))
  (princ "\nUtilisez +/-  pour changer l'échelle")
  (setq loop t
    variateur 10)
  (while (and (setq gr (grread t 12 0)) loop)
  (setvar 'clayer "PAP-FEUIL-AIRE")
  (setq framescale (/ 1000 (atof (nth variateur scalelist))))
    (cond
      (
        (= (car gr) 5)
          (and tmpframe (entdel tmpframe) (setq tmpframe nil))
          (and scaletext (entdel scaletext) (setq scaletext nil))
          (setq coordtmpframe (MF:coord:rectangle (cadr gr) (* framescale deltaX) (* framescale deltaY)))
          (setq tmpframe (maker:frame coordtmpframe "PAP-FEUIL-AIRE"))
          (setq scaletext (maker:text (MF:addCoordDif (cadr gr) 3 -5) 2.5 "BOLD" "PAP-TEXTE" (strcat "AGRANDISSEMENT - 1:" (nth variateur scalelist))))
      )
      ((= (car gr) 2)
          (if (and (= (cadr gr) 43) (< variateur 17)); touche +
            (setq variateur (1+ variateur)))
          (if (and (= (cadr gr) 45) (> variateur 0)); touche -
            (setq variateur (1- variateur)))
      )
      ((= (car gr) 3)(setq loop nil)); click
    )
  )
;;Création de la fenêtre d'agrandissement
  (vl-cmdf "fmult" "o" tmpframe)
  (vl-cmdf "_mspace")
  (vl-cmdf "scu" "v")
  (vl-cmdf "zoom" "f" point1 point2)
  (setvar 'cannoscale (strcat "1:" (nth variateur scalelist)))
  (vl-cmdf "_pspace")
;;Rétablissement des variables
  (setvar 'osmode osm)
  (princ)
)
(c:zoomedwindow)