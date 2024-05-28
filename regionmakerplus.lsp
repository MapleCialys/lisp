;---------------------------------------------------------------------------------------------------;
; Auteur: Mickael FRANCOIS (mfrancois@ecceterra.com)                                                ;
; regionmaker.lsp (c) 2024                                                                          ;
; Desc: Créer une région a partir d'un ou plusieurs espaces fermés et soustrait les ilots.          ;
; Créé:  2024-04-25T15:06:59.625Z                                                                   ;
; Modifié: 2024-05-01T19:52:04.512Z
;---------------------------------------------------------------------------------------------------;
;---------------------------------------------------------------------------------------------------;
; Variables:                                                                                        ;
; modelSpace:    Object contenant l'espace modèle en cours.                                         ;
; startEntity:   Enregistre l'la dernière entitée créer avant de lancer la fonction.                ;
; sset:          Jeux de séléction des polylignes créer par la commande contour.                    ;
; lstRegion:     Liste des régions créées.                                                          ;
; nextEnt:       Plyligne suivante.                                                                 ;
; mainRegion:    Région principale.                                                                 ;
; ilot:          Régions secondaire pour soustraire a la région principale.                         ;
; varRegion:     Objet variant de la région en cours.                                               ;
; i:             Compteur de boucle.                                                                ;
; subVariant:    Objet variant a soustraire.                                                        ;
; pick:          Coordonées du points piqué dans l'espace fermé                                     ;
; addRegion:     Liste des régions à unir en fin de processus                                       ;
;---------------------------------------------------------------------------------------------------;

(defun c:regionmakerplus( / modelSpace startEntity sset lstRegion nextEnt mainRegion ilot varRegion i subVariant pick addRegion)
  ;Error handler 
    (defun *error* (msg)
      (if OSM (setvar 'osmode OSM))
      (if OLD_SCU (command "scu" "p"))
      (print msg)
      (princ)
    )
  ;Déclaration de variable et mise en état avant de démarer le processus.
    (setq   OSM (getvar 'osmode)
            OLD_SCU t
            modelSpace (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))
    (setvar 'osmode 0)
    (setvar 'cmdecho 0)
    (vl-cmdf "scu" "GE")
    (setq   sset (ssadd)
            lstRegion ())
  ;Point de repère de début de fonction
    (while (setq pick (getpoint "Cliquez a l'intérieur d'un espace férmé"))                         ;Début de la boucle pour multiple selection.
        (setq startEntity (entlast))  
        (command "-contour" "a" "i" "e" "" pick "")
        (setq nextEnt (entnext startEntity))
  ;Mise en jeux de selection des entités créées.
        (while (/= nil nextEnt)
          (setq sset (ssadd nextEnt sset))
          (setq nextEnt (entnext nextEnt))
        )
  ;Création des régions
        (setq mainRegion (vla-AddRegion modelSpace (ssname sset 0)))
        (setq addRegion (cons (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value mainRegion) 0)) addRegion))
        (setq i 1)
        (while (setq ilot (ssname sset i))
          (setq varRegion (vla-AddRegion modelSpace ilot))
          (setq lstRegion (cons varRegion lstRegion))
          (setq i (1+ i))
        )
  ;Soustraction des régions interne.
        (setq i (- (length lstRegion) 1))
        (while (>= i 0)
          (setq subVariant (nth i lstRegion))
          (vl-cmdf "soustraire" (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value mainRegion) 0)) "" (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value subVariant) 0)) "")
          (setq i (- i 1))
        )
        (redraw (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value mainRegion) 0)) 3)
        (command "_erase" sset "")
        (setq   sset nil
                lstRegion nil)    
        (setq   sset (ssadd)
                lstRegion ())
    )
    (setq i 1)
    (while (nth i addRegion)
    (vl-cmdf "union" (car addRegion) (nth i addRegion)"")
    (setq i (+ i 1)))
    (princ  (strcat "\nSupérficie de la région créée : "(rtos (vla-get-Area (vlax-ename->vla-object (car addRegion))) 2 2) " m²"))
  ;Suppression des polylignes.
    (command "_erase" sset "")
  ;Rétablissement des variables.
    (if OSM (setvar 'osmode OSM))
    (if OLD_SCU (command "scu" "p"))
    (princ)
)
;Lancement de la fonction au chargement.
(c:regionmakerplus)