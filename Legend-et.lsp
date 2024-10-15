; 
; Auteur: Mickael FRANCOIS (mfrancois@ecceterra.com)
; legend-et.lsp (c) 2024
; Desc: Programme de création de légende automatique
; Créé:  2022
; Modifié: 2024-06-13T20:31:18.129Z
; Dois etre compiler avec legend-db.lsp 
;

(defun c:legend-et ( / blname basecadre inbase lstblock lstlmod lstmod
                       lstline lstall ins arbre loop variateurx variateury
                       tmpframe coordframe compt maxcol bot i y top *error*)

;;;Error handler personalisé pour rétablire les variables
  (defun *error* (msg)
    (if oldecho (setvar 'cmdecho oldecho))
    (if OSM (setvar 'osmode OSM))
    (if OLAY (setvar 'clayer OLAY))
    (if CSTYL (setvar 'textstyle CSTYL))
  )
;;Réglage des variables
  (setvar "cmdecho" 0)
  (setq OSM (getvar "OSMODE"))
  (setvar "osmode" 0)
;;Déclaration des jeux de sélection et listes.
  (setq lstmod (ssadd))
  (setq lstlmod (ssadd))
  (setq lstline (ssadd))
  (setq lstblock (ssadd))
  (setq ins (list 0 0 0))
;;Récupération des éléments a mettre en légende.
  (setq lstall (ssget '((-4 . "<OR") (0 . "INSERT") (0 . "LWPOLYLINE") (0 . "LINE") (-4 . "OR>"))))
;;Sépare les blocs et les lignes du jeux de sélection principale.
  (MF:splitss lstall)
;;Supprime les doublons des jeux de selection.
  (MF:removedouble lstblock)
  (MF:removedbline lstline)
;;Passe au dernier onglet de présentation du dessin.
  (setvar "ctab" (last (layoutlist)))
;;Boucle avec le cadre temporaire
(setq loop t
      variateurx 1  ;nombre de colonne
      variateury (+ (* (sslength lstmod) 4) (* (sslength lstlmod) 5) 13)  ;Hauteur du cadre temporaire
      compt (+ (sslength lstmod)(sslength lstlmod))) ;Compteur total d'element dans la légende
(while (and (setq gr (grread t 12 0)) loop)
  (cond
    (
      (= (car gr) 5)  ; Déplacement de souris
        (and tmpframe (entdel tmpframe) (setq tmpframe nil))
        (setq coordframe
          (list
            (list (car(cadr gr)) (cadr(cadr gr)))
            (MF:addCoordDif (cadr gr) (* 60 variateurx) 0)
            (MF:addCoordDif (cadr gr) (* 60 variateurx) variateury)
            (MF:addCoordDif (cadr gr) 0 variateury)
          )
        )
        (setq tmpframe (maker:frame coordframe "0"))  ;Créer le cadre temporaire
    )
    ((= (car gr) 2) ; Touche du clavier
        (if (and (= (cadr gr) 43) (< variateurx compt)) ; Touche +, pas plus grand que le nombre d'élement dans la légende
          (setq variateurx (1+ variateurx)
                variateury (/ (+ (* (sslength lstmod) 4) (* (sslength lstlmod) 5) 13) variateurx)))
        (if (and (= (cadr gr) 45)(> variateurx 1))  ; Touche -, pas plus petit que 1
          (setq variateurx (1- variateurx)
                variateury (/ (+ (* (sslength lstmod) 4) (* (sslength lstlmod) 5) 13) variateurx)))
    )
    ((= (car gr) 3) ; Clic de souris
      (setq loop nil)
      (setq ins (cadr gr))
      (and tmpframe (entdel tmpframe) (setq tmpframe nil))
    )
  )
)
(setq maxcol (ceiling (/(+ (sslength lstmod)(sslength lstlmod)) variateurx 1.)))  ; Nombre de ligne max par colonne
;;;---------------------------Début algo principal---------------------------
;Préparation du cadre
  (setq insbase ins)
  (setq basecadre (list (+ 3.0 (nth 0 ins)) (nth 1 ins) 0.0))
  (setq ins (list (+ 10.0 (nth 0 ins)) (+ 3.0 (nth 1 ins)) 0.0))
  (setq bot (cadr ins))
;Trace les lignes
(setq y 0)
  (MF:ft_putline lstlmod)
  (command-s "._-layer" "_s" "PAP-TEXTE" "")
  (if (not(null lstmod))
;Trace les blocs
    (progn
      (setq i 0
            arbre 0
            top (cadr ins))
      (setq ename (ssname lstmod i))
      (while ename
        (if (= y maxcol)  ; Nombre de ligne max par colonne
          (progn  
            (if (< top (cadr ins))  ; Garde le point le plus haut de la liste de ligne
              (setq top (cadr ins)))
            (setq y 0
                  ins (list (+ 60 (car ins)) bot))  ; Passe à la colonne suivante
          )
        )
        (setq canno (cond ;Vérifie si le bloc est annotatif
                      ((not (vl-annotative-getscales ename)) 1)
                      (t 1000)))
        (setq blname (cdr (assoc 2 (entget ename))))
        (command-s "._-layer" "_s" (cdr (assoc 8 (entget ename))) "")        
              (cond
        ((OR (= blname "ARBFEUIL") (= blname "ARBCONIF")) ;Conditions spécifiques pour les arbres
          (progn 
            (if (= arbre 0)
            (progn
              (command-s "._insert" "ARBFEUIL" "e" (* canno 0.7) (list (- (nth 0 ins) 2.5) (nth 1 ins)) "")
              (command-s "._insert" "ARBCONIF" "e" (* canno 0.7) (list (+ (nth 0 ins) 2.5) (nth 1 ins)) "")
              (setq arbre (1+ arbre))
            )(setq arbre (1+ arbre)))
          )
        )
        (t (vl-cmdf "._insert" blname "e" canno ins ""))
      )
        (if (AND (not (= blname "ARBFEUIL")) (not (= blname "ARBCONIF")))
        (progn
          (MF:ft_puttext blname)
          (setq ins (list (nth 0 ins) (+ 4.0 (nth 1 ins)) 0.0))

        )
        (if (< arbre 2)
        (progn
          (MF:ft_puttext blname)
          (setq ins (list (nth 0 ins) (+ 4.0 (nth 1 ins)) 0.0))

        )))
          (setq i (1+ i)
                y (1+ y))
          (setq ename (ssname lstmod i))
      )
      (if (= variateurx 1)(setq top (cadr ins)))   
;Trace le cadre
    (MF:ft_putframe)
    )
    (princ)
  )
  (setvar "osmode" OSM)
  (princ)
)

; *
; MF:splitss: Sépare les blocs et les lignes
; @params: lst (SelectionSet) jeux de sélection principal
; *
(defun MF:splitss (lst / e i n )
          (progn
            (setq i 0
                  n (sslength lst)
            )
            (while (< i n)
                (setq e (ssname lst i)
                      i (1+ i)
                )
                (if (= (cdr (assoc 0 (entget e))) "INSERT")
                     (ssadd e lstblock)
                     )
                     (ssadd e lstline)
            )
        )
          (princ)
)

; *
; MF:removedouble: Supprime les doublons d'un jeux de sélection.
; @params: lst (SelectionSet) jeux de sélection à filtrer
; *
(defun MF:removedouble ( lst / e i n y n2 e2 spotted )
        (progn
            (setq i 0
                  n (sslength lst)
            )
            (while (< i n)
                (setq e (ssname lst i)
                      i (1+ i)
                      y 0
                      spotted 0
                      n2 (sslength lstmod)
                )
                (while (< y n2)
                  (setq e2 (ssname lstmod y)
                        y (1+ y)
                  )
                  (if (= (cdr (assoc 2 (entget e))) (cdr (assoc 2 (entget e2))))
                     (setq spotted 1))
                  )
;;Supprime les blocs non voulu pour la légende
                (setq spotted (cond
                    ((= spotted 1) 1)
                    ((= (cdr (assoc 2 (entget e))) "maison") 1)
                    ((= (cdr (assoc 2 (entget e))) "Flaz") 1)
                    ((= (cdr (assoc 2 (entget e))) "PointCalcul") 1)
                    ((= (cdr (assoc 2 (entget e))) "flech-evidee") 1)
                    ((= (cdr (assoc 2 (entget e))) "poincote") 1)
                    ((= (cdr (assoc 2 (entget e))) "galerie") 1)
                    ((= (cdr (assoc 2 (entget e))) "IMPL") 1)
                    ((= (cdr (assoc 2 (entget e))) "RIT") 1)
                    ((= (cdr (assoc 2 (entget e))) "nord-c") 1)
                    ((= (cdr (assoc 2 (entget e))) "REPCHEST") 1)
                    (t 0))
                )
                (if (= spotted 0)
                  (ssadd e lstmod))    
            )
        )
    (princ)
)

; *
; MF:removedbline: Supprime les lignes en double dans un jeux de sélection.
; @params: lst (SelectionSet) Jeux de selection de ligne
; *
(defun MF:removedbline ( lst / e i n y n2 e2 spotted use )
(progn
            (setq i 0
                  n (sslength lst)
            )
            (while (< i n)
                (setq e (ssname lst i)
                      i (1+ i)
                      y 0
                      spotted 0
                      use 0
                      n2 (sslength lstlmod)
                )
                (while (< y n2)
                  (setq e2 (ssname lstlmod y)
                        y (1+ y)
                  )
                  (if (= (cdr (assoc 8 (entget e))) (cdr (assoc 8 (entget e2))))
                     (setq spotted 1))
                  )
;Trie les lignes à garder dans la légende
                (setq use (cond
                    ((= (cdr (assoc 8 (entget e))) "E-VEG-BOISE") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-SER-ELEC-TEL-AERIEN") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-FON-CLOTURE") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-TOP-TALUS-HAUT") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-TOP-TALUS-BAS") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-VEG-HAIE") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-VEG-ALIGNEM-ARBRES") 1)
                    ((= (cdr (assoc 8 (entget e))) "E-FON-PROPRIETE") 1)
                    (t 0))
                )
                (if (AND (= spotted 0) (= use 1))
                  (ssadd e lstlmod))
            )
        )
    (princ)
)

; *
; MF:ft_puttext: Ecris les définitions de la légende
; @params: blname (ename) ename du bloc a définir
; *
(defun MF:ft_puttext (blname / txt instext)
    (command-s "._-layer" "_s" "PAP-TEXTE" "")
    (setq instext  (list (+ 13.0 (nth 0 ins)) (nth 1 ins) 0.0))
    (setq txt (MF:legend-db blname))
    (command-s "._TEXT" "_s" "arial" "_j" "mg" instext "2" "90" txt)
  (princ)  
)

; *
; MF:ft_putframe: Trace le cadre
; *
(defun MF:ft_putframe ( / point1 point2 pointtxt lgd-frame title-frame topline)


    (setvar 'clayer "PAP-TEXTE")
    (if (> variateurx 1)
      (setq topline (rem compt 2))
      (setq topline 0))
    (setq point1 (list (+ (* variateurx 60.0) (nth 0 insbase)) top)
          point2 (list (nth 0 insbase) (+ 6.0 (nth 1 point1)))
          pointtxt (list (+ (/ (* variateurx 60.0)2) (nth 0 insbase)) (+ 3.0 (nth 1 point1)))
        )
(command-s "_rectang" "_f" "1" insbase point1)
(setq lgd-frame (entlast))
(command-s "_rectang" "_f" "1" point2 point1)
(setq title-frame (entlast))
(command-s "._TEXT" "_s" "bold" "_j" "m" pointtxt "2.5" "90" "L\\U+00C9GENDE")
(setvar 'hpcolor "255,255,255")
(setvar 'hptransparency "0")
(command-s "-hachures" "p" "s" "s" lgd-frame title-frame "" "")
(command-s "_hatchtoback")
)

; *
; MF:ft_putline: Trace les lignes de la légende
; @params: lstmod (SelectionSet) Jeux de sélection a traiter
; *
(defun MF:ft_putline (lstlmod / ename lname i)
    (setq i 0)
    (setq ename (ssname lstlmod i))
    (while ename
(if (= y maxcol)
            (setq y 0
                  ins (list (+ 60 (car ins)) bot)))
      (setq lname (cdr (assoc 8 (entget ename))))
      (setvar "clayer" lname)
;;Condition spéciales pour certaines lignes
      (cond
        ((= lname "E-SER-ELEC-TEL-AERIEN") (command-s "._insert" "elec-leg" ins 1 "" ""))
        ((= lname "E-FON-CLOTURE") (command-s "._insert" "clot-leg" ins 1 "" ""))
        ((= lname "E-TOP-TALUS-HAUT") (command-s "._LINE" (list (- (nth 0 ins) 7.5) (- (nth 1 ins) 1)) (list (+ 7.5 (nth 0 ins)) (- (nth 1 ins) 1)) ""))
        ((= lname "E-TOP-TALUS-BAS") (command-s "._LINE" (list (- (nth 0 ins) 7.5) (- (nth 1 ins) 1)) (list (+ 7.5 (nth 0 ins)) (- (nth 1 ins) 1)) ""))
        ((= lname "E-FON-PROPRIETE") (command-s "._PLINE" (list (- (nth 0 ins) 7.5) (nth 1 ins)) "e" "0.6" "" (list (+ 7.5 (nth 0 ins)) (nth 1 ins)) ""))
        (t (command-s "._LINE" (list (- (nth 0 ins) 7.5) (nth 1 ins)) (list (+ 7.5 (nth 0 ins)) (nth 1 ins)) "")) 
      )
      (MF:ft_puttext lname)
      (setq i (1+ i)
            y (1+ y))
      (setq ename (ssname lstlmod i))
      (setq ins (list (nth 0 ins) (+ 5.0 (nth 1 ins)) 0.0))
    )
    (princ)
)