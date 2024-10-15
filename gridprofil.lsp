; 
; Auteur: Mickael FRANCOIS (mfrancois@ecceterra.com)
; gridprofil.lsp (c) 2024
; Desc: Selectionne une polyligne, applique les marquages d'ingénieur et génère la grille de profile en fonction.
; Créé:  2024-05-01T20:51:18.698Z
; Modifié: 2024-06-05T20:20:51.309Z
; 

;/
;flatline ; Créer une version 2D d'une poliligne 3D
;@params: s (selection set); jeux de selection d'une polyligne
;return: nil;
;/
(defun flatline ( s / e i l v x )
    (if s
        (repeat (setq i (sslength s))
            (setq e (ssname  s (setq i (1- i)))
                  l (entget  e)
                  e (entnext e)
                  x (entget  e)
                  v nil
            )
            (while (eq "VERTEX" (cdr (assoc 0 x)))
                (setq v (cons (assoc 10 x) v)
                      e (entnext e)
                      x (entget e)
                )
            )
            (if (entmake
                    (append
                        (list
                           '(000 . "LWPOLYLINE")
                           '(100 . "AcDbEntity")
                           '(100 . "AcDbPolyline")
                            (cons  038 (cadddr (last v)))
                            (cons  090 (length v))
                            (cons  070 (logand 129 (cdr (assoc 70 l))))
                            (assoc 008 l)
                            (cond ((assoc 006 l)) ('(006 . "BYLAYER")))
                            (cond ((assoc 039 l)) ('(039 . 0.0)))
                            (cond ((assoc 062 l)) ('(062 . 256)))
                            (cond ((assoc 370 l)) ('(370 . -1)))
                            (assoc 210 l)
                            (assoc 410 l)
                        )
                        (mapcar '(lambda ( v ) (list 10 (cadr v) (caddr v))) (reverse v))
                    )
                )
                (entdel (cdr (assoc -1 l)))
            )
        )
    )
    (princ)
)

;/
;markamker ; Ecris les repères de distance sur une polyligne 2D
;@params: flatpoly (vl-object) ;Polyligne 2D
;         etape (int) ; Espacement des repères
;         longueur (real) ; Longueur de la polyligne
;return: (lst) ; Liste des entités créées
;/
(defun markmaker (flatpoly etape longeur / marklst i point deriv tang valeur mark)
(setq marklst())
(setq i 0)
  (while (< i longueur)
    (setq point (vlax-curve-getPointAtDist flatpoly i))

    (setq deriv (vlax-curve-getfirstderiv flatpoly (vlax-curve-getparamatdist flatpoly i)))
    (setq tang (+ (* pi 1.5) (angle point (list (+(car point)(car deriv))(+(cadr point)(cadr deriv))(+(caddr point)(caddr deriv))))))
    (setq valeur (cond
                  ((< i 10)(strcat "-0+00" (rtos i 2 0)))
                  ((> i 100)(strcat "-0+" (rtos i 2 0)))
                  (t (strcat "-0+0" (rtos i 2 0)))
                  ))
    (setq point (list (car point) (cadr point) 0))
    (setq mark (entmakex
             (list (cons 0 "TEXT")
                   (cons 10 point)
                   (cons 11 point)
                   (cons 40 (/ 1.6 (getvar 'cannoscalevalue)))
                   (cons 1 valeur)
                   (cons 8 "E-TXT-COTE")
                   (cons 7 "TX1")
                   (cons 72 0)
                   (cons 73 2)
                   (cons 50 tang)
               )))
    (setq marklst (cons mark marklst))
    (setq i (+ i etape))
  )
marklst
)

;/
;deltaz ; Calcul la difference d'altitude entre l'élévation max et l'élévation min d'une polyligne 3D
;@params: vl-poly (vl-object) ; Polyligne 3D
;return: (lst) ; Liste contenant le delta d'altitude et l'altitude min de la polyligne
;/
(defun deltaz (vl-poly / longlst i )
(setq longlst (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates vl-poly))))
(setq i 2
      minval 1000
      maxval 0)
(while (< i (length longlst))
  (cond
  ((> (nth i longlst) maxval) (setq maxval (nth i longlst)))
  ((< (nth i longlst) minval) (setq minval (nth i longlst)))
  )
  (setq i (+ i 3)) 
)
(list (- maxval minval) minval)
)

;/
;drawcurve ; Calcule les coordonées de la polyligne utiliser pour le profil
;@params: flatpoly (vl-object) ; Polyligne 2D
;         firstpoly (vl-object) ; Polyligne 3D
;         startz (real) ; Altitude de référence pour le profil
;         startpoint (lst) ; Point 2D inférieur gauche du profil
;return: (lst) ; Liste de coordonées des sommets de la polyligne de profil
;/
(defun drawcurve (flatpoly firstpoly startz startpoint / i vertices dist-lst z-lst )
  (setq i 2
        z-lst ()
        vertices (length (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates firstpoly)))))
  (while (< i vertices)
    (setq z-lst (cons (- (nth i (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates firstpoly)))) startz) z-lst))
    (setq i (+ 3 i))
  )
  (setq z-lst (reverse z-lst))
  (setq vertices (/ (length (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates flatpoly))))2))
  (setq i 0
        dist-lst ())
  (while (< i vertices)
    (setq dist-lst (cons (vlax-curve-getDistAtParam flatpoly i) dist-lst))
    (setq i (1+ i))
  )
  (setq dist-lst (reverse dist-lst))
  (setq i 0
        newpoly ())
  (while (< i vertices)
    (setq newpoly (cons (list (+(car startpoint)(nth i dist-lst)) (+(cadr startpoint)(nth i z-lst))) newpoly))
    (setq i (1+ i))
  )
  (setq newpoly (reverse newpoly))
)



(defun c:gridprofil ( / s firstpoly flatpoly longueur int point pointup crosspoint deriv i cadre delta startpoint col lin startz newcoord curvepoly ltype ent)
  (defun *error* (msg)
    (setvar 'osmode OLD_OSM)
    (print msg)
  )
  (setq OLD_OSM(getvar 'osmode))
  (setvar 'cmdecho 0)
  (setvar 'osmode 0)
  (setq s (ssadd))
  (while (not ent)
    (setq ent (car(entsel "Séléctionnez une polyligne 3D:"))))
  (setq firstpoly (vlax-ename->vla-object ent))
  (if (not(=(vla-get-EntityName firstpoly)"AcDb3dPolyline"))
    (progn
      (alert "C'est pas une polyligne 3D ça")
      (exit)
    )
  )
;Récupère le delta d'altitude
  (setq delta (+ 6 (atof (rtos (car (deltaz firstpoly)) 2 0))))
  (setq flatpoly (vla-copy firstpoly))
  (setq s (ssadd (vlax-vla-object->ename flatpoly) s))
;Aplani la copie de la polyligne 3D
  (flatline s)                                                    
  (setq flatpoly (vlax-ename->vla-object (entlast)))
  (setq longueur (vla-get-length flatpoly))
  (setq loop 1)
  (setq etape 5)
  (setq marklst (markmaker flatpoly etape longueur))
  (princ "\n Utilisez +/- pour gerer l'espace des ségments")
;Boucle pour le marquage dynamique
  (while (and (setq gr (grread t 12 0)) loop)                     
    (cond
        ((= (cadr gr) 43)   ;Touche +
          (mapcar 'entdel marklst)
          (setq etape (+ etape 5))
    (setq marklst (markmaker flatpoly etape longueur))
        )
        ((= (cadr gr) 45)   ;Touche -
          (mapcar 'entdel marklst)
          (if (> etape 5)
          (setq etape (- etape 5)))
    (setq marklst (markmaker flatpoly etape longueur))
        )
        ((= (cadr gr)13)    ;Touche entrée
          (setq loop nil)
        )
        ((= (cadr gr)32)    ;Barre espace
          (setq loop nil)
        )
    )
  )
  (vla-put-Elevation flatpoly 0)
  (initget "Oui Non")
  (if (= "Oui" (cond ((getkword "Voulez vous créer une grille de profile ? [Oui/Non]: <Non>")) ("Non")))
  (progn
  (setq loop t)
;Boucle pour placer le cadre temporaire pour la coupe
  (while (and (setq gr (grread t 12 0)) loop)                     
    (cond
        ((= (car gr) 5)
            (and cadre (entdel cadre) (setq cadre nil))
            (setq tmp1 (cadr gr)
                  tmp2 (list (caadr gr) (+ delta (cadadr gr)))
                  tmp3 (list (+ (+ longueur 10) (caadr gr)) (+ delta (cadadr gr)))
                  tmp4 (list (+ (+ longueur 10) (caadr gr)) (cadadr gr)))
            (setq cadre (entmakex
             (list (cons 0 "LWPOLYLINE")
                   (cons 10 tmp1)
                   (cons 10 tmp2)
                  (cons 10 tmp3)
                    (cons 10 tmp4)
                   (cons 8 "E-TXT-COTE")
                  (cons 90 4)
                  (cons 70 1)
               )))
        )
        ((= (car gr) 3)
          (setq loop nil)
        )
    )
  )
  (entdel cadre)
  (setq startpoint (cadr gr)
        i 0
        calqueGrille "PAP-GRILLE")
  (while (< i (+ longueur 5))
;Change le type de ligne
    (if (= (rem i etape) 0)       
      (setq couleur 34
            ltype "Continuous")
      (setq couleur 8
            ltype "PONCEAU")
    )
;Place les colonnes de la grille
    (setq col (entmakex                                                                 
             (list (cons 0 "LINE")
                   (cons 10 (list (+ i (car startpoint)) (+ delta (cadr startpoint))))
                   (cons 11 (list (+ i (car startpoint)) (- (cadr startpoint) 1)))
                   (cons 8 calqueGrille)
                  (cons 62 couleur)
                  (cons 6 ltype)
               )))
    (if (= (rem i etape) 0)
      (progn 
;Formatage du texte pour les marques
          (setq valeur (cond                                  
                  ((< i 10)(strcat "0+00" (rtos i 2 0)" "))
                  ((> i 99)(strcat "0+" (rtos i 2 0)" "))
                  (t (strcat "0+0" (rtos i 2 0)" "))
                  ))
            (setq mark (entmakex                              ;Ecris les marques
             (list (cons 0 "TEXT")
                   (cons 10 (list (+ i (car startpoint)) (- (cadr startpoint) 1)))
                   (cons 11 (list (+ i (car startpoint)) (- (cadr startpoint) 1)))
                   (cons 40 (/ 1.6 (getvar 'cannoscalevalue)))
                   (cons 1 valeur)
                   (cons 8 calqueGrille)
                   (cons 7 "TX1")
                   (cons 72 2)
                   (cons 73 2)
                   (cons 62 8)
                   (cons 50 (/ pi 2))
               )))
        )
      nil
    )
    (setq i (+ 5 i)) 
  )
(setq i 0)
(setq startz (- (floor (atof (rtos (cadr (deltaz firstpoly)) 2 2))) 3))
(while (< i delta)
;Change le type de ligne
(if (= (rem (+ i startz) 5) 0)        
  (setq couleur 34
        ltype "Continuous")
  (setq couleur 8
        ltype "PONCEAU")
)
;Place les lignes de la grille
    (setq lin (entmakex                 
             (list (cons 0 "LINE")
                   (cons 10 (list (- (car startpoint) 2) (+ i (cadr startpoint))))
                   (cons 11 (list (+ (+ longueur 5) (car startpoint)) (+ i (cadr startpoint))))
                   (cons 8 calqueGrille)
                    (cons 62 couleur)
                    (cons 6 ltype)
               )))

          (setq valeur (strcat (rtos (+ i startz)2 1)" "))
            (setq mark (entmakex                            ;Ecris les marques
             (list (cons 0 "TEXT")
                   (cons 10 (list (- (car startpoint) 2) (+ i (cadr startpoint))))
                   (cons 11 (list (- (car startpoint) 2) (+ i (cadr startpoint))))
                   (cons 40 (/ 1.6 (getvar 'cannoscalevalue)))
                   (cons 1 valeur)
                   (cons 8 calqueGrille)
                   (cons 7 "TX1")
                   (cons 72 2)
                   (cons 73 2)
                   (cons 62 8)
                   (cons 50 0)
               )))
    (setq i (1+ i))
)
(setq newcoord (drawcurve flatpoly firstpoly startz startpoint))
;Place la coupe
 (setq curvepoly (entmakex (append (list (cons 0 "LWPOLYLINE")            
                         (cons 100 "AcDbEntity")
                         (cons 100 "AcDbPolyline")
                          (cons 8 calqueGrille)
                         (cons 90 (length newcoord))
                         (cons 70 0))
                   (mapcar (function (lambda (p) (cons 10 p))) newcoord))))
  )
  nil
  )

(setvar 'osmode OLD_OSM)
(princ)
)
(c:gridprofil)


