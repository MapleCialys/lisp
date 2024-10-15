(defun c:blockdt ( / old-ec obj i obj-vl obj-lst x y radius letter ec-obj listing mille)
  ;Créer un bloc annotatif a partir des bulles de descriptions.
    ;;;Error handler personalisé pour rétablire les variables
  (defun *error* (msg)
      (if oldecho (setvar 'cmdecho oldecho))
      (if OSM (setvar 'osmode OSM))
      (if CLAY (setvar 'clayer CLAY))
      (princ)
  )
  	;Enregistrement des variables nécessaire.
 	(setq OSM (getvar 'osmode)
        CLAY (getvar 'clayer)
        )
  	(setvar 'cmdecho 0)
  	(setq scalelist (list "1:50" "1:100" "1:150" "1:200" "1:250" "1:300" "1:400"
                        "1:500" "1:600" "1:750" "1:1000" "1:1250" "1:1500"
                        "1:2000" "1:2500" "1:5000"))
   	(setq old-ec (getvar 'cannoscale))
   	;Chargement du Lisp str->lst
	(load "strtolst")
 	;Récupération des éléments et vérif.
  	(if (not (setq obj (ssget)))
       	(progn
          (alert "La y'as rien de sélectionné, fait un effort.")
          (quit)))
   	(if (not (= 3 (sslength obj)))
        (progn
          (alert "Sélectionnez uniquement 3 éléments. La lettre, le cercle et le spline.")
          (quit)))
  	(setvar 'cannoscale "1:1000")
  	(setq i 0)
   	;boucle de récupération des données utiles
  	(while (< i 3)
       (setq obj-vl (vlax-ename->vla-object (ssname obj i)))
       (if (= (vla-get-EntityName obj-vl) "AcDbSpline")
           (progn
           	(setq obj-lst (vlax-safearray->list (vlax-variant-value (vla-get-ControlPoints obj-vl))))
           	(setq y (nth (- (length obj-lst) 2) obj-lst))
   		   	(setq x (nth (- (length obj-lst) 3) obj-lst))
           )
       )
       (if (= (vla-get-EntityName obj-vl) "AcDbCircle")
			(setq radius (vla-get-Radius obj-vl))
  		)
       (if (= (vla-get-EntityName obj-vl) "AcDbText")
	   		(progn
				(setq letter (vla-get-TextString obj-vl))
				(if (tblsearch "block" letter)
					(progn
						(alert "Un bloc avec ce nom existe déjà. Purger le dessin de ce bloc avant de continuer.")
						(setvar 'cannoscale old-ec)
						(quit)
					)
				)
			)
  		)
       (setq i (+ i 1))
   )
   	;Vérif des données récupérées
  	(if (not y)
        (progn
          (alert "Pas de spline sélectionnée.")
  		  (setvar 'cannoscale old-ec)
          (quit)))
   	(if (not radius)
        (progn
          (alert "Pas de cercle sélectionné.")
  		  (setvar 'cannoscale old-ec)
          (quit)))
   	(if (not letter)
        (progn
          (alert "Pas de lettre sélectionnée.")
  		  (setvar 'cannoscale old-ec)
          (quit)))
    ;Modification des éléments a l'écran
   	(setq ec-obj (/ 1 (/ radius 2)))
   	(vl-cmdf "echelle" obj "" (list x y) ec-obj)
   	(vl-cmdf "-bloc" letter "A" "" ""(list x y) obj "")
   	(vl-cmdf "-inserer" letter (list x y) "" "")
   	(setq blk (entlast))
    ;Appel du DCL
   	(setq ec-select (load_dialog "C:\\BricsEcceterra\\LISP\\DCL\\blockdt.dcl"))
   	(new_dialog "listec" ec-select)
   	(set_tile "toptxt1" "Sélectionnez  les échelles")
   	(set_tile "toptxt2" "d'annotation voulues.")
   	(set_tile "toptxt3" "(Maintenir Ctrl pour multiple)")
   	(action_tile "list1" "(list-values)")
   	(action_tile "accept" "(done_dialog)")
   	(start_dialog)
   	(unload_dialog ec-select)
    ;Fin DCL
   	(setvar 'cannoscale old-ec)
   	(setq listing (LM:str->lst listing " "))
   	(foreach n listing (progn
                         (vl-cmdf "_-objectscale" blk "" "a" (nth (atoi n) scalelist) ""))
             			 (if (= (atoi n) 10) (setq mille t))
             			)
    (if (not mille)
        (vl-cmdf "_-objectscale" blk "" "s" "1:1000" "")
    )
   	 (princ)
)

 (defun list-values (/)
 (setq listing $value)
 )

(c:blockdt)