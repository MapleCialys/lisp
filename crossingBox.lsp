;Détermine si un object traverse ou est présent a l'endrroit du texte
;MF - Dec 23
;txt - Ename du texte a vérifier
;Return - t si quelque chose est trouvé, nil si rien n'est trouvé
(defun MF:crossingBox (txt / sel lst ts pt_list temp i ent ret wo)
	(load "minboundingbox")
	(setq sel (ssadd))
	(setq i 0)
 	(setq ret nil
         wo nil)
  	(setq lst (LM:minboundingbox (ssadd txt sel) 0.01))
  	(setq ts (vla-get-TextString (vlax-ename->vla-object txt)))
  	(setq pt_list (list (nth 0 lst) (nth 1 lst) (nth 2 lst) (nth 3 lst)))
  	(setq temp (ssget "_CP" pt_list '((-4 . "<NOT") (0 . "HATCH") (-4 . "NOT>"))))
  	(repeat (sslength temp)
  		(setq ent (ssname temp i))
         (if (= "WIPEOUT" (cdr (assoc 0 (entget ent))))(setq wo t))
         (if (= "TEXT" (cdr (assoc 0 (entget ent))))
           	(progn
            (if (= ts (cdr (assoc 1 (entget ent))))
              nil
            (setq ret t))
         )
         (setq ret t))
        (setq i (1+ i))
    )
   (if wo (setq ret nil))
   ret
 )
