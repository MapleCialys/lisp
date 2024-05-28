;-----------------------------------------------------------------------------------------------------------------------------------;
; Auteur: Sylvain Auger (sauger@ecceterra.com) | Mickael FRANCOIS (mfrancois@ecceterra.com)			                     								;
; ch-ech.lsp (c) 2024																										                                                        		;
; Desc: Change la taille des textes et des blocs suivant l'echelle d'annotation définie. Prend en charge les éléments annotatifs. 	;
; Créé:  Il y as longtemps, dans une galaxy lointaine, très lointaine.						                              										;
; Modifié: 2024-05-07T17:55:07.078Z
;-----------------------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------------------;
; Variable:																														                                                            	;
; SS-SYM:		Jeu de séléction des blocs du dessin.																			                                            	;
; SS-TXT50: 	Jeu de séléction des textes au style TXT50 du dessin.											                                  					;
; SS-TX1:		Jeu de séléction des textes au style TX1 du dessin.													                                    				;
; SS-TX2: 		Jeu de séléction des textes au style TX2 du dessin.														                                  			;
; SS-TX3: 		Jeu de séléction des textes au style TX3 du dessin.														                                  			;
; SS-LOT: 		Jeu de séléction des textes au style LOT du dessin.												                                  					;
; lst: 			Liste des calques touchés par cette fonction														                                        				;
; EN: 			Entité du jeux de séléction actuel																			                                            		;
; OLD-EC: 		Echelle annotative en cours dans le dessin lors du lancement de la fonction.				                      						;
; ec2: 			Valeur de l'echelle d'annotation courante en millieme.													                                  			;
; ec: 			Valeur de l'echelle d'annotation courante en unité.																                                    	;
; compt: 		Compteur utilisé pour les boucles de jeux de sélection.														                                  		;
; mille: 		Marqueur de retenue au cas ou l'entitée annotative possede l'echelle de 1000.					                        					;
; ht-txt: 		Stockage de la valeur de hauteur de texte pour les element non-annotatifs.					                      						;
;-----------------------------------------------------------------------------------------------------------------------------------;

(DEFUN C:ch-ech(/ lst SS-SYM SS-TXT50 SS-TXT SS-TX1 SS-TX2 SS-TX3 SS-LOT EN OLD-EC mille ec ec2 compt ht-txt)
;;Stockage des préférence 
  (setq PST (getvar "PICKSTYLE"))
  (setvar "PICKSTYLE" 0)
  (setvar "cmdecho" 0)
  (command "._ucs" "_w")
  (setq osm (getvar "osmode"))
  (setvar "osmode" 0)
  (load "strtolst")
  (setvar 'nomutt 0)

;;Création de la liste pour traiter uniquement les blocs voulus  
  (setq lst "E-TXT-DESCRIPTIF E-REP-CHEMINEMENT E-REP-IMPLANTATION E-REP-ARPENTAGE E-VEG-ARBRE
  				E-VEG-PLANTATION E-BAT-ACCESSOIRE E-DRA-EGOUT-PLUVIAL E-DRA-EGOUT-PLUVIAL-ELEMENT
				E-DRA-EAU-JOUR E-VOI-PROTECTION E-SER-AQUEDUC E-SER-AQUEDUC-ELEMENT E-SER-EGOUT-SANITAIR-ELEMENT
				E-SER-TEL E-SER-ELEC E-SER-ELEC-TEL E-SER-ELEC-TEL-HAUBAN E-SER-ELEC-TEL-HAUBAN E-SER-ELEC-TEL-LAMP
				E-SER-ELEC-LAMPAD E-SIG-ECLAIRAGE E-REP-CONTROLE-PHOTO E-TOP-COTE-ALTIME")
  
;;Récupération de l'echelle
	(setq ec2 (atof(vl-string-subst "" " "(substr (getvar "cannoscale") 3))))
	(setq ec (/ ec2 1000))
  (setq old-ec (getvar 'cannoscale))
	(setvar 'cannoscale "1:1000")
  
;;Transformation des blocs	
    (setq SS-SYM (ssget "X" '((0 . "INSERT")(410 . "Model"))))													                ;Récupère les blocs.
    (if (not(null SS-SYM))
       	(progn
       		(setq compt 0)
       		(setq EN (ssname SS-SYM COMPT))
       		(while EN
       			(if (/= (vl-string-search (cdr(assoc 8 (entget EN))) lst) nil)									            ;Si notre blocs est sur un calque qui figure dans la liste.
       				(progn
						    (setq mille nil)
						    (if (not(vl-annotative-getscales en))														                        ;Si notre bloc n'est pas annotatif.
						    	(progn
						    		(vla-put-XEffectiveScaleFactor (vlax-ename->vla-object EN) EC)
						    		(vla-put-YEffectiveScaleFactor (vlax-ename->vla-object EN) EC)
						    		(vla-put-ZEffectiveScaleFactor (vlax-ename->vla-object EN) EC)
						    	)
						    	(progn																					                                      ;Si le bloc est annotatif.
						    		(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "))			;Si le bloc a l'échelle de 1000 dans sa liste d'echelle d'annotation.
						    			(setq mille t))
						    		(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
						    		(vla-put-XEffectiveScaleFactor (vlax-ename->vla-object EN) 1)
						    		(if (not mille)
						    			(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
						    		(if (= "1:1" (LM:lst->str (vl-annotative-getscales en) " "))
                        (vl-cmdf "_-objectscale" en "" "a" "1:1000" ""))
						    		(vl-cmdf "_-objectscale" en "" "s" "1:1" "")
						    	)
       			    )
       		    )
				    )
       			(setq compt (1+ compt))
       			(setq EN (ssname SS-SYM COMPT))
       		)
       	)
    )
    (command "_attsync" "_n" "maison")																				                              ;Maison est un bloc avec attribut qui necessite la commande attsync.
        
;;Transformation des textes
;Textes de style "TXT50"
    (setq SS-txt50 (ssget "X" '((0 . "TEXT")(7 . "TXT50")(410 . "Model"))))											            ;Récupère les textes au style TXT50.
    (if (not(null SS-TXT50))
    	(progn
    		(setq compt 0)
    		(setq EN (ssname SS-TXT50 COMPT))
    		(setq ht-txt (* 1.5 ec))
    		(while EN
    			(setq mille nil)
    				(if (not (vl-annotative-getscales en))															                              ;Si le texte n'est pas annotatif.
    					(vla-put-Height (vlax-ename->vla-object EN) ht-txt)
    					(progn																						                                            
    						(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "));Si le texte est annotatif.
    							(setq mille t))
    						(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
                (vla-put-Height (vlax-ename->vla-object EN) 1.5)
    						(if (not mille)
    							(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
    					)
    				)
    				(setq compt (1+ COMPT))
    				(setq EN (ssname SS-txt50 COMPT))
    			)
    		)
    	)            
;Textes de style "TXT"
    (setq SS-txt (ssget "X" '((0 . "TEXT")(7 . "TXT")(410 . "Model"))))											                ;Récupère les textes au style TXT.
    (if (not(null SS-TXT))
    	(progn
    	  	(setq compt 0)
           	(setq EN (ssname SS-TXT COMPT))
    	  	  (setq ht-txt (* 1.3 ec))
              (while EN
    			      (setq mille nil)
    			      (if (not (vl-annotative-getscales en))
    				      (vla-put-Height (vlax-ename->vla-object EN) ht-txt)																			  ;Si le texte n'est pas annotatif.
    				      (progn																						                                                
    				      	(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "));Si le texte est annotatif.
    				      		(setq mille t))
    				      	(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
                    (vla-put-Height (vlax-ename->vla-object EN) 1.3)
    				      	(if (not mille)
    				      		(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
    				      )
    	          )
    	   	      (setq compt (1+ COMPT))
    	   	      (setq EN (ssname SS-txt COMPT))
    		      )
      )
    )          
;Textes de style "TX1"
    (setq SS-tx1 (ssget "X" '((-4 . "<OR")(0 . "TEXT")(0 . "MTEXT")(-4 . "OR>")(7 . "TX1")(410 . "Model"))));Récupère les textes au style TX1.
    (if (not(null SS-TX1))
    	(progn
    		(setq compt 0)
        (setq EN (ssname SS-TX1 COMPT))
    		(setq ht-txt (* 1.6 ec))
        (while EN
    			(setq mille nil)
    			(if (not(vl-annotative-getscales en))																							
    				(vla-put-Height (vlax-ename->vla-object EN) ht-txt)                                            ;Si le texte n'est pas annotatif.
    				(progn																							                                            
    					(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "));Si le texte est annotatif.
    						(setq mille t))
    					(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
              (vla-put-Height (vlax-ename->vla-object EN) 1.6)
    					(if (not mille)
    						(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
    				)
    	   	)
    			(setq compt (1+ COMPT))
    			(setq EN (ssname SS-tx1 COMPT))
        )
      )
    )           
;Textes de style "TX2"
    (setq SS-tx2 (ssget "X" '((0 . "TEXT")(7 . "tx2")(410 . "Model"))))												              ;Récupère les textes au style TX2.
    (if (not(null SS-tx2))
    	(progn
    		(setq compt 0)
        (setq EN (ssname SS-tx2 COMPT))
    		(setq ht-txt (* 2.25 ec))
        (while EN
    			(setq mille nil)
    			(if (not(vl-annotative-getscales en))
    				(vla-put-Height (vlax-ename->vla-object EN) ht-txt)                                             ;Si le texte n'est pas annotatif.
    				(progn																							                                            ;Si le texte est annotatif.
    					(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "))
    						(setq mille t))
    					(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
              (vla-put-Height (vlax-ename->vla-object EN) 2.25)
    					(if (not mille)
    						(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
    				)
          )
          (setq compt (1+ COMPT))
          (setq EN (ssname SS-tx2 COMPT))
        )
      )
    )
;Textes de style "TX3"
    (setq SS-tx3 (ssget "X" '((0 . "TEXT")(7 . "tx3")(410 . "Model"))))												              ;Récupères les textes au style TX3.
    (if (not(null SS-tx3))
    	(progn
    		(setq compt 0)
        (setq EN (ssname SS-tx3 COMPT))
    		(setq ht-txt (* 3 ec))
        (while EN
    			(setq mille nil)
    			(if (not(vl-annotative-getscales en))																							
    				(vla-put-Height (vlax-ename->vla-object EN) ht-txt)                                           ;Si le texte n'est pas annotatif.
    				(progn																							                                          ;Si le texte est annotatif.
    					(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "))
    						(setq mille t))
    					(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
              (vla-put-Height (vlax-ename->vla-object EN) 3)
    					(if (not mille)
    						(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
    				)
          )
          (setq compt (1+ COMPT))
          (setq EN (ssname SS-tx3 COMPT))
        )
      )
    )
;Textes de style "lot"
    (setq SS-lot(ssget "X" '((0 . "TEXT")(7 . "lot")(410 . "Model"))))												            ;Récupère les textes au style LOT.
    (if (not(null SS-lot))
    	(progn
    		(setq compt 0)
        (setq EN (ssname SS-lot COMPT))
    		(setq ht-txt (* 2.8222 ec))
        (while EN
    			(setq mille nil)
    			(if (not(vl-annotative-getscales en))																						
    				(vla-put-Height (vlax-ename->vla-object EN) ht-txt)                                           ;Si le texte n'est pas annotatif
    				(progn																							                                          ;Si le texte est annotatif.
    					(if (vl-string-search "1:1000" (LM:lst->str (vl-annotative-getscales en) " "))
    						(setq mille t))
    					(vl-cmdf "_-objectscale" en "" "a" "1:1000" "")
              (vla-put-Height (vlax-ename->vla-object EN) 2.8222)
    					(if (not mille)
    						(vl-cmdf "_-objectscale" en "" "s" "1:1000" ""))
    				)
          )
          (setq compt (1+ COMPT))
          (setq EN (ssname SS-lot COMPT))
        )
      )
    )            
  (setvar 'cannoscale old-ec)
  (print)
;;Retour des préférences
  (setvar "osmode" osm)
  (setvar "PICKSTYLE" PST)
  (command "._ucs" "_p")
  (setvar 'nomutt 0)
  (PRINC)
)
