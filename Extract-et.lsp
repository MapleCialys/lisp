; ****************************************************************************
; *                              EXTRACT-ET.lsp                              *
; *               Importe les objets de l'extraction cadastrale              *
; *                       Configuré pour Bricscad 2023                       *
; *                             Mickael FRANCOIS                             *
; *                                2023-04-19                                *
; ****************************************************************************

(defun c:Extract-et(/ GO FILE1 FILE2 FILE4 FILE5 FILE6 FILE7 file8 file9 FILE-LIST FILE-LIST1 DATA NBRE1 NBRE2 RAP)

(setq COUT-UNITAIRE 0.212) ;0 à 999 lots
(setq COUT-UNITAIRE1 0.03) ;1000 lots et plus  
(setq COUT-BASE     7.05)
(setq etfilepath "C:/bricsEcceTerra/Extraction/")

(setvar "INSUNITS" 0)
(SETVAR "CMDECHO" 0)

(setq FILE-LIST1 (vl-directory-files  etfilepath nil 1))
(print File-LIST1)

(foreach FILE4 FILE-LIST1
     (if (or (= "O" (substr (vl-filename-extension FILE4) 2 1))
	    (= "C" (substr (vl-filename-extension FILE4) 2 1))
	    (= "o" (substr (vl-filename-extension FILE4) 2 1))
	    (= "c" (substr (vl-filename-extension FILE4) 2 1)))
       (progn
	 (setq FILE5 (findfile FILE4))
	 (setq FILE6 (strcat etfilepath
			     (substr (vl-filename-extension FILE4) 2 2)
			     ".dxf"))
	 (if (not (vl-file-copy FILE5 FILE6))
	 	(alert (strcat "copie  " file5 "   ->   " file6 "     non completee")))
	 (command "._insert" FILE6 "0,0,0" "1" "" "0" )
	 (command "._explode" (entlast))
	 	 );fin progn
       );fin if
  );fin foreach
  (command "._regen" )
  (command "._zoom" "_e")

(foreach FILE4 FILE-LIST1
(if (or (= ".rap" (vl-filename-extension FILE4))
	(= ".RAP" (vl-filename-extension FILE4)))
	(progn
          (setq FILE7 (open (findfile FILE4) "r"))
          (setq DATA (read-line FILE7))
          (while DATA
           (cond((= (substr DATA 1 40) "Le nombre de lots officiels géométriques")
                    (setq NBRE1 (atoi (substr DATA 57 5)))
                    )
                ((= (substr DATA 1 40) "Le nombre de lots de contexte extraits e")
                    (setq NBRE2 (atoi (substr DATA 46 5)))
                    )
            );fin cond        
          (setq DATA (read-line FILE7))
          );fin while
	  (CLOSE FILE7) 
      );fin progn
);fin if
);fin foreach
(princ)
(cond
  ((> 999 (+ NBRE1 NBRE2))
   (setq COUT(strcat
	     "Nombre de lots extrait: "
	     (itoa (+ NBRE1 NBRE2))
	     " - "
	     (rtos  (+ COUT-BASE (* (+ NBRE1 NBRe2) COUT-UNITAIRE))2 2)
	     "$"
	     ))
   )

  ((< 999 (+ NBRE1 NBRE2))
   (setq COUT(strcat
	     "Nombre de lots extrait: "
	     (itoa (+ NBRE1 NBRE2))
	     " - "
	     (rtos  (+ COUT-BASE 199.80 (* (- (+ NBRE1 NBRE2) 999.0) COUT-UNITAIRE1))2 2)
	     "$"
	     ))
   )
  )
(print)
(princ (strcat "File: "(vl-filename-base file3)))
(print COUT)
(princ)
)