; ****************************************************************************
; *                                TOLCHK.lsp                                *
; *      Vérifie et inscrit les tolérence graphique lors de l'extraction     *
; *                    Version développé pour Bricscad v23                   *
; *                             Mickael FRANCOIS                             *
; *                                2023-05-24                                *
; ****************************************************************************

(defun c:tolchk( / s-mesures s-noseg s-str s-arc str vl-str lst-lines line vl-line mid len id data pos i arc)
  (vl-load-com)
    ;;;Error handler personalisé pour rétablire les variables
  (defun *error* (msg)
      (if oldecho (setvar 'cmdecho oldecho))
      (if OSM (setvar 'osmode OSM))
      (if OLAY (setvar 'clayer OLAY))
      (if CSTYL (setvar 'textstyle CSTYL))
      (princ)
      )
  (setq OSM (getvar 'osmode))
  (setvar 'cmdecho 0)
  (setvar 'osmode 0)
  ;Récupération des infos
  (setq s-lines (ssget "_x" '((0 . "LINE")))
        s-arc (ssget "_x" '((0 . "ARC")))
        s-redt (ssget "_x" '((-4 . "<AND")
                               (0 . "TEXT")
                               (-4 . "<OR")
                                 (8 . "CON_INF_STR")
                                 (8 . "STR_NO_SEG")
                                 (8 . "STR_LO_LIG")
                               (-4 . "OR>")
                             (-4 . "AND>")))
        s-mesures (ssget "_x" '((-4 . "<AND")
                                (0 . "TEXT")
                                (-4 . "<OR")
                                (8 . "CON_INF_GEN")
                                (8 . "OFF_LO_LIG")
                                (-4 . "OR>")
                                (-4 . "AND>"))))
  ;Création des variable
  (setq s-noseg (ssadd)
        s-str (ssadd)
        s-mesures (MF:delsup s-mesures))
  (MF:splitss s-redt)
  (setq s-str (MF:remove_double s-str))
  (setq s-noseg (MF:remove_double s-noseg))
  (setq lst-lines '())
  (setq lst-str '())
  ;(MF:printss s-mesures)
  ;Création de la liste des textes
  (if s-str
  (while (setq str (ssname s-str 0))
         (setq vl-str (vlax-ename->vla-object str))
         (setq pos (strcat
                      (rtos(car (vlax-get vl-str 'InsertionPoint)))
                      ","
                      (rtos(nth 1 (vlax-get vl-str 'InsertionPoint)))))
         (setq lentxt (MF:findlentxt vl-str s-mesures))
         (setq idtxt (MF:findidtxt (vlax-get vl-str 'TextString)))
         (setq datastr (list pos lentxt idtxt))
         (if (and (not(MF:is-in-list datastr lst-str)) lentxt)
            (setq lst-str (cons datastr lst-str))
         )
         (ssdel str s-str)
  )nil)
 ; (princl lst-str)
  ;Création de liste des lignes
  (if s-lines
  (while (setq line (ssname s-lines 0))
         (setq vl-line (vlax-ename->vla-object line))
         (setq mid (strcat (rtos (/ (+ (car (vlax-get vl-line 'StartPoint)) (car (vlax-get vl-line 'EndPoint))) 2) 2 1)
                           ","
                           (rtos (/ (+ (nth 1 (vlax-get vl-line 'StartPoint)) (nth 1 (vlax-get vl-line 'EndPoint))) 2) 2 1)))
         (setq len (LM:roundto (vlax-get vl-line 'Length) 4))
         (setq id (MF:findid mid s-noseg))
         (setq data (list mid len id))
         (if (not(MF:is-in-list data lst-lines))
             (setq lst-lines (cons data lst-lines))
         )
         (ssdel line s-lines)
  )nil)
  ;Ajout a la liste les arc de cercle
  (if s-arc
  (while (setq arc (ssname s-arc 0))
         (setq v-arc (vlax-ename->vla-object arc))
         (setq mid (vlax-curve-getPointAtDist v-arc (/ (vlax-curve-getDistAtPoint v-arc (vlax-curve-getEndPoint v-arc)) 2)))
         (setq mid (strcat (rtos (car mid) 2 1) "," (rtos (nth 1 mid) 2 1)))
         (setq len (LM:roundto (vlax-curve-getDistAtPoint arc (vlax-curve-getEndPoint arc)) 4))
         (setq id (MF:findid mid s-noseg))
         (setq data (list mid len id))
         (if (not(MF:is-in-list data lst-lines))
             (setq lst-lines (cons data lst-lines))
         )
         (ssdel arc s-arc)
  )nil)
 ; (princl lst-lines)
  (setvar 'clayer "E-FON-COMMENTAIRES")
  (setvar 'TEXTSTYLE "TX1")
  (while (setq newline (car lst-str))
   (setq realsize (MF:realsize (nth 2 newline) lst-lines))
       (if (> (strlen realsize) 6)
        (setq realsize (strcat (substr realsize 1 (- (strlen realsize) 6)) " " (substr realsize (- (strlen realsize) 5))))nil)
  (print realsize)
   (setq realtxt (strcat "Tol\U+00E9rance graphique:\n" realsize))
   (setvar 'cecolor (MF:findcolor realsize (nth 1 newline)))
         (if (not(= realsize (nth 1 newline)))
             (if (= realsize "0,00")
          (vl-cmdf "_text" (car newline) "" "V\U+00E9rifiez")
          (vl-cmdf "_mtext" (car newline) "j" "bg" "" realtxt ""))
          nil
         )
         (setq lst-str (cdr lst-str))
  )
  (if OSM (setvar 'osmode OSM))
  (princ)
  ;(princl lst-lines)
)

;---------------------------------------------------------------
;Renvoie le code couleur en fonction de la diférence de longueur
(defun MF:findcolor(nb1 nb2 / res ret)
  (setq res (- (atof (vl-string-subst "." "," nb1)) (atof (vl-string-subst "." "," nb2))))
  (if(< res 0)
      (setq res (* res -1))nil)
  (if (< res (* (atof (vl-string-subst "." "," nb2)) 0.002))
      (setq ret "3")
      (setq ret "1"))
)

;---------------------------------------
;Renvoie la valeur graphique d'une ligne
(defun MF:realsize(l lst / i y n m ret)
  (setq ret 0
        i 0
        y 0
        n (length l)
        m (length lst))
  (while (< i n)
         (while (< y m)
                (if (= (nth i l) (nth 2 (nth y lst)))
                    (setq ret (+ ret (nth 1 (nth y lst))))
                    nil
                )
                (setq y (+ y 1))
          )
          (setq i (+ i 1)
                y 0)
  )
  (setq ret (vl-string-subst "," "." (rtos ret 2 2)))
)

;----------------------------------------
;Renvoie une liste des numero de segments
(defun MF:findidtxt(str / lst-ret)
  (if (> (vl-string-position (ascii ";") str) 0)
  (setq str (substr str (+ (vl-string-position (ascii ";") str) 2)))
  (setq str (substr str 2)))
  (setq lst-ret (LM:strtolst str ","))
)

;-------------------------------------------------------
;Renvoi la TextString qui correspond a la longueur texte
(defun MF:findlentxt( vl-obj sset / ret i n elem)
  (setq ret nil
        i 0
        n (sslength sset)
        elem (ssname sset i))
  (while (and (< i n) (= ret nil))
        (setq elem (ssname sset i))
        (setq vl-elem (vlax-ename->vla-object elem))
        (if (equal (vlax-get vl-elem 'InsertionPoint) (vlax-get vl-obj 'InsertionPoint))
            (setq ret (vlax-get vl-elem 'TextString)))
        (setq i (+ 1 i))
  )
  (setq ret ret)
)

;------------------------------------------------ 
;Vérifie si un element existe deja dans une liste
(defun MF:is-in-list (raw liste / ret i n)
  (setq ret nil
        i 0
        n (length liste))
  (while (< i n)
         (if (equal raw (nth i liste))
             (progn
               (setq ret t
                     i n))
         (setq i (+ i 1))
         )
         (setq ret ret)
  )
)

;------------------------------------------------------
;Renvoi la valeur du numero de segment lié  au midpoint
(defun MF:findid(ins s-num / ret vl-num i n num insp)
  (setq ret nil
        i 0
        n (sslength s-num)
        num (ssname s-num i))
  (while (and (< i n) (= ret nil))
         (setq num (ssname s-num i))
         (setq vl-num (vlax-ename->vla-object num))
         (setq insp (strcat
                      (rtos(car (vlax-get vl-num 'InsertionPoint)) 2 1)
                      ","
                      (rtos(nth 1 (vlax-get vl-num 'InsertionPoint)) 2 1)))
       ;  (print (strcat "ins = " ins))
       ;  (print (strcat "insp = " insp))
         (if (= ins insp)
             (setq ret (vlax-get vl-num 'TextString))
             nil
          )
         (setq i (+ 1 i))
  )
  (setq ret ret)
)

;-------------------------------------------------------------------
;Sépare un jeu de selection pour le repartir dans deux nouveaux jeux
(defun MF:splitss(sset / elem vl-elem)
  (while (setq elem (ssname sset 0))
         (setq vl-elem (vlax-ename->vla-object elem))
         (if (vl-string-position (ascii ";") (vlax-get vl-elem 'TextString))
             (ssadd elem s-str)
             (ssadd elem s-noseg)
          )
         (ssdel elem sset)
  )
)

;----------------------------------------------
;Supprime les doublons dans un jeu de selection
(defun MF:remove_double(ss / i y n name name2)
  (setq i 0
        y 0
        n (sslength ss))
  (while (< i n)
    (setq name (ssname ss i)
          y (+ i 1))
         (while (< y n)
                (if (= (setq name2 (ssname ss y)) name)
                    (progn
                      (ssdel name2 ss)
                      (setq n (sslength ss))
                    )
                    (setq y (+ 1 y))
                )
         )
         (setq i (+ 1 i)
               n (sslength ss))
  )
  (setq ss ss)
)

;-----------------------------------------------------------------------
;Supprime de la selection les elements qui ont "S" ou "R" dans la valeur
(defun MF:delsup(ss / i n ename vl-name)
  (setq i 0
        n (sslength ss))
  (while (< i n)
         (setq ename (ssname ss i)
               vl-ename (vlax-ename->vla-object ename))
         (if (or
               (vl-string-position (ascii "S") (vlax-get vl-ename 'TextString))
               (vl-string-position (ascii "R") (vlax-get vl-ename 'TextString))
              )
             (ssdel ename ss)
             (setq i (+ 1 i))
          )
         (setq n (sslength ss)) 
  )
  (setq ss ss)
)

;-----------------------------------------------------------------
;Imprime la variable texte de chaque element d'un jeu de selection
(defun MF:printss(ss / ssw ssv)
  (while (setq ssw (ssname ss 0))
         (setq ssv (vlax-ename->vla-object ssw))
         (print (vlax-get ssv 'TextString))
         (ssdel ssw ss)
  )
)


;------------------------------------------------------------
;Lisp de Lee Mac pour arondire les real
;------------------------------------------------------------
(defun LM:roundto ( n p )
    (LM:roundm n (expt 10.0 (- p)))
)
(defun LM:roundm ( n m )
    (* m (atoi (rtos (/ n (float m)) 2 0)))
)

;; String to List  -  Lee Mac
;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings

(defun LM:strtolst ( str del / len lst pos )
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
)


    ;; Print List  -  Lee Mac
    ;; Prints a supplied list to the command-line or to a given filename,
    ;; with nested lists displayed in a hierarchical format.
    ;; l - [lst] List to print
    ;; f - [str] Optional filename
     
    (defun LM:princl ( l f / _print _princ d r )
        
        (defun _print ( l i )
            (if (and (= 'list (type l)) (vl-list-length l) (vl-some 'vl-consp l))
                (progn
                    (_princ (strcat "\n" i "("))
                    (foreach x l (_print x (strcat i "    ")))
                    (_princ (strcat "\n" i ")"))
                )
                (_princ (strcat "\n" i (vl-prin1-to-string l)))
            )
        )
     
        (eval
            (list 'defun '_princ '( x )
                (if (and (= 'str (type f)) (setq d (open f "w")))
                    (list 'princ 'x d)
                   '(princ x)
                )
            )
        )
     
        (setq r (vl-catch-all-apply '_print (list l "")))
        (if (= 'file (type d))
            (progn
                (setq d (close d))
                (startapp "notepad" f)
            )
        )
        (if (vl-catch-all-error-p r)
            (prompt (vl-catch-all-error-message r))
            l
        )
    )
     
    (defun princl ( l ) (LM:princl l nil) (princ))
    (defun princf ( l ) (LM:princl l (vl-filename-mktemp "list" (getvar 'dwgprefix) ".txt")) (princ))
    (princ)