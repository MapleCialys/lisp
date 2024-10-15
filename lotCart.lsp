; ****************************************************************************
; *                                LOTCART.lsp                               *
; *         Inscrit les numéro de lot sélectionnés dans le cartouche         *
; *                        développé pour Bricscad v23                       *
; *                             Mickael FRANCOIS                             *
; *                                2023-12-13                                *
; ****************************************************************************

(defun c:lotcart( / i activTab txtbrute current lots len str1 str2 cart loop gr)
    ;;;Error handler personalisé pour rétablire les variables
  (defun *error* (msg)
      (if oldecho (setvar 'cmdecho oldecho))
      (if OSM (setvar 'osmode OSM))
      (if CLAY (setvar 'clayer CLAY))
      (princ)
  )
  (setq OSM (getvar 'osmode)
        CLAY (getvar 'clayer)
        )
  (setvar 'osmode 0)
  (setvar 'cmdecho 0)
  (load "isInList")
  (setq i 0)
  (setq activTab (vla-get-ActiveLayout (vla-get-activedocument (vlax-get-acad-object))))
  (if (= (vla-get-name activTab) "Model")
    (alert "Cette fonction nécessite d'être sur un onglet de l'espace papier.")
    (progn
        ;check l'effectivename
      (setq cart (MF:findCart)) ;Ne permet plus de trouver le cartouche a chaque fois.
      ;(setq cart (vlax-ename->vla-object (car (entsel "Sélectionnez le cartouche cible:")))); plus besoin en passant par les objets vl
      (if (not cart)
        (alert "Aucun cartouche trouvé.")
        (progn
          (setvar 'ctab "Model")
          (princ "\nSélectionnez les numéros de lots:")
          (setvar 'nomutt 1)
          (setq txtbrute (ssget '((0 . "TEXT"))))
          (setvar 'nomutt 0)
          (while (setq current (ssname txtbrute i))
            (setq current (vlax-ename->vla-object current))
            (if (AND (MF:regexLot (vla-get-TextString current))(not (MF:is-in-list (vla-get-TextString current) lots)))
              (setq lots (cons (vla-get-TextString current) lots))
            )
            (setq i (+ i 1))
          )
          (setq lots (vl-sort lots '<))
          (setq len (length lots))
          (setvar 'ctab (vla-get-name activTab))
          (cond
            ((<= len 6)
                (setq str1 (LM:lst->str lots ", "))
                (LM:vl-setattributevalue cart "LOT2" "")
                (LM:vl-setattributevalue cart "LOT1" str1)
            )
            ((<= len 12)
                (setq str1 (LM:lst->str (MF:subseq lots '(0 5)) ", "))
                (setq str2 (LM:lst->str (MF:subseq lots '(6)) ", "))
                (LM:vl-setattributevalue cart "LOT1" str1)
                (LM:vl-setattributevalue cart "LOT2" str2)
            )
            ((> len 12)(MF:putTableLots (LM:lst->str lots ", ")))
          )
        )
      )
    )
  )
  (setvar 'osmode OSM)
  (princ)
)

(defun MF:putTableLots (str / gr loop entt frame topFrame botFrame)
  (setq loop t)
  (while (and (setq gr (grread t 12 0)) loop)
    (cond
      ((= (car gr) 5)
       (and entt (entdel entt) (setq entt nil))
       (setq entt (entmakex
                    (list (cons 0 "MTEXT")
                          (cons 10 (cadr gr))
                          (cons 40 2.5)
                          (cons 41 47.0)
                          (cons 1 str)
                          (cons 8 "PAP-TEXTE")
                          (cons 7 "NORMAL2")
                          (cons 72 1)
                          (cons 73 1)
                          (cons 50 0.0)
                      ))
             frame (cadr gr))
      )
      ((= (car gr) 3)
      (setq loop nil))
    )
  )
  (setq topFrame (list (- (car frame) 5.0) (+ 5.0 (cadr frame)) 0.0)
        botFrame (list (+ 55.0 (car topFrame)) (- (cadr topFrame) (+ (cdr (assoc 43 (entget entt))) 10.0)) 0.0))
  (print entt)
  (vl-cmdf "_RECTANG" "R" "1" topFrame botFrame)     
)

(defun MF:findCart( / blklst s e ret)
"Renvoie l'objet vl du bloc cartouche"
    (if (setq s (ssget "X" (list '(0 . "INSERT") (cons 410 (getvar "ctab")))))
        (while (setq e (ssname s 0))
                (if (vl-string-search "Cartouche Ecceterra 2022 - v14" (vla-get-EffectiveName (vlax-ename->vla-object e)))
                   (setq ret (vlax-ename->vla-object e))
                )
               (ssdel e s)
        )
    )
    ret
)

(defun MF:subseq(lst param / start end ret)
    (setq start (car param)
          end (cadr param))
    (if (not end)
      (setq end (- (length lst) 1))
    )
    (while (>= end start)
        (setq ret (cons (nth end lst) ret))
        (setq end (- end 1))
    )
    ret
)

(defun MF:regexLot(str / ret lst)
"Regex pour vérifier le patterne imposé"
  (setq lst (vl-string->list str))
  (if (AND 
        (= (strlen str) 9)
        (AND
          (>= (nth 0 lst) 49)
          (<= (nth 0 lst) 57)
        )
        (= (nth 1 lst) 32)
        (AND
          (>= (nth 2 lst) 48)
          (<= (nth 2 lst) 57)
        )
        (AND
          (>= (nth 3 lst) 48)
          (<= (nth 3 lst) 57)
        )
        (AND
          (>= (nth 4 lst) 48)
          (<= (nth 4 lst) 57)
        )
        (= (nth 5 lst) 32)
        (AND
          (>= (nth 6 lst) 48)
          (<= (nth 6 lst) 57)
        )
        (AND
          (>= (nth 7 lst) 48)
          (<= (nth 7 lst) 57)
        )
        (AND
          (>= (nth 8 lst) 48)
          (<= (nth 8 lst) 57)
        )
      )
      (setq ret t)
      (setq ret nil))
)

; ****************************************************************************
; *                           Lisp de LeeMac utilisé                         *
; ****************************************************************************


;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:vl-setattributevalue ( blk tag val )
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            )
        )
        (vlax-invoke blk 'getattributes)
    )
)

(defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
)
