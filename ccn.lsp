; ****************************************************************************
; *                                  CCN.lsp                                 *
; *                Insert l'élévation sur une courbe de niveau               *
; *                        Développé pour Bricscad v23                       *
; *                             Mickael FRANCOIS                             *
; *                                2023-06-07                                *
; ****************************************************************************

(defun c:ccn( / elev gr loop elev-txt pl)
    ;;;Error handler personalisé pour rétablire les variables
  (defun *error* (msg)
      (and elev (entdel elev) (setq elev nil))
      (if oldecho (setvar 'cmdecho oldecho))
      (if OSM (setvar 'osmode OSM))
      (if OLAY (setvar 'clayer OLAY))
      (if CSTYL (setvar 'textstyle CSTYL))
      (setvar 'nomutt 0)
      (princ)
      )
  (vl-load-com)
  (setq loop t
        oldecho (getvar 'cmdecho)
        OSM (getvar 'osmode))
  (setvar 'cmdecho 0)
  (setvar 'osmode 0)
  (setvar 'wipeoutframe 0)
  (repeat 5 (terpri))
  ;Selection de la courbe
  (princ "\nSélectionner une courbe:\n")
  (setvar 'nomutt 1)
  (load "textmask")
  (while (not pl)
  (setq pl (ssget "_+.:s:e")))
  (setq pl (ssname pl 0))
  (setq vlpl (vlax-ename->vla-object pl))
  ;Ligne pour récuperer l'élevation de la courbe.
  (setq elev-txt (vl-string-subst "," "." (rtos (cdr (assoc '38 (entget pl))) 2 1)))
  ;Boucle pour afficher le texte avant de le placer
  (while (and (setq gr (grread t 12 0)) loop)
    (cond
      ((= (car gr) 5)
       (and elev (entdel elev) (setq elev nil))
       (setq elev (entmakex
                    (list (cons 0 "TEXT")
                          (cons 10 (vlax-curve-getClosestPointTo vlpl (cadr gr)))
                          (cons 11 (vlax-curve-getClosestPointTo vlpl (cadr gr)))
                          (cons 40 (/ 1.6 (getvar 'cannoscalevalue)))
                          (cons 1 elev-txt)
                          (cons 8 "E-TXT-COTE")
                          (cons 7 "TX1")
                          (cons 72 4)
                          (cons 73 0)
                          (cons 50 (+ (/ pi 2) (angle (cadr gr) (vlax-curve-getClosestPointTo vlpl (cadr gr)))))
                      )))
       )
      ((= (car gr) 3)
       (acet-textmask-make-wipeout elev 0.35)
       (setq loop nil)
       )
  ))
  (setvar 'nomutt 0)
  (princ)
  )
(c:ccn)