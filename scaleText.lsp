; *
; MF:scaleText: highligth les textes qui ne sont pas a l'échelle
; @params: sset-txt (SelectionSet) Jeux de sélection contenant des textes
; Requis pour drcheck.lsp
; *
(defun MF:scaleText(sset-txt / tx3 tx2 tx1 lot ec ret stl)
	(setq 	tx3 3
       		tx2 2.25
         	lot 2.8222
         	tx1 1.6
          	ec (/ 1 (getvar "cannoscalevalue"))
           	ret f)
  (while (setq ename (ssname sset-txt 0))
         (setq vl-ename (vlax-ename->vla-object ename))
         (setq stl (cond 
                     	((= (vla-get-StyleName vl-ename) "TX1") tx1)
                     	((= (vla-get-StyleName vl-ename) "TX2") tx2)
                     	((= (vla-get-StyleName vl-ename) "TX3") tx3)
                     	((= (vla-get-StyleName vl-ename) "LOT") lot)
                     	(t 0)
                      	))
         (if (not(=(vla-get-Height vl-ename)(* ec stl)))
             (progn
               (setq ret t)
               (redraw ename 3)
             )
             nil
         )
         (ssdel ename sset-txt)
  )
  ret
)