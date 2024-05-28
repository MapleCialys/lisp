; *
; MF:addcoord: ajoute un nombre aux deux premier elements d'une liste
; @params:  lst (list) Liste de deux real
;           nbr (real) Nombre a ajouter
; #return: ret (list) Liste de deux real
; Obsolète, créer avant de comprendre mapcar
; *
(defun MF:addCoord(lst nbr / ret)
  (setq x (+ (car lst) nbr)
        y (+ (nth 1 lst) nbr)
        ret (list x y))
  ret
  )