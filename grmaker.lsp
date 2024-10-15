; 
; Auteur: Mickael FRANCOIS (mfrancois@ecceterra.com)
; grmaker.lsp (c) 2024
; Desc: Fichier de fonction de création d'entité pour les boucles gr
; Créé:  2024-06-06T18:41:35.114Z
; Modifié: 2024-06-06T18:41:56.592Z
; 

;/
;maker:frame
;Créer un réctangle
;@param {list} coord; List de 4 listes de coordonée pour le réctangle
;@param {string} lay; Calque du réctangle
;@return {entity} L'entité créer par entmakex
;/
(defun maker:frame (coord lay / p)
(entmakex (list
               (cons 0 "LWPOLYLINE")
               (cons 8 "PAP-TEXTE")
               (cons 70 1)
               (cons 8 lay)
               (cons 10 (car coord))
               (cons 10 (cadr coord))
               (cons 10 (caddr coord))
               (cons 10 (cadddr coord))
          )
)
)
;/
;maker:text
;Créer un text
;@param {list} pos; List de coordonée pour la position du texte
;@param {number} ht; Hauteur du texte
;@param {string} stl; Style de texte
;@param {string} lay; Calque du texte
;@param {string} str; Contenue du texte
;@return {entity} L'entité créer par entmakex
;/
(defun maker:text (pos ht stl lay str / )
     (entmakex (list 
                    (cons 0 "TEXT")
                    (cons 10 pos)
                    (cons 40 ht)
                    (cons 7 stl)
                    (cons 8 lay)
                    (cons 1 str)
               )
     )
)

;/
;coord:rectangle
;Calcul les coordonées d'un réctangle
;@param {list} start; List de coordonée pour un des points du réctangle
;@param {number} dx; Longueur sur l'axe X du réctangle
;@param {number} dy; Longueur sur l'axe Y du réctangle
;@return {list} Liste de 4 listes de 2 float
;/
(defun MF:coord:rectangle( start dx dy / )
    (list
      start
      (MF:addCoordDif start dx 0)
      (MF:addCoordDif start dx dy)
      (MF:addCoordDif start 0 dy)
    )
)
