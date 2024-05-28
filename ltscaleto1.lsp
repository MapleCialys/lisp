; 
; Auteur: Mickael FRANCOIS (mfrancois@ecceterra.com)
; ltscaleto1.lsp (c) 2024
; Desc: Mets l'echelle du type de ligne à 1 pour les éléments du jeux de sélection.
; Créé:  2024-04-23T21:19:16.429Z
; Modifié: 2024-04-23T21:27:19.138Z
; 
;@param: {SelectionSet} sset;
;@var: {ename} elem;
;@var: {vlx object} vl-elem;
;
;

(defun MF:ltscaleto1(sset / elem vl-elem)
    (while (setq elem (ssname sset 0))
        (setq vl-elem (vlax-ename->vla-object elem))
        (vla-put-LinetypeScale vl-elem "1")
        (ssdel elem sset)
    )
)