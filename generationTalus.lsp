; *
; MF:generationTalus: desactive la génération du type de ligne
; @params sset (Selection Set) Jeux de selection contenant les polylignes de talus
; Requis pour drcheck.lsp
; *
(defun MF:generationTalus (sset / ename)
  (while (setq ename (ssname sset 0))
         (vla-put-LinetypeGeneration (vlax-ename->vla-object ename) "0")
         (ssdel ename sset)
  )
)