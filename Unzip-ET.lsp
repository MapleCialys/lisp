; ****************************************************************************
; *                               UNZIP-ET.lsp                               *
; *          Dézip automatiquement les fichiers reçu de l'extraction         *
; *                       Configuré pour Bricscad 2023                       *
; *                             Mickael FRANCOIS                             *
; *                                2023-04-19                                *
; ****************************************************************************

(defun c:Unzip-et(/ file-list file1 file2 path)
  (setq etfilepath "C:/bricsEcceTerra/Extraction/")
  ;Récupération du chemin vers le dossier telechargement
  (setq path (strcat "C:/Users/" (getvar "LOGINNAME") "/Downloads/" (VL-FILENAME-BASE (GETVAR "DWGNAME")) ".ZIP"))
  ;Vide le dossier Extraction
  (setq FILE-LIST (vl-directory-files  etfilepath nil 1))
  (foreach FILE1 FILE-LIST
    (progn
      (setq FILE2 (strcat etfilepath FILE1))
      (vl-file-delete FILE2)
    )
  )
  ;Demande le fichier ZIP d'extraction et l'extrait
  (setq FILE3 (getfiled "EXTRACTIONS" path "zip" 0))
  (SETQ FILE3 (vl-string-subst "~1" "m Files" file3))
  (setq FILE3 (strcat "\"" file3 "\""))
  (Setq cmd-line (strcat "C:\\Progra~1\\7-Zip\\7z.exe e " file3 " -o" etfilepath ))
  (command "._SH" cmd-line)
)
