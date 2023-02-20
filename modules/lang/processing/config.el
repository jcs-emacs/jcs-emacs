;;; lang/processing/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-processing-template "processing" "default.txt"
  "Header for Processing file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'processing-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pde")
                              'jcs-insert-processing-template))
