;;; lang/mermaid/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-mermaid-template "mermaid" "default.txt"
  "Header for Mermaid header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'mermaid-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]mmd")
                              'jcs-insert-mermaid-template))
