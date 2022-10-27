;;; lang/scala/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-scala-template "scala" "default.txt"
  "Header for Scala header file.")

;;
;; (@* "Hook" )
;;

(leaf scala-mode
  :init
  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t))

(jcs-add-hook 'scala-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]scala")
                              'jcs-insert-scala-template))
