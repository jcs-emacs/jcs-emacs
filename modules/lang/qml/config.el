;;; lang/qml/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-qml-template "qml" "default.txt"
  "Header for QML header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'qml-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]qml")
                              'jcs-insert-qml-template))
