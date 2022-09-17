;;; jcs-qml-mode.el --- QML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'qml-mode)

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

(provide 'jcs-qml-mode)
;;; jcs-qml-mode.el ends here
