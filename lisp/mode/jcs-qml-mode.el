;;; jcs-qml-mode.el --- QML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'qml-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-qml-template ()
  "Header for QML header file."
  (jcs--file-header--insert "qml" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'qml-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]qml")
                              'jcs-insert-qml-template))

(provide 'jcs-qml-mode)
;;; jcs-qml-mode.el ends here
