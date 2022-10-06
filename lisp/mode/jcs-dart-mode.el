;;; jcs-dart-mode.el --- Dart mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dart-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-dart-template "dart" "default.txt"
  "Template for Dart.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dart-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]dart")
                              'jcs-insert-dart-template))

(provide 'jcs-dart-mode)
;;; jcs-dart-mode.el ends here
