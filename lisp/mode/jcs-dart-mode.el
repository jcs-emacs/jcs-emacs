;;; jcs-dart-mode.el --- Dart mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dart-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-dart-template ()
  "Template for Dart."
  (jcs--file-header--insert "dart" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'dart-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]dart")
                              'jcs-insert-dart-template))

(provide 'jcs-dart-mode)
;;; jcs-dart-mode.el ends here
