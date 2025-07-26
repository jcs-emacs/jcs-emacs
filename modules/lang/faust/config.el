;;; lang/faust/config.el  -*- lexical-binding: t; -*-

(require 'faustine)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-faust-template "faust" "default.txt"
  "Template for Faust.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'faust-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]dsp")
                              'jcs-insert-faust-template))
