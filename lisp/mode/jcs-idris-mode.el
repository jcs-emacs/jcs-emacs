;;; jcs-idris-mode.el --- Tdris mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'idris-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-idris-template "idris" "default.txt"
  "Idris file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'idris-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]lidr")
                              'jcs-insert-idris-template))

(provide 'jcs-idris-mode)
;;; jcs-idris-mode.el ends here
