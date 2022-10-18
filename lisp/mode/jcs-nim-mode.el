;;; jcs-nim-mode.el --- Nim mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nim-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-nim-template "nim" "default.txt"
  "Nim file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nim-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]nim")
                              'jcs-insert-nim-template))

(provide 'jcs-nim-mode)
;;; jcs-nim-mode.el ends here
