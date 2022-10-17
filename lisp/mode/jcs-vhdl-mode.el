;;; jcs-vhdl-mode.el --- VHDL mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'vhdl-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-vhdl-template "vhdl" "default.txt"
  "VHDL file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'vhdl-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]vhdl")
                              'jcs-insert-vhdl-template))

(provide 'jcs-vhdl-mode)
;;; jcs-vhdl-mode.el ends here
