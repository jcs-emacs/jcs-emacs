;;; lang/vhdl/config.el  -*- lexical-binding: t; -*-

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
  (jcs-insert-header-if-valid '("[.]vhd" "[.]vhdl")
                              'jcs-insert-vhdl-template))
