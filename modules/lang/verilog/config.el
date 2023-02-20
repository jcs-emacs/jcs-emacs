;;; lang/verilog/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-verilog-template "verilog" "default.txt"
  "Header for Verilog header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'verilog-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]v")
                              'jcs-insert-verilog-template))
