;;; jcs-verilog-mode.el --- Verilog mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'verilog-mode)

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

(provide 'jcs-verilog-mode)
;;; jcs-verilog-mode.el ends here
