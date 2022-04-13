;;; jcs-verilog-mode.el --- Verilog mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'verilog-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-verilog-template ()
  "Header for Verilog header file."
  (jcs--file-header--insert "verilog" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'verilog-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]v")
                              'jcs-insert-verilog-template))

(provide 'jcs-verilog-mode)
;;; jcs-verilog-mode.el ends here
