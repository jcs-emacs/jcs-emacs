;;; jcs-verilog-mode.el --- Verilog mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'verilog-mode)


(defun jcs-verilog-mode-hook ()
  "Verilog mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]v")
                              'jcs-insert-verilog-template))

(add-hook 'verilog-mode-hook 'jcs-verilog-mode-hook)


(provide 'jcs-verilog-mode)
;;; jcs-verilog-mode.el ends here
