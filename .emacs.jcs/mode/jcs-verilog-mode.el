;;; jcs-verilog-mode.el --- Verilog mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'verilog-mode)


(defun jcs-verilog-mode-hook ()
  "Verilog mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]v" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-verilog-template))
          ))

  )
(add-hook 'verilog-mode-hook 'jcs-verilog-mode-hook)


(provide 'jcs-verilog-mode)
;;; jcs-verilog-mode.el ends here
