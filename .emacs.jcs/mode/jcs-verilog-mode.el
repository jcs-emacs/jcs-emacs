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

  (defun jcs-verilog-script-format ()
    "Format the given file as a Verilog file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-verilog-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]v" buffer-file-name) (jcs-verilog-script-format))
          ))

  )
(add-hook 'verilog-mode-hook 'jcs-verilog-mode-hook)

(add-to-list 'auto-mode-alist '("\\.v'?\\'" . verilog-mode))


(provide 'jcs-verilog-mode)
;;; jcs-verilog-mode.el ends here
