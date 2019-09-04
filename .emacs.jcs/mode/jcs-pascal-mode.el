;;; jcs-pascal-mode.el --- Pascal mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'pascal)


(defun jcs-pascal-mode-hook ()
  "Pascal mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pas")
                              'jcs-insert-pascal-template)

  )
(add-hook 'pascal-mode-hook 'jcs-pascal-mode-hook)


(provide 'jcs-pascal-mode)
;;; jcs-pascal-mode.el ends here
