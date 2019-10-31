;;; jcs-basic-mode.el --- BASIC mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'basic-mode)


(defun jcs-basic-mode-hook ()
  "Hook for `basic-mode'."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]bas")
                              'jcs-insert-basic-template))

(add-hook 'basic-mode-hook 'jcs-basic-mode-hook)


(provide 'jcs-basic-mode)
;;; jcs-basic-mode.el ends here
