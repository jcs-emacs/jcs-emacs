;;; jcs-pascal-mode.el --- Pascal mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'pascal)


(defun jcs-pascal-mode-hook ()
  "Pascal mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pas")
                              'jcs-insert-pascal-template))

(add-hook 'pascal-mode-hook 'jcs-pascal-mode-hook)


(provide 'jcs-pascal-mode)
;;; jcs-pascal-mode.el ends here
