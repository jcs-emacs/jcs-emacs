;;; jcs-less-css-mode.el --- LESS CSS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'less-css-mode)


(defun jcs-less-css-mode-hook ()
  "LESS CSS mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]less")
                              'jcs-insert-less-template)

  )
(add-hook 'less-css-mode-hook 'jcs-less-css-mode-hook)


(provide 'jcs-less-css-mode)
;;; jcs-less-css-mode.el ends here
