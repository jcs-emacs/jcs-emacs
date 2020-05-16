;;; jcs-jsx-mode.el --- JavaScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rjsx-mode)

(defun jcs-jsx-mode-hook ()
  "Mode hook for JSX mode."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]jsx$")
                              'jcs-insert-jsx-template))

(add-hook 'rjsx-mode-hook 'jcs-jsx-mode-hook)

(provide 'jcs-jsx-mode)
;;; jcs-jsx-mode.el ends here
