;;; jcs-jayces-mode.el --- JayCeS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'jayces-mode)


(defun jcs-jayces-mode-hook ()
  "JayCeS mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  ;; File Header
  (jcs-insert-header-if-valid '("[.]jcs"
                                "[.]jayces")
                              'jcs-insert-jayces-template)

  )
(add-hook 'jayces-mode-hook 'jcs-jayces-mode-hook)


(provide 'jcs-jayces-mode)
;;; jcs-jayces-mode.el ends here
