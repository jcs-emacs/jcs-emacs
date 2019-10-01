;;; jcs-ruby-mode.el --- Ruby mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ruby-mode)


(defun jcs-ruby-mode-hook ()
  "Ruby mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rb")
                              'jcs-insert-ruby-template)

  )
(add-hook 'ruby-mode-hook 'jcs-ruby-mode-hook)


(provide 'jcs-ruby-mode)
;;; jcs-ruby-mode.el ends here
