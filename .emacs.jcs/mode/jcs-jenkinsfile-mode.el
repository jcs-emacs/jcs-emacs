;;; jcs-jenkinsfile-mode.el --- Jenkinsfile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jenkinsfile-mode)

(defun jcs-jenkinsfile-mode-hook ()
  "Jenkinsfile mode hook."
  (jcs-insert-header-if-valid '("Jenkinsfile")
                              'jcs-insert-jenkinsfile-template))

(add-hook 'jenkinsfile-mode-hook 'jcs-jenkinsfile-mode-hook)

(provide 'jcs-jenkinsfile-mode)
;;; jcs-jenkinsfile-mode.el ends here
