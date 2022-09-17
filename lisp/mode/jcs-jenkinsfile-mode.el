;;; jcs-jenkinsfile-mode.el --- Jenkinsfile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jenkinsfile-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-jenkinsfile-template "jenkins" "default.txt"
  "Header for Jenkinsfile.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'jenkinsfile-mode-hook
  (jcs-insert-header-if-valid '("Jenkinsfile")
                              'jcs-insert-jenkinsfile-template))

(provide 'jcs-jenkinsfile-mode)
;;; jcs-jenkinsfile-mode.el ends here
