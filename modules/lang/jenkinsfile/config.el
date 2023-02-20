;;; lang/jenkinsfile/config.el  -*- lexical-binding: t; -*-

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
