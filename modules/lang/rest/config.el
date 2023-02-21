;;; lang/rest/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-rest-template "rest" "default.txt"
  "RestClient file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'restclient-mode-hook
  (run-hooks 'prog-mode-hook)

  (company-fuzzy-backend-add 'company-restclient)

  (jcs-insert-header-if-valid '("[.]http")
                              'jcs-insert-rest-template))
