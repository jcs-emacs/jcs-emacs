;;; lang/coq/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-coq-template "coq" "default.txt"
  "Coq file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'coq-mode-hook
  (company-fuzzy-backend-add-before 'company-coq-master-backend 'company-dabbrev)
  (company-fuzzy-backend-add-before 'company-coq-choices-backend 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]lua" "[.]luac")
                              'jcs-insert-lua-template))
