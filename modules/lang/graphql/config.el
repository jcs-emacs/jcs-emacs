;;; lang/graphql/config.el  -*- lexical-binding: t; -*-

(require 'company-graphql)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'graphql-mode-hook
  (company-fuzzy-backend-add-before 'company-graphql 'company-dabbrev))
