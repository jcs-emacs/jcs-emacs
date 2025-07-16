;;; tools/direnv/config.el  -*- lexical-binding: t; -*-

(require 'sh-script)
(require 'envrc)

;;
;; (@* "Hook" )
;;

(jcs-add-hook '( envrc-file-mode-hook)
  ;; File Header
  (jcs-insert-header-if-valid '("[.]envrc")
                              'jcs-insert-sh-template))
