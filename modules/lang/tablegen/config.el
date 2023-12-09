;;; lang/tablegen/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'tablegen-mode-hook
  (run-hooks 'prog-mode-hook)

  (jcs-use-cc-mutliline-comment)

  (modify-syntax-entry ?_ "w"))
