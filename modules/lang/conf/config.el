;;; lang/conf/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'conf-javaprop-mode-hook
  (modify-syntax-entry ?_ "w"))  ; Treat underscore as word
