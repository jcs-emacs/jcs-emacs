;;; lang/feature/config.el  -*- lexical-binding: t; -*-

(require 'org)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'feature-mode-hook
  (run-hooks 'prog-mode-hook))
