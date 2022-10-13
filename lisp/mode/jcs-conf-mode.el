;;; jcs-conf-mode.el --- conf mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'conf-javaprop-mode-hook
  (modify-syntax-entry ?_ "w"))  ; Treat underscore as word

(provide 'jcs-conf-mode)
;;; jcs-conf-mode.el ends here
