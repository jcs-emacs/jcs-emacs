;;; jcs-yaml-mode.el --- YAML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yaml-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-yaml-template "yaml" "default.txt"
  "Header for YAML header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'yaml-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]yaml"
                                "[.]yml")
                              'jcs-insert-yaml-template)

  (jcs-key-local
    `(((kbd "<up>")   . jcs-smart-previous-line)
      ((kbd "<down>") . jcs-smart-next-line))))

(provide 'jcs-yaml-mode)
;;; jcs-yaml-mode.el ends here
