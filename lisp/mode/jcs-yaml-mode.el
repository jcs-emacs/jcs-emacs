;;; jcs-yaml-mode.el --- YAML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yaml-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-yaml-template ()
  "Header for YAML header file."
  (jcs--file-header--insert "yaml" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'yaml-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]yaml"
                                "[.]yml")
                              'jcs-insert-yaml-template)

  (jcs-key-local
    `(((kbd "<up>")        . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>")      . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "SPC")         . jcs-smart-space)
      ((kbd "<backspace>") . jcs-smart-backspace))))

(provide 'jcs-yaml-mode)
;;; jcs-yaml-mode.el ends here
