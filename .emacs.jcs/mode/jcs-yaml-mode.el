;;; jcs-yaml-mode.el --- YAML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yaml-mode)

(require 'jcs-python)

;;
;; (@* "Hook" )
;;

(defun jcs-yaml-mode-hook ()
  "YAML mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]yaml"
                                "[.]yml")
                              'jcs-insert-yaml-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "SPC") #'jcs-smart-space)
  (jcs-bind-key (kbd "<backspace>") #'jcs-smart-backspace))

(add-hook 'yaml-mode-hook 'jcs-yaml-mode-hook)

(provide 'jcs-yaml-mode)
;;; jcs-yaml-mode.el ends here
