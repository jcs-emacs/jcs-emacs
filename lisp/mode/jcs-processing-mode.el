;;; jcs-processing-mode.el --- Processing mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'processing-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-processing-template ()
  "Header for Processing file."
  (jcs--file-header--insert "processing" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'processing-mode-hook

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pde")
                              'jcs-insert-processing-template)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace))

(provide 'jcs-processing-mode)
;;; jcs-processing-mode.el ends here
