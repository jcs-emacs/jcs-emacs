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

(defun jcs-processing-mode-hook ()
  "Hook for processing mode."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pde")
                              'jcs-insert-processing-template)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace))

(add-hook 'processing-mode-hook 'jcs-processing-mode-hook)

(provide 'jcs-processing-mode)
;;; jcs-processing-mode.el ends here
