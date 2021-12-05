;;; jcs-fountain-mode.el --- Fountain mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fountain-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-fountain-template ()
  "Template for Fountain Lisp."
  (jcs--file-header--insert "fountain" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-fountain-mode-hook ()
  "Fountain mode hook."
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fountain")
                              'jcs-insert-fountain-template))

(add-hook 'fountain-mode-hook 'jcs-fountain-mode-hook)

(provide 'jcs-fountain-mode)
;;; jcs-fountain-mode.el ends here
