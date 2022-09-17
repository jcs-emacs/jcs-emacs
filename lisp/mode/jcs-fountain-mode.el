;;; jcs-fountain-mode.el --- Fountain mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fountain-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-fountain-template "fountain" "default.txt"
  "Template for Fountain Lisp.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'fountain-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]fountain")
                              'jcs-insert-fountain-template))

(provide 'jcs-fountain-mode)
;;; jcs-fountain-mode.el ends here
