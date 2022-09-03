;;; jcs-haml-mode.el --- Web Development mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haml-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-haml-template ()
  "Template for HAML."
  (jcs--file-header--insert "haml" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'haml-mode-hook
  (jcs-insert-header-if-valid '("[.]haml")
                              'jcs-insert-haml-template))

(provide 'jcs-haml-mode)
;;; jcs-haml-mode.el ends here
