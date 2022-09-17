;;; jcs-haml-mode.el --- Web Development mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'haml-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-haml-template "haml" "default.txt"
  "Template for HAML.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'haml-mode-hook
  (jcs-insert-header-if-valid '("[.]haml")
                              'jcs-insert-haml-template))

(provide 'jcs-haml-mode)
;;; jcs-haml-mode.el ends here
