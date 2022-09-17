;;; jcs-cobol-mode.el --- COBOL mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cobol-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-cobol-template "cobol" "default.txt"
  "Template for COBOL.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'cobol-mode-hook
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]cbl")
                              'jcs-insert-cobol-template)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-cobol-mode)
;;; jcs-cobol-mode.el ends here
