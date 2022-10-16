;;; jcs-perl-mode.el --- Perl mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'perl-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-perl-template "perl" "default.txt"
  "Header for Perl header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'perl-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pl")
                              'jcs-insert-perl-template))

(provide 'jcs-perl-mode)
;;; jcs-perl-mode.el ends here
