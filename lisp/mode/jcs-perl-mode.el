;;; jcs-perl-mode.el --- Perl mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'perl-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-perl-template ()
  "Header for Perl header file."
  (jcs--file-header--insert "perl" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'perl-mode-hook
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pl")
                              'jcs-insert-perl-template)

  (jcs-key-local
    `(((kbd "DEL") . jcs-electric-backspace)
      ((kbd "{")   . jcs-vs-opening-curly-bracket-key)
      ((kbd ";")   . jcs-vs-semicolon-key))))

(provide 'jcs-perl-mode)
;;; jcs-perl-mode.el ends here
