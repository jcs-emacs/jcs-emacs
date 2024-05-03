;;; lang/perl/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-perl-template "perl" "default.txt"
  "Header for Perl header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'perl-mode-hook
  (company-fuzzy-backend-add-before 'company-plsense 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pl")
                              'jcs-insert-perl-template))
