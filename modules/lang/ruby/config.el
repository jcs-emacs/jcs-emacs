;;; lang/ruby/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-ruby-template "ruby" "default.txt"
  "Header for Ruby header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ruby-mode-hook
  (modify-syntax-entry ?@ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rb")
                              'jcs-insert-ruby-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-ruby
  :hook (ruby-mode . flymake-ruby-load))
