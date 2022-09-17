;;; jcs-ruby-mode.el --- Ruby mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ruby-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-ruby-template "ruby" "default.txt"
  "Header for Ruby header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ruby-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]rb")
                              'jcs-insert-ruby-template))

(provide 'jcs-ruby-mode)
;;; jcs-ruby-mode.el ends here
