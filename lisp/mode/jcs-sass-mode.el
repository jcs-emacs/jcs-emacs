;;; jcs-sass-mode.el --- Sass mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'ssass-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-sass-template "sass" "default.txt"
  "Header for SASS header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ssass-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]sass")
                              'jcs-insert-sass-template)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next)))))

(provide 'jcs-sass-mode)
;;; jcs-sass-mode.el ends here
