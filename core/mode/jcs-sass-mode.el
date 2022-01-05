;;; jcs-sass-mode.el --- Sass mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'ssass-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-sass-template ()
  "Header for SASS header file."
  (jcs--file-header--insert "sass" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'ssass-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]sass")
                              'jcs-insert-sass-template)

  ;; Normal

  ;; Edit
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(provide 'jcs-sass-mode)
;;; jcs-sass-mode.el ends here
