;;; jcs-sass-mode.el --- Sass mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'ssass-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-sass-mode-hook ()
  "Sass mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sass")
                              'jcs-insert-sass-template)

  ;; Normal

  ;; Edit
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next)))

(add-hook 'ssass-mode-hook 'jcs-sass-mode-hook)

(provide 'jcs-sass-mode)
;;; jcs-sass-mode.el ends here
