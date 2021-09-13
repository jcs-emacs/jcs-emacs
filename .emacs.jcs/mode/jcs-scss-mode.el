;;; jcs-scss-mode.el --- SCSS mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'scss-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-scss-mode-hook ()
  "SCSS mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]scss")
                              'jcs-insert-scss-template)

  ;; sort attribute in order => `com-css-sort' package.
  (jcs-bind-key (kbd "C-k s") #'com-css-sort-attributes-block)
  (jcs-bind-key (kbd "C-k d") #'com-css-sort-attributes-document))

(add-hook 'scss-mode-hook 'jcs-scss-mode-hook)

(provide 'jcs-scss-mode)
;;; jcs-scss-mode.el ends here
