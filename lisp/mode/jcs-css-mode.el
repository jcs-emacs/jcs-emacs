;;; jcs-css-mode.el --- CSS mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'web-mode)

(require 'com-css-sort)
(require 'emmet-mode)
(require 'rainbow-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-css-template "css" "default.txt"
  "Template for CSS.")

;;
;; (@* "Hook" )
;;

(add-hook 'css-mode-hook 'emmet-mode)

(jcs-add-hook 'css-mode-hook
  (impatient-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]css")
                              'jcs-insert-css-template)

  (jcs-key-local
    `(((kbd "C-k s") . com-css-sort-attributes-block)
      ((kbd "C-k d") . com-css-sort-attributes-document)))

  ;; Eemmet
  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))

(provide 'jcs-css-mode)
;;; jcs-css-mode.el ends here
