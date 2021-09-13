;;; jcs-css-mode.el --- CSS mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'com-css-sort)
(require 'emmet-mode)
(require 'rainbow-mode)

(require 'jcs-web)

;;
;; (@* "Hook" )
;;

(defun jcs-css-mode-hook ()
  "Hook for CSS mode."
  (impatient-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]css")
                              'jcs-insert-css-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; sort attribute in order => `com-css-sort' package.
  (jcs-bind-key (kbd "C-k s") #'com-css-sort-attributes-block)
  (jcs-bind-key (kbd "C-k d") #'com-css-sort-attributes-document)

  ;; Eemmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line))

(add-hook 'css-mode-hook 'jcs-css-mode-hook)
(add-hook 'css-mode-hook 'emmet-mode)

(provide 'jcs-css-mode)
;;; jcs-css-mode.el ends here
