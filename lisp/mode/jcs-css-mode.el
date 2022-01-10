;;; jcs-css-mode.el --- CSS mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'web-mode)

(require 'com-css-sort)
(require 'emmet-mode)
(require 'rainbow-mode)

(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  ;; NOTE: after using this, I think is better if I bind this function/command
  ;; to another key.
  ;;(com-css-sort-attributes-document)
  (jcs-untabify-save-buffer))

;;
;; (@* "Templates" )
;;

(defun jcs-insert-css-template ()
  "Template for CSS."
  (jcs--file-header--insert "css" "default.txt"))

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
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "DEL")    . jcs-electric-backspace)
      ((kbd "{")      . jcs-vs-opening-curly-bracket-key)
      ((kbd ";")      . jcs-vs-semicolon-key)
      ((kbd "C-k s")  . com-css-sort-attributes-block)
      ((kbd "C-k d")  . com-css-sort-attributes-document)))

  ;; Eemmet
  (jcs-key emmet-mode-keymap
    `((kbd "C-<return>") . jcs-emmet-expand-line)))

(provide 'jcs-css-mode)
;;; jcs-css-mode.el ends here
