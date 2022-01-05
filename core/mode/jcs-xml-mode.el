;;; jcs-xml-mode.el --- XML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'nxml-mode)
(require 'web-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-xml-template ()
  "Header for XML header file."
  (jcs--file-header--insert "xml" "default.txt"))

;;
;; (@* "Hook" )
;;

(add-hook 'nxml-mode-hook 'emmet-mode)

(jcs-add-hook 'nxml-mode-hook
  (auto-rename-tag-mode 1)
  (visual-line-mode t)

  (toggle-truncate-lines 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]xml")
                              'jcs-insert-xml-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "SPC") #'jcs-smart-space)
  (jcs-bind-key (kbd "<backspace>") #'jcs-smart-backspace))


(provide 'jcs-xml-mode)
;;; jcs-xml-mode.el ends here
