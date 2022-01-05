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

(defun jcs-xml-mode-hook ()
  "XML mode hook."
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

;; STUDY: they ae using nxml-mode instead of xml-mode
;; which is really weird.
(add-hook 'nxml-mode-hook 'jcs-xml-mode-hook)
(add-hook 'nxml-mode-hook 'emmet-mode)

(provide 'jcs-xml-mode)
;;; jcs-xml-mode.el ends here
