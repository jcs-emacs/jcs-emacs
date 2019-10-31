;;; jcs-xml-mode.el --- XML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'nxml-mode)

(require 'jcs-web-func)


(defun jcs-xml-mode-hook ()
  "XML mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]xml")
                              'jcs-insert-xml-template)

  ;; Normal
  (define-key nxml-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (define-key nxml-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  ;; Edit
  (define-key web-mode-map (kbd "RET") #'jcs-web-return-key))

;; STUDY: they ae using nxml-mode instead of xml-mode
;; which is really weird.
(add-hook 'nxml-mode-hook 'jcs-xml-mode-hook)
(add-hook 'nxml-mode-hook 'emmet-mode)


(provide 'jcs-xml-mode)
;;; jcs-xml-mode.el ends here
