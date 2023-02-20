;;; lang/xml/config.el  -*- lexical-binding: t; -*-

(require 'web-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-xml-template "xml" "default.txt"
  "Header for XML header file.")

;;
;; (@* "Hook" )
;;

(add-hook 'nxml-mode-hook 'emmet-mode)

(jcs-add-hook 'nxml-mode-hook
  (setq truncate-lines t)

  (auto-rename-tag-mode 1)
  (visual-line-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]xml")
                              'jcs-insert-xml-template)

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))
