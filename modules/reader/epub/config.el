;;; reader/epub/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nov-mode-hook
  (visual-line-mode 1)
  (face-remap-add-relative 'variable-pitch :family "Times New Roman")

  (jcs-key-local
    `(((kbd "M-<up>")   . nov-previous-document)
      ((kbd "M-<down>") . nov-next-document))))
