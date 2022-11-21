;;; editor/snippets/config.el  -*- lexical-binding: t; -*-

(use-package yasnippet
  :init
  (setq yas-verbosity 0))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'snippet-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . previous-line)
      ((kbd "<down>") . next-line))))
