;;; lang/snippet/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'snippet-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . previous-line)
      ((kbd "<down>") . next-line))))
