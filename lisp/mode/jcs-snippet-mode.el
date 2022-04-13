;;; jcs-snippet-mode.el --- Snippet mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yasnippet)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'snippet-mode-hook
  (jcs-key-local
    `(((kbd "<up>")   . previous-line)
      ((kbd "<down>") . next-line))))

(provide 'jcs-snippet-mode)
;;; jcs-snippet-mode.el ends here
