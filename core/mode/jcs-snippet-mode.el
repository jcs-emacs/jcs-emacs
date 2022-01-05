;;; jcs-snippet-mode.el --- Snippet mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yasnippet)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'snippet-mode-hook
  ;; Normal
  (jcs-bind-key (kbd "<up>") #'previous-line)
  (jcs-bind-key (kbd "<down>") #'next-line))

(provide 'jcs-snippet-mode)
;;; jcs-snippet-mode.el ends here
