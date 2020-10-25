;;; jcs-snippet-mode.el --- Snippet mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yasnippet)

;;
;; (@* "Hook" )
;;

(defun jcs-snippet-mode-hook()
  "Snippet mode hook."

  ;; Normal
  (define-key snippet-mode-map (kbd "<up>") #'previous-line)
  (define-key snippet-mode-map (kbd "<down>") #'next-line))

(add-hook 'snippet-mode-hook 'jcs-snippet-mode-hook)

(provide 'jcs-snippet-mode)
;;; jcs-snippet-mode.el ends here
