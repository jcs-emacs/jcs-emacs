;;; jcs-markdown-mode.el --- Markdown mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'markdown-mode)

(require 'jcs-markdown-func)
(require 'jcs-web-func)

;;
;; (@* "Hook" )
;;

(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."
  (emojify-mode 1)

  (jcs-make-electric-pair-pairs-local '((?\` . ?\`)))

  ;; Normal
  (define-key markdown-mode-map (kbd "<backspace>") #'jcs-real-backspace)
  (define-key markdown-mode-map (kbd "RET") #'jcs-markdown-return-key)

  ;; Eemmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line))

(add-hook 'markdown-mode-hook 'jcs-markdown-mode-hook)
(add-hook 'markdown-mode-hook 'emmet-mode)

(provide 'jcs-markdown-mode)
;;; jcs-markdown-mode.el ends here
