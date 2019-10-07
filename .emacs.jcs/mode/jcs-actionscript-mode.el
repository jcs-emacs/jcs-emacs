;;; jcs-actionscript-mode.el --- ActionScript 3.0 mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'actionscript-mode)


(defun jcs-actionscript-mode-hook ()
  "ActionScript mode hook."
  (abbrev-mode 1)
  (auto-highlight-symbol-mode t)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (highlight-indent-guides-mode 1)
  (lsp-deferred)

  (jcs-use-cc-mutliline-comment)

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]as")
                              'jcs-insert-actionscript-template)

  ;; Normal
  (define-key actionscript-mode-map (kbd "<up>") #'jcs-smart-indent-up)
  (define-key actionscript-mode-map (kbd "<down>") #'jcs-smart-indent-down)

  (define-key actionscript-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key actionscript-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key actionscript-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key actionscript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  (define-key actionscript-mode-map (kbd "C-v") #'jcs-smart-yank)

  ;; comment block
  (define-key actionscript-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key actionscript-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'actionscript-mode-hook 'jcs-actionscript-mode-hook)


(provide 'jcs-actionscript-mode)
;;; jcs-actionscript-mode.el ends here
