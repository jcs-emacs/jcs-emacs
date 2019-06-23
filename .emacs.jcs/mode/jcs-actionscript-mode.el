;;; jcs-actionscript-mode.el --- ActionScript 3.0 mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'actionscript-mode)


(defun jcs-actionscript-mode-hook ()
  "ActionScript mode hook."
  (goto-address-mode 1)

  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]as" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-actionscript-template))
          ))

  ;; Normal
  (define-key actionscript-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key actionscript-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key actionscript-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key actionscript-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key actionscript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key actionscript-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key actionscript-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'actionscript-mode-hook 'jcs-actionscript-mode-hook)


(provide 'jcs-actionscript-mode)
;;; jcs-actionscript-mode.el ends here
