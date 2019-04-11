;;; jcs-js-mode.el --- JSON mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'json-mode)
(defun jcs-json-mode-hook ()
  "JSON mode hook."

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)

  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)


  (defun jcs-json-format()
    "Format for json file."
    (when (jcs-is-current-file-empty-p)
      ;; empty, cause json should only take data.
      ;; Comment will be treat as a data too...
      ))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]json" buffer-file-name) (jcs-json-format))
          ))

  ;; Normal
  (define-key json-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key json-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key json-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key json-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key json-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key json-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key json-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'json-mode-hook 'jcs-json-mode-hook)

(add-to-list 'auto-mode-alist '("\\.json'?\\'" . json-mode))


(provide 'jcs-json-mode)
;;; jcs-json-mode.el ends here
