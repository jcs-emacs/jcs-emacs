;;; jcs-go-mode.el --- GO mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'go-mode)
(defun jcs-go-mode-hook ()
  "Go mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (defun jcs-go-script-format ()
    "Format the given file as a GO file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-go-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]go" buffer-file-name) (jcs-go-script-format))
          ))

  ;; Normal
  (define-key go-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key go-mode-map "\C-c\C-c" #'kill-ring-save)
  )
(add-hook 'go-mode-hook 'jcs-go-mode-hook)

(add-to-list 'auto-mode-alist '("\\.go?\\'" . go-mode))


(provide 'jcs-go-mode)
;;; jcs-go-mode.el ends here
