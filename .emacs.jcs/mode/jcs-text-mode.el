;;; jcs-text-mode.el --- Text modes. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-text-mode-format()
  "Fromat the given file as a normal Text file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-text-template)))

(defun jcs-text-mode-hook ()
  "Text mode hook."
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]txt" buffer-file-name) (jcs-text-mode-format))
          ))

  ;; Normal
  (define-key text-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key text-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key text-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key text-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'text-mode-hook 'jcs-text-mode-hook)


(provide 'jcs-text-mode)
;;; jcs-text-mode.el ends here
