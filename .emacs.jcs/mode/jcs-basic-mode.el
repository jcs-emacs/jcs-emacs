;;; jcs-basic-mode.el --- BASIC mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'basic-mode)


(defun jcs-basic-script-format ()
  "Format the given file as a Basic file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-basic-template)))


(defun jcs-basic-mode-hook ()
  "Hook for `basic-mode'."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]bas" buffer-file-name) (jcs-basic-script-format))
          ))

  ;; Normal
  (define-key basic-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key basic-mode-map (kbd "C-c C-c") #'kill-ring-save)
  )
(add-hook 'basic-mode-hook 'jcs-basic-mode-hook)


(provide 'jcs-basic-mode)
;;; jcs-basic-mode.el ends here
