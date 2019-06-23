;;; jcs-sh-mode.el --- Shell Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'sh-script)


(defun jcs-sh-script-hook()
  "Shell Script mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((or (string-match "[.]sh" buffer-file-name)
               (string-match "[.]linux" buffer-file-name)
               (string-match "[.]macosx" buffer-file-name))
           (jcs-insert-header-if-empty 'jcs-insert-sh-template))
          ))

  ;; Normal
  (define-key sh-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key sh-mode-map (kbd "C-c C-c") #'kill-ring-save)

  ;; save buffer
  (define-key sh-mode-map (kbd "C-s") #'jcs-sh-untabify-save-buffer)
  )
(add-hook 'sh-mode-hook 'jcs-sh-script-hook)


(provide 'jcs-sh-mode)
;;; jcs-sh-mode.el ends here
