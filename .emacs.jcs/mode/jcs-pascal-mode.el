;;; jcs-pascal-mode.el --- Pascal mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-pascal-script-format ()
  "Format the given file as a Pascal script."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-pascal-template)))

(require 'pascal)
(defun jcs-pascal-mode-hook ()
  "Pascal mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]pas" buffer-file-name) (jcs-pascal-script-format))
          ))

  ;; Normal
  (define-key pascal-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key pascal-mode-map "\C-c\C-c" #'kill-ring-save)
  )
(add-hook 'pascal-mode-hook 'jcs-pascal-mode-hook)


(provide 'jcs-pascal-mode)
;;; jcs-pascal-mode.el ends here
