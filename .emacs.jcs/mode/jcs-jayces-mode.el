;;; jcs-jayces-mode.el --- JayCeS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'jayces-mode)


(defun jcs-jayces-mode-hook ()
  "JayCeS mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((or (string-match "[.]jcs" buffer-file-name)
               (string-match "[.]jayces" buffer-file-name))
           (jcs-insert-header-if-empty 'jcs-insert-jayces-template))
          ))

  )
(add-hook 'jayces-mode-hook 'jcs-jayces-mode-hook)


(provide 'jcs-jayces-mode)
;;; jcs-jayces-mode.el ends here
