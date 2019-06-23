;;; jcs-swift-mode.el --- Swift mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'swift-mode)


(defun jcs-swift-mode-hook ()
  "Swift mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]swift" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-swift-template))
          ))

  ;; Normal
  (define-key swift-mode-map "\ek" #'jcs-maybe-kill-this-buffer)
  )
(add-hook 'swift-mode-hook 'jcs-swift-mode-hook)


(provide 'jcs-swift-mode)
;;; jcs-swift-mode.el ends here
