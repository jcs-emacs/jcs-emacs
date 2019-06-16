;;; jcs-swift-mode.el --- Swift mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'swift-mode)


(defun jcs-swift-format ()
  "Format the given file as a Swift file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-swift-template)))


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
          ((string-match "[.]swift" buffer-file-name) (jcs-swift-format))
          ))

  ;; Normal
  (define-key swift-mode-map "\ek" #'jcs-maybe-kill-this-buffer)
  )
(add-hook 'swift-mode-hook 'jcs-swift-mode-hook)


(provide 'jcs-swift-mode)
;;; jcs-swift-mode.el ends here
