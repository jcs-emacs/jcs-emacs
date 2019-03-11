;;; jcs-swift-mode.el --- Swift mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'swift-mode)
(defun jcs-swift-mode-hook ()
  "Swift mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-swift-format ()
    "Format the given file as a Swift file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-swift-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]swift" buffer-file-name) (jcs-swift-format))
          ))

  ;; jcs Swift key binding
  (define-key swift-mode-map "\ek" #'jcs-maybe-kill-this-buffer)
  )
(add-hook 'swift-mode-hook 'jcs-swift-mode-hook)

(add-to-list 'auto-mode-alist '("\\.swift'?\\'" . swift-mode))


(provide 'jcs-swift-mode)
;;; jcs-swift-mode.el ends here
