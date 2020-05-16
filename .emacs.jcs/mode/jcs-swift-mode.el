;;; jcs-swift-mode.el --- Swift mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'swift-mode)

(defun jcs-swift-mode-hook ()
  "Swift mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]swift")
                              'jcs-insert-swift-template)

  ;; Normal
  (define-key swift-mode-map (kbd "M-k") #'jcs-maybe-kill-this-buffer))

(add-hook 'swift-mode-hook 'jcs-swift-mode-hook)

(provide 'jcs-swift-mode)
;;; jcs-swift-mode.el ends here
