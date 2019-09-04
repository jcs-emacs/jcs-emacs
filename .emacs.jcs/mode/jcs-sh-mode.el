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

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sh"
                                "[.]linux"
                                "[.]macosx")
                              'jcs-insert-sh-template)

  ;; Normal
  (define-key sh-mode-map (kbd "C-s") #'jcs-sh-untabify-save-buffer)
  )
(add-hook 'sh-mode-hook 'jcs-sh-script-hook)


(provide 'jcs-sh-mode)
;;; jcs-sh-mode.el ends here
