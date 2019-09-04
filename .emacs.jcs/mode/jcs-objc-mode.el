;;; jcs-objc-mode.el --- Objective-C mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-objc-mode-hook ()
  "Objective-C mode hook."
  (abbrev-mode 1)
  (auto-highlight-symbol-mode t)
  (goto-address-mode 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]hin"
                                "[.]hpp"
                                "[.]h")
                              'jcs-objc-header-format)
  (jcs-insert-header-if-valid '("[.]cin"
                                "[.]cpp"
                                "[.]c"
                                "[.]m")
                              'jcs-objc-source-format)

  ;; Normal
  (define-key objc-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key objc-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key objc-mode-map [f7] #'jcs-same-file-other-window)

  (define-key objc-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key objc-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key objc-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comment Block.
  (define-key objc-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key objc-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Comement
  (define-key objc-mode-map (kbd "C-c s") #'jcs-toggle-c-comment-style)

  ;; Undo/Redo
  (define-key objc-mode-map (kbd "C-z") #'jcs-undo)
  (define-key objc-mode-map (kbd "C-y") #'jcs-redo)
  )
(add-hook 'objc-mode-hook 'jcs-objc-mode-hook)


(provide 'jcs-objc-mode)
;;; jcs-objc-mode.el ends here
