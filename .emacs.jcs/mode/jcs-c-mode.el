;;; jcs-c-mode.el --- C mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-cc-mode)

(defun jcs-c-mode-hook ()
  "C mode handling"

  (jcs-company-safe-add-backend 'company-c-headers)

  ;; File Header
  (jcs-cc-insert-header)

  ;; Normal
  (jcs-bind-key [f8] #'jcs-find-corresponding-file)
  (jcs-bind-key [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the corresponding file.
  (jcs-bind-key [f7] #'jcs-same-file-other-window)

  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comement
  (jcs-bind-key (kbd "C-k s") #'jcs-toggle-c-comment-style)

  (jcs-bind-key (kbd "#") #'jcs-vs-sharp-key)

  ;; Undo/Redo
  (jcs-bind-key (kbd "C-z") #'jcs-undo)
  (jcs-bind-key (kbd "C-y") #'jcs-redo))

(add-hook 'c-mode-hook 'jcs-c-mode-hook)

(provide 'jcs-c-mode)
;;; jcs-c-mode.el ends here
