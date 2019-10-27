;;; jcs-c++-mode.el --- C++ mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'jcs-cc-mode)


(defun jcs-c++-mode-hook ()
  "C++ mode handling"

  ;; File Header
  (jcs-cc-insert-header)

  ;; Normal
  (define-key c++-mode-map [f8] #'jcs-find-corresponding-file)
  (define-key c++-mode-map [S-f8] #'jcs-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key c++-mode-map [f7] #'jcs-same-file-other-window)

  (define-key c++-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key c++-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key c++-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key c++-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; Comment Block.
  (define-key c++-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key c++-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Comement
  (define-key c++-mode-map (kbd "C-k s") #'jcs-toggle-c-comment-style)

  (define-key c++-mode-map (kbd "#") #'jcs-vs-sharp-key)

  ;; Undo/Redo
  (define-key c++-mode-map (kbd "C-z") #'jcs-undo)
  (define-key c++-mode-map (kbd "C-y") #'jcs-redo)
  )
(add-hook 'c++-mode-hook 'jcs-c++-mode-hook)


(provide 'jcs-c++-mode)
;;; jcs-c++-mode.el ends here
