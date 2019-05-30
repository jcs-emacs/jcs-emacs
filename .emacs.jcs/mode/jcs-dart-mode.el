;;; jcs-dart-mode.el --- Dart mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-dart-script-format ()
  "Format the given file as a Dart script."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-dart-template)))

(require 'dart-mode)
(defun jcs-dart-mode-hook ()
  "Dart mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]dart" buffer-file-name) (jcs-dart-script-format))
          ))

  ;; Normal
  (define-key dart-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key dart-mode-map "\C-c\C-c" #'kill-ring-save)
  )
(add-hook 'dart-mode-hook 'jcs-dart-mode-hook)


(provide 'jcs-dart-mode)
;;; jcs-dart-mode.el ends here
