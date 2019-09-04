;;; jcs-dart-mode.el --- Dart mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'dart-mode)


(defun jcs-dart-mode-hook ()
  "Dart mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]dart")
                              'jcs-insert-dart-template)

  )
(add-hook 'dart-mode-hook 'jcs-dart-mode-hook)


(provide 'jcs-dart-mode)
;;; jcs-dart-mode.el ends here
