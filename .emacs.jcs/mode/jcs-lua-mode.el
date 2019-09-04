;;; jcs-lua-mode.el --- Lua mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'lua-mode)


(defun jcs-lua-mode-hook ()
  "Lau mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]lua"
                                "[.]luac")
                              'jcs-insert-lua-template)

  ;; Comment
  (define-key lua-mode-map (kbd "-") #'jcs-lua-maybe-insert-codedoc)

  ;; comment block
  (define-key lua-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  )
(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)


(provide 'jcs-lua-mode)
;;; jcs-lua-mode.el ends here
