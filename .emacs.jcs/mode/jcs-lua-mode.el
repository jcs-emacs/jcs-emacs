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

  (defun jcs-lua-script-format ()
    "Format the given file as a Lua script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-lua-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]lua" buffer-file-name) (jcs-lua-script-format))
          ((string-match "[.]luac" buffer-file-name) (jcs-lua-script-format))
          ))

  ;; Normal
  (define-key lua-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key lua-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; Comment
  (define-key lua-mode-map (kbd "-") #'jcs-lua-maybe-insert-codedoc)

  ;; comment block
  (define-key lua-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  )
(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)


(provide 'jcs-lua-mode)
;;; jcs-lua-mode.el ends here
