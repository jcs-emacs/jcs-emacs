;;; jcs-sh-mode.el --- Shell Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'sh-script)


(defun jcs-sh-script-format ()
  "Format the given file as a shell script file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-sh-template)))


(defun jcs-sh-script-hook()
  "Shell Script mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; TOPIC: Treat underscore as word.
  ;; URL: https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]sh" buffer-file-name) (jcs-sh-script-format))
          ((string-match "[.]linux" buffer-file-name) (jcs-sh-script-format))
          ((string-match "[.]macosx" buffer-file-name) (jcs-sh-script-format))
          ))

  ;; Normal
  (define-key sh-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key sh-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; save buffer
  (define-key sh-mode-map "\C-s" #'jcs-sh-untabify-save-buffer)
  )
(add-hook 'sh-mode-hook 'jcs-sh-script-hook)


(provide 'jcs-sh-mode)
;;; jcs-sh-mode.el ends here
