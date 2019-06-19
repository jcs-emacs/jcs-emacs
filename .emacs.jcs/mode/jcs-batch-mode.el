;;; jcs-batch-mode.el --- Batch Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'bat-mode)


(defun jcs-batch-script-format ()
  "Format the given file as a Batch file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-batch-template)))


(defun jcs-batch-mode-hook ()
  "Batch mode hook."
  (electric-pair-mode nil)
  (abbrev-mode 1)
  (goto-address-mode 1)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]bat" buffer-file-name) (jcs-batch-script-format))
          ))

  ;; Normal
  (define-key bat-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key bat-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key bat-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key bat-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'bat-mode-hook 'jcs-batch-mode-hook)


(provide 'jcs-batch-mode)
;;; jcs-batch-mode.el ends here
