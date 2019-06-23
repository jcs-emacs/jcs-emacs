;;; jcs-vimscript-mode.el --- VimScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'vimrc-mode)


(defun jcs-vim-mode-hook ()
  "Vimrc mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((or (string-match "[.]vim" buffer-file-name)
               (string-match "[.]vimrc" buffer-file-name)
               (string-match "_vimrc" buffer-file-name))
           (jcs-insert-header-if-empty 'jcs-insert-vimscript-template))
          ))

  ;; Normal
  (define-key vimrc-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key vimrc-mode-map (kbd "C-c C-c") #'kill-ring-save)
  (define-key vimrc-mode-map (kbd "C-a") #'jcs-mark-whole-buffer)

  (define-key vimrc-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key vimrc-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'vimrc-mode-hook 'jcs-vim-mode-hook)


(provide 'jcs-vimscript-mode)
;;; jcs-vimscript-mode.el ends here
