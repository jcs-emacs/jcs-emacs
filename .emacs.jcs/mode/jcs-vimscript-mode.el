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

  (defun jcs-vim-script-format ()
    "Format the given file as a VimScript file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-vimscript-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]vim" buffer-file-name) (jcs-vim-script-format))
          ((string-match "[.]vimrc" buffer-file-name) (jcs-vim-script-format))
          ((string-match "_vimrc" buffer-file-name) (jcs-vim-script-format))
          ))

  ;; Normal
  (define-key vimrc-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key vimrc-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key vimrc-mode-map "\C-a" #'mark-whole-buffer)

  (define-key vimrc-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key vimrc-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'vimrc-mode-hook 'jcs-vim-mode-hook)

(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)'?\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)_vimrc" . vimrc-mode))


(provide 'jcs-vimscript-mode)
;;; jcs-vimscript-mode.el ends here
