;;; jcs-cobol-mode.el --- COBOL mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'cobol-mode)
(defun jcs-cobol-mode-hook ()
  "COBOL mode hook."
  (electric-pair-mode nil)
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (defun jcs-cobol-format ()
    "Format the given file as a COBOL file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-cobol-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]cbl" buffer-file-name) (jcs-cobol-format))
          ))

  ;; Set Faces.
  (face-remap-add-relative 'font-lock-comment-face '(jcs-font-lock-comment-face))
  (face-remap-add-relative 'font-lock-string-face '(jcs-font-lock-string-face))

  ;; Normal
  (define-key cobol-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key cobol-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key cobol-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key cobol-mode-map (kbd "<down>") #'jcs-py-indent-down)
  )
(add-hook 'cobol-mode-hook 'jcs-cobol-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cbl?\\'" . cobol-mode))


(provide 'jcs-cobol-mode)
;;; jcs-cobol-mode.el ends here
