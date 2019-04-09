;;; jcs-scss-mode.el --- SCSS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'scss-mode)
(defun jcs-scss-mode-hook ()
  "SCSS mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (defun jcs-scss-file-format ()
    "Format the given file as a SCSS file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-scss-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]scss" buffer-file-name) (jcs-scss-file-format))
          ))

  ;; Normal
  (define-key scss-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key scss-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; Save
  (define-key css-mode-map "\C-s" #'jcs-css-save-buffer)

  ;; comment block
  (define-key scss-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key scss-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; sort attribute in order => `css-sort' package.
  (define-key scss-mode-map "\C-ks" #'jcs-css-sort-attributes)
  (define-key scss-mode-map "\C-kd" #'jcs-css-sort-attributes-document)
  )
(add-hook 'scss-mode-hook 'jcs-scss-mode-hook)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))


(provide 'jcs-scss-mode)
;;; jcs-scss-mode.el ends here
