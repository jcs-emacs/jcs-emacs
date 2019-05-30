;;; jcs-scss-mode.el --- SCSS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-scss-file-format ()
  "Format the given file as a SCSS file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-scss-template)))

(require 'scss-mode)
(defun jcs-scss-mode-hook ()
  "SCSS mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

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

  ;; sort attribute in order => `com-css-sort' package.
  (define-key scss-mode-map "\C-ks" #'com-css-sort-attributes-block)
  (define-key scss-mode-map "\C-kd" #'com-css-sort-attributes-document)
  )
(add-hook 'scss-mode-hook 'jcs-scss-mode-hook)


(provide 'jcs-scss-mode)
;;; jcs-scss-mode.el ends here
