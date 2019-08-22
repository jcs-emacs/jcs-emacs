;;; jcs-scss-mode.el --- SCSS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'scss-mode)


(defun jcs-scss-mode-hook ()
  "SCSS mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)
  (lsp-deferred)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]scss")
                              'jcs-insert-scss-template)

  ;; Normal
  (define-key scss-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key scss-mode-map (kbd "C-c C-c") #'kill-ring-save)

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
