;;; jcs-jsx-mode.el --- JavaScript XML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rjsx-mode)
(require 'web-mode)
(require 'emmet-mode)

(defun jcs-jsx-mode-hook ()
  "Mode hook for JSX mode."
  (auto-rename-tag-mode 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]jsx$")
                              'jcs-insert-jsx-template)

  ;; Normal
  (define-key rjsx-mode-map (kbd "C-v") #'jcs-web-yank)
  (define-key rjsx-mode-map (kbd "RET") #'jcs-web-return-key)

  (define-key rjsx-mode-map (kbd "{") #'jcs-web-vs-opening-curly-bracket-key)
  (define-key rjsx-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key rjsx-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; Emmet
  (define-key emmet-mode-keymap (kbd "C-<return>") #'jcs-emmet-expand-line))

(add-hook 'rjsx-mode-hook 'jcs-jsx-mode-hook)
(add-hook 'rjsx-mode-hook 'emmet-mode)

(provide 'jcs-jsx-mode)
;;; jcs-jsx-mode.el ends here
