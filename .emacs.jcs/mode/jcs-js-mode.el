;;; jcs-js-mode.el --- JavaScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'js2-mode)

(defun jcs--js-to-jsx-mode ()
  "Switch from JavaScript mode to JSX mode if needed."
  (when (and (not (jcs-is-current-major-mode-p "rjsx-mode"))
             (string-match-p "React" (buffer-string)))
    (rjsx-mode)))

(defun jcs-js-mode-hook ()
  "Mode hook for JavaScript mode."
  (auto-rename-tag-mode 1)
  (impatient-mode t)
  (js2-minor-mode 1)

  (setq js2-bounce-indent-p t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]js$")
                              'jcs-insert-js-template)

  ;; Normal
  (define-key js2-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key js2-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key js2-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key js2-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key js2-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key js2-mode-map (kbd "*") #'jcs-c-comment-pair)

  (jcs--js-to-jsx-mode))

(add-hook 'js-mode-hook 'jcs-js-mode-hook)
(add-hook 'js2-mode-hook 'jcs-js-mode-hook)

(provide 'jcs-js-mode)
;;; jcs-js-mode.el ends here
