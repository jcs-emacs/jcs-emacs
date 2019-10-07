;;; jcs-vue-mode.el --- Vue mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'vue-mode)

(require 'css-mode)
(require 'js2-mode)
(require 'web-mode)


(defun jcs-vue-mode-hook ()
  "Vue mode hook."
  (set-face-background 'mmm-default-submode-face "#000000")

  ;; Treat some character as whitespace character.
  (modify-syntax-entry ?- "-")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]vue")
                              'jcs-insert-vue-template)

  (defun jcs--vue-mode--post-command-hook ()
    "Vue mode `post-command-hook'."
    (let ((cur-indent-line-fnc indent-line-function))
      (cond ((jcs-is-current-major-mode-p '("js-mode"
                                            "typescript-mode"))
             (jcs-use-cc-mutliline-comment)
             (setq syntax-ppss-table nil))
            (t
             (setq-local indent-line-function 'web-mode-indent-line)))
      (unless (eq cur-indent-line-fnc indent-line-function)
        (call-interactively #'indent-for-tab-command))))
  (add-hook 'post-command-hook 'jcs--vue-mode--post-command-hook nil t)
  )
(add-hook 'vue-mode-hook 'jcs-vue-mode-hook)


(provide 'jcs-vue-mode)
;;; jcs-vue-mode.el ends here
