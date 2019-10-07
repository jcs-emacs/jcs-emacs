;;; jcs-vue-mode.el --- Vue mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'vue-mode)

(require 'css-mode)
(require 'web-mode)


(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))


(defun jcs-vue-mode-hook ()
  "Vue mode hook."
  (set-face-background 'mmm-default-submode-face "#000000")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]vue")
                              'jcs-insert-vue-template)
  )
(add-hook 'vue-mode-hook 'jcs-vue-mode-hook)


(provide 'jcs-vue-mode)
;;; jcs-vue-mode.el ends here
