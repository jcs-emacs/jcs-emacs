;;; jcs-vue-mode.el --- Vue mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'vue-mode)


(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))


(defun jcs-vue-mode-hook ()
  "Vue mode hook."
  ;; File Header
  (jcs-insert-header-if-valid '("[.]vue")
                              'jcs-insert-vue-template)
  )
(add-hook 'vue-mode-hook 'jcs-vue-mode-hook)


(provide 'jcs-vue-mode)
;;; jcs-vue-mode.el ends here
