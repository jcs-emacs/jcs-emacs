;;; jcs-vue-mode.el --- Vue mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'vue-mode)


(defun jcs-vue-mode-hook ()
  "Vue mode hook."
  ;; File Header
  (jcs-insert-header-if-valid '("[.]vue")
                              'jcs-insert-vue-template)
  )
(add-hook 'vue-mode-hook 'jcs-vue-mode-hook)


(provide 'jcs-vue-mode)
;;; jcs-vue-mode.el ends here
