;;; jcs-vue-mode.el --- Vue mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'vue-mode)

(require 'css-mode)
(require 'js2-mode)
(require 'typescript-mode)
(require 'web-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-vue-template ()
  "Header for Vue header file."
  (jcs--file-header--insert "vue" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'vue-mode-hook
  (set-face-background 'mmm-default-submode-face "#000000")

  ;; Treat some character as whitespace character.
  (modify-syntax-entry ?- "-")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]vue")
                              'jcs-insert-vue-template))

(provide 'jcs-vue-mode)
;;; jcs-vue-mode.el ends here
