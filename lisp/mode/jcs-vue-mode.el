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

(file-header-defins jcs-insert-vue-template "vue" "default.txt"
  "Header for Vue header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'vue-mode-hook
  ;; Treat some character as whitespace character.
  (modify-syntax-entry ?- "-")

  (set-face-background 'mmm-default-submode-face "#000000")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]vue")
                              'jcs-insert-vue-template))

(provide 'jcs-vue-mode)
;;; jcs-vue-mode.el ends here
