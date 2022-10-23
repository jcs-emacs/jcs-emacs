;;; jcs-js-mode.el --- JavaScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'js)

;;
;; (@* "Templates" )
;;

(file-header-defsrc jcs-js--ask-source "Major source for this JavaScript file: "
  '("Default (JS)" "Default (JSX)" "ReactJS" "React Native")
  (pcase index
    (0 (jcs-insert-js-template))
    (1 (jcs-insert-jsx-template))
    (2 (jcs-insert-jsx-react-js-template) )
    (3 (jcs-insert-jsx-react-native-template)))
  (js-mode))  ; detect for JSX, switch if needed

(file-header-defins jcs-insert-js-template "js" "default.txt"
  "Template for JavaScript.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'js-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (auto-rename-tag-mode 1)
  (impatient-mode t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]js$")
                              'jcs-js--ask-source
                              :interactive t))

(provide 'jcs-js-mode)
;;; jcs-js-mode.el ends here
