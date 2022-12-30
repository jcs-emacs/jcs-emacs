;;; lang/jsx/config.el  -*- lexical-binding: t; -*-

(require 'web-mode)
(require 'emmet-mode)

;; Ask the source SC for editing JavaScript XML file.
(file-header-defsrc jcs-jsx--ask-source "Major source for this JavaScript XML file: "
  '(("Default"      . "Normal JSX file")
    ("ReactJS"      . "Scripting for React.js")
    ("React Native" . "Scripting for React Native"))
  (pcase index
    (0 (jcs-insert-jsx-template))
    (1 (jcs-insert-jsx-react-js-template))
    (2 (jcs-insert-jsx-react-native-template))))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-jsx-template "jsx" "default.txt"
  "Template for JavaScript XML (JSX).")

(file-header-defins jcs-insert-jsx-react-js-template "jsx" "react/js.txt"
  "Template for React JS JavaScript XML (JSX).")

(file-header-defins jcs-insert-jsx-react-native-template "jsx" "react/native.txt"
  "Template for React Native JavaScript XML (JSX).")

;;
;; (@* "Hook" )
;;

(add-hook 'js-jsx-mode-hook 'emmet-mode)

(jcs-add-hook 'js-jsx-mode
  (auto-rename-tag-mode 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]jsx$")
                              'jcs-jsx--ask-source
                              :interactive t)

  (jcs-key-local
    `(((kbd "{") . jcs-web-vs-opening-curly-bracket-key)))

  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))
