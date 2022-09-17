;;; jcs-jsx-mode.el --- JavaScript XML mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rjsx-mode)
(require 'web-mode)
(require 'emmet-mode)

(defun jcs-jsx--ask-source (sc)
  "Ask the source SC for editing JavaScript XML file."
  (interactive
   (list (completing-read
          "Major source for this JavaScript XML file: "
          '("Default"
            "ReactJS"
            "React Native"))))
  (pcase sc
    ("Default" (jcs-insert-jsx-template))
    ("ReactJS" (jcs-insert-jsx-react-js-template))
    ("React Native" (jcs-insert-jsx-react-native-template))))

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

(add-hook 'rjsx-mode-hook 'emmet-mode)

(jcs-add-hook 'rjsx-mode-hook
  (auto-rename-tag-mode 1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]jsx$")
                              'jcs-jsx--ask-source
                              :interactive t)

  (jcs-key-local
    `(((kbd "{") . jcs-web-vs-opening-curly-bracket-key)))

  (jcs-key emmet-mode-keymap
    `(((kbd "C-<return>") . jcs-emmet-expand-line))))

(provide 'jcs-jsx-mode)
;;; jcs-jsx-mode.el ends here
