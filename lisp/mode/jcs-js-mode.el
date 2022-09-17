;;; jcs-js-mode.el --- JavaScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'js2-mode)

(defconst jcs-javascript-modes '(javascript-mode js-mode js2-mode js3-mode)
  "List of all JavaScript major modes.")

(defun jcs--js-to-jsx-mode (&optional force)
  "Switch from JavaScript mode to JSX mode, FORCE will ignore any conditions."
  (let (case-fold-search)
    (when (or (and (memq major-mode jcs-javascript-modes)
                   (string-match-p "React" (buffer-string)))
              force)
      (message "[INFO] Detect JSX file, change to `rjsx-mode` instead")
      (rjsx-mode))))

(defun jcs-js--ask-source (sc)
  "Ask the source SC for editing JavaScript file."
  (interactive
   (list (completing-read
          "Major source for this JavaScript file: " '("Default (JS)"
                                                      "Default (JSX)"
                                                      "ReactJS"
                                                      "React Native"))))
  (let (to-jsx)
    (pcase sc
      ("Default (JS)" (jcs-insert-js-template))
      ("Default (JSX)" (jcs-insert-jsx-template) (setq to-jsx t))
      ("ReactJS" (jcs-insert-jsx-react-js-template) (setq to-jsx t))
      ("React Native" (jcs-insert-jsx-react-native-template) (setq to-jsx t)))
    (when to-jsx (jcs--js-to-jsx-mode t))))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-js-template "js" "default.txt"
  "Template for JavaScript.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook '(js-mode-hook js2-mode-hook)
  (auto-rename-tag-mode 1)
  (impatient-mode t)
  (js2-minor-mode 1)

  (setq js2-bounce-indent-p t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]js$")
                              'jcs-js--ask-source
                              :interactive t)

  (jcs-key-local
    `(((kbd "DEL") . jcs-electric-backspace)))

  (jcs--js-to-jsx-mode))

(provide 'jcs-js-mode)
;;; jcs-js-mode.el ends here
