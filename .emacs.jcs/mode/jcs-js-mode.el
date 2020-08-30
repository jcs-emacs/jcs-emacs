;;; jcs-js-mode.el --- JavaScript mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'js2-mode)

(defun jcs--js-to-jsx-mode (&optional force)
  "Switch from JavaScript mode to JSX mode, FORCE will ignore any conditions."
  (when (or (and (not (jcs-is-current-major-mode-p "rjsx-mode"))
                 (string-match-p "React" (buffer-string)))
            force)
    (message "[INFO] Detect JSX file, change to `rjsx-mode` instead")
    (rjsx-mode)))

(defun jcs-js--ask-source (sc)
  "Ask the source SC for editing JavaScript file."
  (interactive
   (list (completing-read
          "Major source for this JavaScript file: " '("Default (JS)"
                                                      "Default (JSX)"
                                                      "ReactJS"
                                                      "React Native"))))
  (let (to-jsx)
    (cond ((string= sc "Default (JS)") (jcs-insert-js-template))
          ((string= sc "Default (JSX)") (jcs-insert-jsx-template)
           (setq to-jsx t))
          ((string= sc "ReactJS") (jcs-insert-jsx-react-js-template)
           (setq to-jsx t))
          ((string= sc "React Native") (jcs-insert-jsx-react-native-template)
           (setq to-jsx t)))
    (when to-jsx (jcs--js-to-jsx-mode t))))

;;----------------------------------------------------------------------------

(defun jcs-js-mode-hook ()
  "Mode hook for JavaScript mode."
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

  ;; Normal
  (define-key js2-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key js2-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key js2-mode-map (kbd "}") #'jcs-vs-closing-curly-bracket-key)
  (define-key js2-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key js2-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key js2-mode-map (kbd "*") #'jcs-c-comment-pair)

  (jcs--js-to-jsx-mode))

(add-hook 'js-mode-hook 'jcs-js-mode-hook)
(add-hook 'js2-mode-hook 'jcs-js-mode-hook)

(provide 'jcs-js-mode)
;;; jcs-js-mode.el ends here
