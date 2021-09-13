;;; jcs-java-mode.el --- Java mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'javadoc-lookup)
(require 'organize-imports-java)

(setq javadoc-lookup-completing-read-function #'completing-read)

;;
;; (@* "Hook" )
;;

(defun jcs-java-mode-hook ()
  "Java mode hook."

  (setq-local docstr-show-type-name nil)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]java")
                              'jcs-insert-java-template)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key)

  ;; switch window
  (jcs-bind-key "\ew" #'jcs-other-window-next)
  (jcs-bind-key (kbd "M-q") #'jcs-other-window-prev)

  ;; imports/package declaration.
  (jcs-bind-key (kbd "C-S-o") #'jcs-java-organize-imports)

  ;; javadoc
  (jcs-bind-key (kbd "<f2>") #'javadoc-lookup)
  (jcs-bind-key (kbd "S-<f2>") #'javadoc-lookup))

(add-hook 'java-mode-hook 'jcs-java-mode-hook)

(provide 'jcs-java-mode)
;;; jcs-java-mode.el ends here
