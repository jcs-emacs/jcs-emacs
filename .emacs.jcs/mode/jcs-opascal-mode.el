;;; jcs-opascal-mode.el --- Object Pascal mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'opascal)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-opascal-template ()
  "Header for Object Pascal header file."
  (jcs--file-header--insert "opascal" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-opascal-mode-hook ()
  "Object Pascal mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]dpk"
                                "[.]dpr")
                              'jcs-insert-opascal-template)

  ;; Normal
  (jcs-bind-key (kbd "M-q") #'jcs-other-window-prev))

(add-hook 'opascal-mode-hook 'jcs-opascal-mode-hook)

(provide 'jcs-opascal-mode)
;;; jcs-opascal-mode.el ends here
