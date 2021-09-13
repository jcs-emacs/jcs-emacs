;;; jcs-make-mode.el --- Makefile mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'make-mode)

;;
;; (@* "Hook" )
;;

(defun jcs-makefile-mode-hook ()
  "Makefile mode hook."
  (electric-pair-mode nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]makefile"
                                "[Mm]akefile"
                                "[.]mak")
                              'jcs-makefile-format-info)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))
  (jcs-bind-key (kbd "RET") #'jcs-makefile-newline)
  (jcs-bind-key (kbd "C-v") #'yank))

(add-hook 'makefile-mode-hook 'jcs-makefile-mode-hook)

(provide 'jcs-make-mode)
;;; jcs-make-mode.el ends here
