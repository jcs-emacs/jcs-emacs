;;; jcs-vimscript-mode.el --- VimScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'vimrc-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-vimscript-template ()
  "Header for Vimscript header file."
  (jcs--file-header--insert "vimscript" "default.txt"))

;;
;; (@* "Hook" )
;;

(defun jcs-vim-mode-hook ()
  "Vimrc mode hook."

  ;; File Header
  (jcs-insert-header-if-valid '("[.]vim"
                                "[.]vimrc"
                                "_vimrc")
                              'jcs-insert-vimscript-template)

  ;; Normal
  (jcs-bind-key (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
  (jcs-bind-key (kbd "<down>") (jcs-get-prev/next-key-type 'next))

  (jcs-bind-key (kbd "C-a") #'jcs-mark-whole-buffer))

(add-hook 'vimrc-mode-hook 'jcs-vim-mode-hook)

(provide 'jcs-vimscript-mode)
;;; jcs-vimscript-mode.el ends here
