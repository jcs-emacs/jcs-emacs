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

(jcs-add-hook 'vimrc-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]vim"
                                "[.]vimrc"
                                "_vimrc")
                              'jcs-insert-vimscript-template)

  (jcs-key-local
    `(((kbd "<up>")   . ,(jcs-get-prev/next-key-type 'previous))
      ((kbd "<down>") . ,(jcs-get-prev/next-key-type 'next))
      ((kbd "C-a")    . jcs-mark-whole-buffer))))

(provide 'jcs-vimscript-mode)
;;; jcs-vimscript-mode.el ends here
