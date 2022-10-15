;;; jcs-vimscript-mode.el --- VimScript mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'vimrc-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-vimscript-template "vimscript" "default.txt"
  "Header for Vimscript header file.")

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
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line)
      ((kbd "C-a")    . mark-whole-buffer))))

(provide 'jcs-vimscript-mode)
;;; jcs-vimscript-mode.el ends here
