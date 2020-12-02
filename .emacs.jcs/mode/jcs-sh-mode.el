;;; jcs-sh-mode.el --- Shell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sh-script)

;;
;; (@* "Hook" )
;;

(defun jcs-sh-script-hook()
  "Shell Script mode hook."

  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sh"
                                "[.]linux"
                                "[.]macosx")
                              'jcs-insert-sh-template))

(add-hook 'sh-mode-hook 'jcs-sh-script-hook)

(provide 'jcs-sh-mode)
;;; jcs-sh-mode.el ends here
