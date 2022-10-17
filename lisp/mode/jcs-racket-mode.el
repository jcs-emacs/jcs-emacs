;;; jcs-racket-mode.el --- Racket mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'racket-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-racket-template "racket" "default.txt"
  "Header for Racket header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'racket-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]rkt")
                              'jcs-insert-racket-template)

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))

(provide 'jcs-racket-mode)
;;; jcs-racket-mode.el ends here
