;;; lang/racket/config.el  -*- lexical-binding: t; -*-

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

;;
;; (@* "Extensions" )
;;

(use-package racket-mode
  :init
  (setq racket-show-functions '( sideline-racket-show)))

(use-package flymake-racket
  :hook ((scheme-mode . flymake-racket-add-hook)
         (racket-mode . flymake-racket-add-hook)))

(use-package sideline-racket
  :init
  (setq sideline-backends-right '(sideline-racket)))
