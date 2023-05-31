;;; lang/common-lisp/config.el  -*- lexical-binding: t; -*-

(require 'sly-macrostep)
(require 'sly-quicklisp)

;;
;; (@* "Settings" )
;;

(setq inferior-lisp-program (shell-quote-argument (executable-find "sbcl")))

;;
;; (@* "Extensions" )
;;

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))
