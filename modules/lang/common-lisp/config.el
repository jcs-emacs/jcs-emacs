;;; lang/common-lisp/config.el  -*- lexical-binding: t; -*-

(require 'common-lisp-snippets)

(require 'sly-macrostep)
(require 'sly-quicklisp)

;;
;; (@* "Settings" )
;;

(elenv-when-exec "sbcl" nil
  (setq inferior-lisp-program (shell-quote-argument value)))

;;
;; (@* "Extensions" )
;;

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color)

  (message-clean-mode-add-echo-commands '( sly-message)))
