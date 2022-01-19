;;; jcs-erlang-mode.el --- Erlang mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'erlang)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-erlang-template ()
  "Template for Erlang Lisp."
  (jcs--file-header--insert "erlang" "default.txt"))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'erlang-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]erl"
                                "[.]hrl")
                              'jcs-insert-erlang-template))

(provide 'jcs-erlang-mode)
;;; jcs-erlang-mode.el ends here
