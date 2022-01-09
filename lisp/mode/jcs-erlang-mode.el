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
                              'jcs-insert-erlang-template)

  (jcs-key-local
    `(((kbd "M-w") . jcs-other-window-next)
      ((kbd "M-q") . jcs-other-window-prev))))

(provide 'jcs-erlang-mode)
;;; jcs-erlang-mode.el ends here
