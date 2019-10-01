;;; jcs-erlang-mode.el --- Erlang Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'erlang)


(defun jcs-erlang-mode-hook ()
  "Erlang mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]erl"
                                "[.]hrl")
                              'jcs-insert-erlang-template)

  ;; switch window
  (define-key erlang-mode-map (kbd "M-w") #'jcs-other-window-next)
  (define-key erlang-mode-map (kbd "M-q") #'jcs-other-window-prev)
  )
(add-hook 'erlang-mode-hook 'jcs-erlang-mode-hook)


(provide 'jcs-erlang-mode)
;;; jcs-erlang-mode.el ends here
