;;; lang/common-lisp/config.el  -*- lexical-binding: t; -*-

(require 'common-lisp-snippets)

(require 'sly-asdf)
(require 'sly-macrostep)
(require 'sly-quicklisp)

;;
;; (@* "Settings" )
;;

(msg-clean-add-echo-commands '( sly-message
                                sly-overlay-eval-defun))

(elenv-when-exec "sbcl" nil
  (setq inferior-lisp-program (shell-quote-argument value)))

(jcs-add-hook 'sly-mrepl-mode-hook
  (jcs-key-local
    `(((kbd "M-<up>")   . sly-mrepl-previous-input-or-button)
      ((kbd "M-<down>") . sly-mrepl-next-input-or-button)
      ((kbd "M-K")      . sly-mrepl-clear-repl))))

;;
;; (@* "Extensions" )
;;

(use-package sly
  :init
  (eval-mark-add-after-commands '( sly-eval-buffer sly-eval-defun sly-eval-region
                                   sly-eval-last-expression
                                   sly-overlay-eval-defun)))

(use-package sly-repl-ansi-color
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-asdf
  :init
  (add-to-list 'sly-contribs 'sly-asdf))

(use-package sideline-sly
  :hook (sideline-mode . sideline-sly-setup))
