;;; jcs-lua-mode.el --- Lua mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'lua-mode)

(defun jcs-lua--electric-pair-inhibit-predicate (c)
  "Electric pair predicate for `lua-mode'."
  (if (jcs-current-char-equal-p "[")
      (electric-pair-default-inhibit c)
    (jcs--electric-pair-inhibit-predicate c)))

;;
;; (@* "Hook" )
;;

(defun jcs-lua-mode-hook ()
  "Lau mode hook."

  (jcs-make-electric-pair-pairs-local '((?\[ . ?\])))

  (setq-local electric-pair-inhibit-predicate 'jcs-lua--electric-pair-inhibit-predicate)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]lua" "[.]luac")
                              'jcs-insert-lua-template))

(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)

(provide 'jcs-lua-mode)
;;; jcs-lua-mode.el ends here
