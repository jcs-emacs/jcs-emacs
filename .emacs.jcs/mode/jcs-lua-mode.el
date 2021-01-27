;;; jcs-lua-mode.el --- Lua mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'lua-mode)

(defun jcs-lua--electric-pair-inhibit-predicate (c)
  "Electric pair predicate for `lua-mode'."
  (if (jcs-current-char-equal-p "[")
      (electric-pair-default-inhibit c)
    (jcs--electric-pair-inhibit-predicate c)))

(defun jcs-lua-return ()
  "Return key for `lua-mode'."
  (interactive)
  (let (able-insert-docstring-p)
    (if (not (jcs-inside-comment-block-p)) (newline-and-indent)
      (setq able-insert-docstring-p
            (and (save-excursion (search-backward "--[[" (line-beginning-position) t))
                 (save-excursion (search-forward "]]" (line-end-position) t))))
      (newline-and-indent)
      (when able-insert-docstring-p
        (end-of-line))
      (unless (string= "--[[" (jcs-start-comment-symbol))
        (insert "-- ")))))

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
                              'jcs-insert-lua-template)

  ;; comment block
  (define-key lua-mode-map (kbd "RET") #'jcs-lua-return))

(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)

(provide 'jcs-lua-mode)
;;; jcs-lua-mode.el ends here
