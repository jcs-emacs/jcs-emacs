;;; jcs-lisp-mode.el --- Lisp mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-lisp-format ()
  "Format the given file as a Lisp file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-lisp-template)))

(defun jcs-lisp-mode-hook ()
  "Lisp mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (jcs-make-electric-pair-pairs-local '((?\` . ?\')))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]lisp" buffer-file-name) (jcs-lisp-format))
          ))

  )
(add-hook 'lisp-mode-hook 'jcs-lisp-mode-hook)


(provide 'jcs-lisp-mode)
;;; jcs-lisp-mode.el ends here
