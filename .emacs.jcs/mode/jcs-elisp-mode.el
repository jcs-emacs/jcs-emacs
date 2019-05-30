;;; jcs-elisp-mode.el --- Emacs Lisp Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-emacs-lisp-format ()
  "Format the given file as a Emacs Lisp file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-emacs-lisp-template)))

(defun jcs-emacs-lisp-mode-hook ()
  "Emacs Lisp mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (jcs-make-electric-pair-pairs-local '((?\` . ?\')))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]el" buffer-file-name) (jcs-emacs-lisp-format))
          ))

  )
(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)


(provide 'jcs-elisp-mode)
;;; jcs-elisp-mode.el ends here
