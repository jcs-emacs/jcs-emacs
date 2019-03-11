;;; jcs-elisp-mode.el --- Emacs Lisp Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun jcs-emacs-lisp-mode-hook ()
  "JayCeS Emacs Lisp hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Emacs Lisp here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-emacs-lisp-format ()
    "Format the given file as a Emacs Lisp file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-elisp-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]el" buffer-file-name) (jcs-emacs-lisp-format))
          ))

  )
(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.el'?\\'" . emacs-lisp-mode))


(provide 'jcs-elisp-mode)
;;; jcs-elisp-mode.el ends here
