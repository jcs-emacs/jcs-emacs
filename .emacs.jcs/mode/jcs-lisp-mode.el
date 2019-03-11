;;; jcs-lisp-mode.el --- Lisp mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-lisp-mode-hook ()
  "Lisp mode hook."

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

  (defun jcs-lisp-format ()
    "Format the given file as a Lisp file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-lisp-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]lisp" buffer-file-name) (jcs-lisp-format))
          ))

  )
(add-hook 'lisp-mode-hook 'jcs-lisp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.lisp'?\\'" . lisp-mode))


(provide 'jcs-lisp-mode)
;;; jcs-lisp-mode.el ends here
