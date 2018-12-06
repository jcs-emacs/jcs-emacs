;; ========================================================================
;; $File: jcs-lisp-mode.el $
;; $Date: 2018-10-12 16:05:24 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================



(defun jcs-lisp-mode-hook ()
  "JayCeS Lisp hook."

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
