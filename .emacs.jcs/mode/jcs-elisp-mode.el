;; ========================================================================
;; $File: jcs-elisp-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Emacs Lisp mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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

  ;; NOTE(jenchieh): while loading this will get loading emacs
  ;; error, so simple add the `ignore-errors' function can avoid
  ;; this. Furthermore this will stil work after the first load.
  (ignore-errors
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]el" buffer-file-name) (jcs-emacs-lisp-format))
          )
    )

  ;; Set Faces.
  (jcs-init-set-face)
  )
(add-hook 'emacs-lisp-mode-hook 'jcs-emacs-lisp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.el'?\\'" . emacs-lisp-mode))
