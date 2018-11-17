;; ========================================================================
;; $File: jcs-rust-mode.el $
;; $Date: 2018-11-17 20:23:17 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'rust-mode)
(defun jcs-rust-mode-hook ()
  "Rust mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-rust-script-format ()
    "Format the given file as a Rust script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-rust-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]rs" buffer-file-name) (jcs-rust-script-format))
        )
  )
(add-hook 'rust-mode-hook 'jcs-rust-mode-hook)

(add-to-list 'auto-mode-alist '("\\.rs'?\\'" . rust-mode))
