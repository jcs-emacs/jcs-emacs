;; ========================================================================
;; $File: jcs-haskell-mode.el $
;; $Date: 2018-06-20 00:42:05 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'haskell-mode)
(defun jcs-haskell-mode-hook ()
  "Haskell mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)


  (defun jcs-haskell-script-format ()
    "Format the given file as a class. - JenChieh Haskell Script"
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-haskell-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hs" buffer-file-name) (jcs-haskell-script-format))
        )


  )
(add-hook 'haskell-mode-hook 'jcs-haskell-mode-hook)

(add-to-list 'auto-mode-alist '("\\.hs?\\'" . haskell-mode))
