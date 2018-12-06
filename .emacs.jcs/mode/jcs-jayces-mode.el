;; ========================================================================
;; $File: jcs-jayces-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh JayCeS mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(require 'jayces-mode)
(defun jcs-jayces-mode-hook ()
  "JayCeS mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-jayces-class-format ()
    "Format the given file as a JayCeS file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-jayces-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]jcs" buffer-file-name) (jcs-jayces-class-format))
          ((string-match "[.]jayces" buffer-file-name) (jcs-jayces-class-format))
          ))

  )
(add-hook 'jayces-mode-hook 'jcs-jayces-mode-hook)

(add-to-list 'auto-mode-alist '("\\.jcs'?\\'" . jayces-mode))
(add-to-list 'auto-mode-alist '("\\.jayces'?\\'" . jayces-mode))
