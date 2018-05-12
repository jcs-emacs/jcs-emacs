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

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-jayces-class-format ()
    "Format the given file. - JenChieh JayCeS files"
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-jayces-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]jcs" buffer-file-name) (jcs-jayces-class-format))
        ((string-match "[.]jayces" buffer-file-name) (jcs-jayces-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)
  )
(add-hook 'jayces-mode-hook 'jcs-jayces-mode-hook)

(add-to-list 'auto-mode-alist '("\\.jcs?\\'" . jayces-mode))
(add-to-list 'auto-mode-alist '("\\.jayces?\\'" . jayces-mode))
