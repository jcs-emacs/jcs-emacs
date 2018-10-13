;; ========================================================================
;; $File: jcs-scala-mode.el $
;; $Date: 2018-02-03 12:59:49 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Scala mode.
;; URL(jenchieh): https://www.emacswiki.org/emacs/ScalaMode
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'scala-mode)
(defun jcs-scala-mode-hook ()
  "JayCeS Scala mode."

  ;; highlight URL and clickable.
  (goto-address-mode 1)


  (defun jcs-scala-class-format ()
    "Format the given file as a Scala file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-scala-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]scala" buffer-file-name) (jcs-scala-class-format))
        )

  ;; Set Faces.
  (jcs-init-set-face)

  ;; Scala key bindings
  ;; comment block
  (define-key scala-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key scala-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'scala-mode-hook 'jcs-scala-mode-hook)

(add-to-list 'auto-mode-alist '("\\.scala'?\\'" . scala-mode))
