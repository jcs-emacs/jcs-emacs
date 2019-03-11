;;; jcs-scala-mode.el --- Scala mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'scala-mode)
(defun jcs-scala-mode-hook ()
  "JayCeS Scala mode."

  ;; highlight URL and clickable.
  (goto-address-mode 1)


  (defun jcs-scala-class-format ()
    "Format the given file as a Scala file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-scala-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]scala" buffer-file-name) (jcs-scala-class-format))
          ))

  ;; Scala key bindings
  ;; comment block
  (define-key scala-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key scala-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'scala-mode-hook 'jcs-scala-mode-hook)

(add-to-list 'auto-mode-alist '("\\.scala'?\\'" . scala-mode))


(provide 'jcs-scala-mode)
;;; jcs-scala-mode.el ends here
