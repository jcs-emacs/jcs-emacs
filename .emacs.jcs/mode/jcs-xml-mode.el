;;; jcs-xml-mode.el --- XML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'nxml-mode)


(defun jcs-xml-format ()
  "Format the given file as a XML file."
  (when (jcs-is-current-file-empty-p)
    (jcs-insert-xml-template)))


(defun jcs-xml-mode-hook ()
  "XML mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]xml" buffer-file-name) (jcs-xml-format))
          ))

  ;; Normal
  (define-key emmet-mode-keymap (kbd "C-c C-c") #'kill-ring-save)

  (define-key nxml-mode-map (kbd "<up>") #'jcs-smart-indent-up)
  (define-key nxml-mode-map (kbd "<down>") #'jcs-smart-indent-down)
  )
;; STUDY: they ae using nxml-mode instead of xml-mode
;; which is really weird.
(add-hook 'nxml-mode-hook 'jcs-xml-mode-hook)
(add-hook 'nxml-mode-hook 'emmet-mode)


(provide 'jcs-xml-mode)
;;; jcs-xml-mode.el ends here
