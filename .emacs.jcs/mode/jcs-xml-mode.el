;;; jcs-xml-mode.el --- XML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-xml-mode-hook ()

  (interactive)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for XML here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-xml-format ()
    "Format the given file as a XML file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-xml-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]xml" buffer-file-name) (jcs-xml-format))
          ))

  )
;; NOTE(jenchieh): they ae using nxml-mode instead of xml-mode
;; which is really weird.
(add-hook 'nxml-mode-hook 'jcs-xml-mode-hook)
(add-hook 'nxml-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.xml?\\'" . xml-mode))


(provide 'jcs-xml-mode)
;;; jcs-xml-mode.el ends here
