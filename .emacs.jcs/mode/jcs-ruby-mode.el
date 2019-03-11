;;; jcs-ruby-mode.el --- Ruby mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-ruby-mode-hook ()
  "Ruby mode hook."

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

  (defun jcs-ruby-script-format ()
    "Format the given file as a Ruby script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-ruby-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]rb" buffer-file-name) (jcs-ruby-script-format))
          ))

  )
(add-hook 'ruby-mode-hook 'jcs-ruby-mode-hook)

(add-to-list 'auto-mode-alist '("\\.rb'?\\'" . ruby-mode))


(provide 'jcs-ruby-mode)
;;; jcs-ruby-mode.el ends here
