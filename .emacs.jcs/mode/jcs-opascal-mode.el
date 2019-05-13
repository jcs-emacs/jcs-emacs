;;; jcs-opascal-mode.el --- Object Pascal mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'opascal)
(defun jcs-opascal-mode-hook ()
  "Object Pascal mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-opascal-script-format ()
    "Format the given file as an Object Pascal script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-opascal-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]dpk" buffer-file-name) (jcs-opascal-script-format))
          ((string-match "[.]dpr" buffer-file-name) (jcs-opascal-script-format))
          ))

  ;; Normal
  (define-key opascal-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key opascal-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key opascal-mode-map "\eq" #'jcs-other-window-prev)
  )
(add-hook 'opascal-mode-hook 'jcs-opascal-mode-hook)


(provide 'jcs-opascal-mode)
;;; jcs-opascal-mode.el ends here
