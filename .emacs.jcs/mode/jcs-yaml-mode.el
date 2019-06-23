;;; jcs-yaml-mode.el --- YAML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'yaml-mode)

(require 'jcs-python-func)
(require 'jcs-yaml-func)


(defun jcs-yaml-mode-hook ()
  "YAML mode hook."
  (abbrev-mode 1)
  (electric-pair-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((or (string-match "[.]yaml" buffer-file-name)
               (string-match "[.]yml" buffer-file-name))
           (jcs-insert-header-if-empty 'jcs-insert-yaml-template))
          ))

  ;; Normal
  (define-key yaml-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key yaml-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key yaml-mode-map (kbd "<backspace>") #'jcs-yaml-electric-backspace)

  (define-key yaml-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key yaml-mode-map (kbd "<down>") #'jcs-py-indent-down)
  )
(add-hook 'yaml-mode-hook 'jcs-yaml-mode-hook)


(provide 'jcs-yaml-mode)
;;; jcs-yaml-mode.el ends here
