;;; jcs-yaml-mode.el --- YAML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-yaml-mode-hook ()
  "YAML mode hook."

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

  (defun jcs-yaml-script-format ()
    "Format the given file as a YAML script."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-yaml-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]yaml" buffer-file-name) (jcs-yaml-script-format))
          ((string-match "[.]yml" buffer-file-name) (jcs-yaml-script-format))
          ))

  ;; jcs YAML key binding
  (define-key yaml-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key yaml-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key yaml-mode-map (kbd "<backspace>") #'jcs-yaml-electric-backspace)

  (define-key yaml-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key yaml-mode-map (kbd "<down>") #'jcs-py-indent-down)
  )
(add-hook 'yaml-mode-hook 'jcs-yaml-mode-hook)

(add-to-list 'auto-mode-alist '("\\.yaml'?\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml'?\\'" . yaml-mode))


(provide 'jcs-yaml-mode)
;;; jcs-yaml-mode.el ends here
