;;; jcs-python-mode.el --- Python mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'python-mode)
(defun jcs-python-mode-hook ()
  "Python mode hook."
  (electric-pair-mode t)
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; NOTE(jenchieh): Default is set to 8 for some reason.
  (setq-local tab-width 4)

  (font-lock-add-keywords
   nil
   '(;; TODO(jenchieh): This face would not apply cuz this conflict to the
     ;; oop missing modes.
     ;;("\\(\"\"\"[^\"]*\"\"\"\\)" 1 'jcs-py-mode-docstring-face t)

     ;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
     ;; NOTE(jenchieh): Fixed comment and string conflict.
     ("[^\"]\\(#[^\"\r\n]*\\)[^\"]" 1 'jcs-font-lock-comment-face t)
     ("[^\"]\\(\"[^\"]*\"\\)[^\"]" 1 'jcs-font-lock-string-face t)
     ;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     )'end)


  (defun jcs-python-class-format ()
    "Format the given file as a Python file."
    (when (jcs-is-current-file-empty-p)
      (call-interactively #'jcs-ask-python-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]py" buffer-file-name) (jcs-python-class-format))
          ))

  ;; Normal
  (define-key python-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key python-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key python-mode-map [C-backspace] #'jcs-backward-delete-word)

  (define-key python-mode-map [M-up] #'jcs-previous-blank-line)
  (define-key python-mode-map [M-down] #'jcs-next-blank-line)

  (define-key python-mode-map "\C-k\C-f" #'jcs-py-indent-region)
  (define-key python-mode-map "\C-k\C-d" #'jcs-py-format-document)
  (define-key python-mode-map (kbd "C-S-f") #'jcs-py-format-region-or-document)

  ;; Edit
  (define-key python-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key python-mode-map (kbd "<down>") #'jcs-py-indent-down)
  (define-key python-mode-map (kbd "SPC") #'jcs-py-space)
  (define-key python-mode-map (kbd "S-SPC") #'jcs-py-real-space)
  (define-key python-mode-map (kbd "<backspace>") #'jcs-py-backspace)
  (define-key python-mode-map (kbd "S-<backspace>") #'jcs-py-real-backspace)
  (define-key python-mode-map (kbd "TAB") #'jcs-tab-key)

  (define-key python-mode-map (kbd "RET") #'jcs-py-return)

  ;; Comment
  (define-key python-mode-map (kbd "\"") #'jcs-py-maybe-insert-codedoc)
  )
(add-hook 'python-mode-hook 'jcs-python-mode-hook)


(provide 'jcs-python-mode)
;;; jcs-python-mode.el ends here
