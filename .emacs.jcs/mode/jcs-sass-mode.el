;;; jcs-sass-mode.el --- SASS mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'ssass-mode)
(defun jcs-sass-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-sass-file-format ()
    "Format the given file as a SASS file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-sass-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]sass" buffer-file-name) (jcs-sass-file-format))
          ))

  ;; jcs SASS key binding
  (define-key ssass-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key ssass-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; comment block
  (define-key ssass-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key ssass-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; Edit
  (define-key ssass-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key ssass-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'ssass-mode-hook 'jcs-sass-mode-hook)
(add-to-list 'auto-mode-alist '("\\.sass'?\\'" . ssass-mode))


(provide 'jcs-sass-mode)
;;; jcs-sass-mode.el ends here
