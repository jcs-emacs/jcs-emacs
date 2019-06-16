;;; jcs-java-mode.el --- Java mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'javadoc-lookup)
(require 'organize-imports-java)


(defun jcs-java-class-format ()
  "Format the given file as a Java file."
  (when (jcs-is-current-file-empty-p)
    ;; insert the package declaration.
    (jcs-java-insert-package-from-src)

    ;; Leave one empty line between header.
    (insert "\n")

    (jcs-insert-java-template)))


(defun jcs-java-mode-hook ()
  "Java mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (face-remap-add-relative 'font-lock-constant-face '((:foreground "#D2D2D2")))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]java" buffer-file-name) (jcs-java-class-format))
          ))

  ;; Normal
  (define-key java-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key java-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key java-mode-map (kbd "C-s") #'jcs-java-untabify-save-buffer)

  (define-key java-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key java-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key java-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key java-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key java-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; switch frame.
  (define-key java-mode-map "\ew" #'jcs-other-window-next)
  (define-key java-mode-map (kbd "M-q") #'jcs-other-window-prev)

  ;; imports/package declaration.
  (define-key java-mode-map (kbd "C-S-o") #'jcs-java-organize-imports)

  ;; javadoc
  (define-key java-mode-map (kbd "<f2>") #'javadoc-lookup)
  (define-key java-mode-map (kbd "S-<f2>") #'javadoc-lookup)
  )
(add-hook 'java-mode-hook 'jcs-java-mode-hook)


(use-package javadoc-lookup
  :config
  ;; Function used when performing a minibuffer read.
  (setq javadoc-lookup-completing-read-function #'completing-read))


(provide 'jcs-java-mode)
;;; jcs-java-mode.el ends here
