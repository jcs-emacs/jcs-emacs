;;; jcs-nasm-mode.el --- Assembly Language Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'nasm-mode)
(defun jcs-nasm-mode-hook()
  ;;
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-asm-format ()
    "Format the given file as a ASM code."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-asm-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]asm" buffer-file-name) (jcs-asm-format))
          ((string-match "[.]inc" buffer-file-name) (jcs-asm-format))
          ))

  ;; jcs key binding
  (define-key nasm-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key nasm-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key nasm-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key nasm-mode-map (kbd "<down>") #'jcs-py-indent-down)

  ;; Comment
  (define-key nasm-mode-map (kbd "RET") #'jcs-nasm-return)
  (define-key nasm-mode-map (kbd ";") #'jcs-nasm-comment)

  ;; Edit
  (define-key nasm-mode-map (kbd "SPC") #'jcs-py-space)
  (define-key nasm-mode-map (kbd "S-SPC") #'jcs-py-real-space)
  (define-key nasm-mode-map (kbd "<backspace>") #'jcs-py-backspace)
  (define-key nasm-mode-map (kbd "S-<backspace>") #'jcs-py-real-backspace)
  )
(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.asm'?\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.inc'?\\'" . nasm-mode))


(provide 'jcs-nasm-mode)
;;; jcs-nasm-mode.el ends here
