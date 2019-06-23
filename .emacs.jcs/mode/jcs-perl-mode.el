;;; jcs-perl-mode.el --- Perl mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'perl-mode)


(defun jcs-perl-mode-hook ()
  "Perl mode hook."
  (abbrev-mode 1)
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (modify-syntax-entry ?_ "w")

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]pl" buffer-file-name)
           (jcs-insert-header-if-empty 'jcs-insert-perl-template))
          ))

  ;; Normal
  (define-key perl-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key perl-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key perl-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key perl-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key perl-mode-map (kbd ";") #'jcs-vs-semicolon-key)
  )
(add-hook 'perl-mode-hook 'jcs-perl-mode-hook)


(provide 'jcs-perl-mode)
;;; jcs-perl-mode.el ends here
