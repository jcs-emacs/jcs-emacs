;; ========================================================================
;; $File: jcs-perl-mode.el $
;; $Date: 2018-02-04 22:02:36 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Perl mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'perl-mode)
(defun jcs-perl-mode-hook ()
  "JayCeS Perl mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-perl-script-format ()
    "Format the given file as a Perl file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-perl-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]pl" buffer-file-name) (jcs-perl-script-format))
        )

  ;; jcs key binding
  (define-key perl-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key perl-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key perl-mode-map (kbd "DEL") #'jcs-delete-backward-char)
  (define-key perl-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key perl-mode-map (kbd ";") #'jcs-vs-semicolon-key)
  )
(add-hook 'perl-mode-hook 'jcs-perl-mode-hook)

(add-to-list 'auto-mode-alist '("\\.pl'?\\'" . perl-mode))
