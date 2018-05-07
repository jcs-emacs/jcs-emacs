;; ========================================================================
;; $File: jcs-cs-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh C# mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'csharp-mode)
(defun jcs-csharp-mode-hook ()

  (preproc-font-lock-mode t)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-csharp-class-format ()
    "Format the given file as a class. - JenChieh C# class"
    (when (is-current-file-empty-p)
      (jcs-insert-cs-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]cs" buffer-file-name) (jcs-csharp-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs C# key binding
  (define-key csharp-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key csharp-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; comment block
  (define-key csharp-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key csharp-mode-map (kbd "*") 'jcs-c-comment-pair)

  (define-key csharp-mode-map (kbd "/") 'jcs-vs-csharp-maybe-insert-codedoc)

  (define-key csharp-mode-map "\eq" 'jcs-other-window-prev)
  )
(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cs?\\'" . csharp-mode))
