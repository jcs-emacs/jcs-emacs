;; ========================================================================
;; $File: jcs-lua-mode.el $
;; $Date: 2017-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


(require 'lua-mode)
(defun jcs-lua-mode-hook ()
  "Lau mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")

  (defun jcs-lua-script-format ()
    "Format the given file as a class. - JenChieh Lua Script"
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-lua-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]lua" buffer-file-name) (jcs-lua-script-format))
        ((string-match "[.]luac" buffer-file-name) (jcs-lua-script-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs Lua key binding
  (define-key lua-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key lua-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; Comment
  (define-key lua-mode-map (kbd "-") #'jcs-lua-maybe-insert-codedoc)

  ;; comment block
  (define-key lua-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  )
(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)

(add-to-list 'auto-mode-alist '("\\.lua?\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luac?\\'" . lua-mode))
