;; ========================================================================
;; $File: jcs-go-mode.el $
;; $Date: 2017-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh GO mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'go-mode)
(defun jcs-go-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-go-script-format ()
    "Format the given file as a GO file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-go-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]go" buffer-file-name) (jcs-go-script-format))
          ))

  ;; jcs Lua key binding
  (define-key go-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key go-mode-map "\C-c\C-c" #'kill-ring-save)

  )
(add-hook 'go-mode-hook 'jcs-go-mode-hook)

(add-to-list 'auto-mode-alist '("\\.go?\\'" . go-mode))
