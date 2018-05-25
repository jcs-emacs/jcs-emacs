;; ========================================================================
;; $File: jcs-cbl-mode.el $
;; $Date: 2017-10-26 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh COBOL mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'cobol-mode)
(defun jcs-cobol-mode-hook ()
  ;; enable the stuff you want for COBOL here
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-cobol-format ()
    "Format the given file to COBOL. - JenChieh COBOL."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-cobol-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]cbl" buffer-file-name) (jcs-cobol-format))
        )

  ;; Set Faces.
  (face-remap-add-relative 'font-lock-comment-face '(jcs-font-lock-comment-face))
  (face-remap-add-relative 'font-lock-string-face '(jcs-font-lock-string-face))

  ;; jcs COBOL key binding
  (define-key cobol-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key cobol-mode-map "\C-c\C-c" 'kill-ring-save)

  (define-key cobol-mode-map (kbd "<up>") 'jcs-py-indent-up)
  (define-key cobol-mode-map (kbd "<down>") 'jcs-py-indent-down)
  )
(add-hook 'cobol-mode-hook 'jcs-cobol-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cbl?\\'" . cobol-mode))
