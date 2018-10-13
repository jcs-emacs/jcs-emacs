;; ========================================================================
;; $File: jcs-batch-mode.el $
;; $Date: 2017-07-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Batch mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'bat-mode)
(defun jcs-batch-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; turn on auto complete.
  (auto-complete-mode t)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-batch-script-format ()
    "Format the given file as a Batch file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-batch-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]bat" buffer-file-name) (jcs-batch-script-format))
        )

  ;; Set Faces.
  (jcs-init-set-face)

  ;; jcs key binding
  (define-key bat-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key bat-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key bat-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key bat-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'bat-mode-hook 'jcs-batch-mode-hook)

(add-to-list 'auto-mode-alist '("\\.bat?\\'" . bat-mode))
