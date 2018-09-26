;; ========================================================================
;; $File: jcs-actionscript-mode.el $
;; $Date: 2017-07-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh ActionScript 3.0 mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'actionscript-mode)
(defun jcs-actionscript-mode-hook ()
  "JayCeS AS mode."

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; TOPIC(jenchieh): Treat underscore as word.
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (modify-syntax-entry ?_ "w")


  (defun jcs-actionsript-class-format ()
    "Format the given file as a class. - JenChieh AS class"
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-actionscript-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]as" buffer-file-name) (jcs-actionsript-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; jcs java key binding
  (define-key actionscript-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key actionscript-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key actionscript-mode-map (kbd "DEL") #'jcs-delete-backward-char)
  (define-key actionscript-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key actionscript-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key actionscript-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key actionscript-mode-map (kbd "*") #'jcs-c-comment-pair)
  )

(add-hook 'actionscript-mode-hook 'jcs-actionscript-mode-hook)

(add-to-list 'auto-mode-alist '("\\.as?\\'" . actionscript-mode))
