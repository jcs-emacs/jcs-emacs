;; ========================================================================
;; $File: jcs-java-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh Java mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;; Minor mode for Java Development
;; SOURCE: https://github.com/mopemope/meghanada-emacs
(require 'meghanada)

(require 'jdee)
(defun jcs-java-mode-hook ()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;;; `meghanada' Configuration
  ;;(meghanada-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  (defun jcs-java-class-format ()
    "Format the given file as a Java file."
    (when (jcs-is-current-file-empty-p)
      ;; insert the package declaration.
      (jcs-java-insert-package-from-src)

      ;; Leave one empty line between header.
      (insert "\n")

      (jcs-insert-java-template)))

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]java" buffer-file-name) (jcs-java-class-format))
        )

  ;; Set Faces.
  (jcs-oop-init-set-face)

  ;; NOTE(jenchieh): change the face locally to this mode.
  (face-remap-add-relative 'font-lock-constant-face
                           '((:foreground "#D2D2D2")))

  ;; jcs java key binding
  (define-key java-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key java-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key java-mode-map (kbd "C-s") #'jcs-java-untabify-save-buffer)

  (define-key java-mode-map (kbd "DEL") #'jcs-delete-backward-char)
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
(add-to-list 'auto-mode-alist '("\\.java'?\\'" . java-mode))

;;(add-hook 'jdee-mode-hook 'jcs-java-mode-hook)
;;(add-to-list 'auto-mode-alist '("\\.java?\\'" . jdee-mode))

;;(autoload 'jde-mode "~/.emacs.d/elpha/jdee-20160304.536/jdee.el" "JDE mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


(require 'jdee-font-lock)
(if (> emacs-major-version 23)
    (defconst c-doc-face-name 'font-lock-doc-face)
  ;; starting with 24, cc-fonts clobbers this because of some change of order
  ;; of loading
  (eval-after-load
      "cc-fonts"
    '(defconst c-doc-face-name 'font-lock-doc-face)))


(set-face-attribute 'jdee-font-lock-number-face nil
                    :foreground "olive drab")

(set-face-attribute 'jdee-font-lock-constant-face nil
                    :foreground "#D2D2D2")

(require 'javadoc-lookup)
;; Function used when performing a minibuffer read.
(setq javadoc-lookup-completing-read-function #'completing-read)
