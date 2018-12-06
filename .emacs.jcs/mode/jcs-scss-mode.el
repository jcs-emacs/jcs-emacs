;; ========================================================================
;; $File: jcs-scss-mode.el $
;; $Date: 2017-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh SCSS mode.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(require 'scss-mode)
(defun jcs-scss-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (defun jcs-scss-file-format ()
    "Format the given file as a SCSS file."
    (when (jcs-is-current-file-empty-p)
      (jcs-insert-scss-template)))

  (when buffer-file-name
    (cond ((file-exists-p buffer-file-name) t)
          ((string-match "[.]scss" buffer-file-name) (jcs-scss-file-format))
          ))

  ;; jcs SCSS key binding
  (define-key scss-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key scss-mode-map "\C-c\C-c" #'kill-ring-save)

  ;; Save
  (define-key css-mode-map "\C-s" #'jcs-css-save-buffer)

  ;; comment block
  (define-key scss-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key scss-mode-map (kbd "*") #'jcs-c-comment-pair)

  ;; sort attribute in order => `css-sort' package.
  (define-key scss-mode-map "\C-ks" #'jcs-css-sort-attributes)
  (define-key scss-mode-map "\C-kd" #'jcs-css-sort-attributes-document)
  )
(add-hook 'scss-mode-hook 'jcs-scss-mode-hook)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . scss-mode))
