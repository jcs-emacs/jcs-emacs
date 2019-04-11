;;; jcs-css-func.el --- CSS related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-css-smart-indent-up ()
  "CSS smart indent up."
  (interactive)
  (jcs-previous-line)
  (let (deactivate-mark)
    (save-excursion
      (indent-for-tab-command)))

  (when (jcs-is-infront-first-char-at-line-p)
    (jcs-goto-first-char-in-line))

  (when (jcs-current-line-empty-p)
    (end-of-line)))

;;;###autoload
(defun jcs-css-smart-indent-down ()
  "CSS smart indent down."
  (interactive)
  (jcs-next-line)
  (let (deactivate-mark)
    (save-excursion
      (indent-for-tab-command)))

  (when (jcs-is-infront-first-char-at-line-p)
    (jcs-goto-first-char-in-line))

  (when (jcs-current-line-empty-p)
    (end-of-line)))

;;;###autoload
(defun jcs-css-return-key ()
  "CSS return key."
  (interactive)
  (if (jcs-is-end-of-line-p)
      (call-interactively #'jcs-smart-context-line-break)
    (progn
      (newline-and-indent)
      (jcs-beginning-of-line)
      (when (jcs-current-line-empty-p)
        (indent-for-tab-command)))))


;;;###autoload
(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  ;; NOTE(jenchieh): after using this, I think is better
  ;; if I bind this function/command to another key.
  ;;(com-css-sort-attributes-document)
  (jcs-untabify-save-buffer))


(defvar jcs-css-modes '(css-mode)
  "CSS mode we want to add it to highlight the face.")

(defun jcs-init-css-faces ()
  "CSS Faces Highlighting."

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(;; Comment overwrite value face.
             ("\\(/\\*[a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:;&^%$#!~]*\\*/\\)" 1 'jcs-font-lock-comment-face t)
             ("[ \t]*\\([#][a-zA-Z0-9_-]*\\)[ \t\n]*[(\[*:>+~,{]" 1 'jcs-css-id-face t)
             ("[ \t]*\\([.][a-zA-Z0-9_-]*\\)[ \t\n]*[(\[*:>+~,{]" 1 'jcs-css-class-face t)
             ("\\([:][a-zA-Z0-9>+~:_-]*\\)[ \t\n]*[,{]" 1 'jcs-css-event-face t)
             ;; Selector
             ("[ \t\n]*\\([a-z0-9_-]*\\)[ \t\n]*:" 1 'jcs-css-type-face t)
             ("[ \t\n]*:[ \t\n]*\\([a-zA-Z0-9 \n\t-.<>?,*'`@\"=_(){}:&^%$#!~]*\\)[ \t\n]*;" 1 'jcs-css-value-face t)
             ;; Number
             ("\\([+-]*[0-9px]*[.]*[0-9px]*\\)[ \t\n;,)]" 1 'jcs-css-number-face t)
             ;; For multi-lines comment.
             ;; TODO(jenchieh): Only inside the curly bracket.
             ;; TODO(jenchieh): There is bug if `/' is inside the comment space.
             ("\\(/\\*[^/]*\\*/\\)" 1 'jcs-font-lock-comment-face t)
             )'end))
        jcs-css-modes)

  ;; Other faces.
  (setq-local font-lock-function-name-face '(:foreground "#17A0FB"))
  (setq-local font-lock-variable-name-face '(:foreground "#38EFCA")))

(defun jcs-init-web-faces ()
  "Initialize Web mode Faces Highlihgting."
  (face-remap-add-relative 'web-mode-block-string-face '(jcs-font-lock-string-face))
  (face-remap-add-relative 'web-mode-html-attr-value-face '(jcs-web-mode-html-attr-value-face)))


(provide 'jcs-css-func)
;;; jcs-css-func.el ends here
