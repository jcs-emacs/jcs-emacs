;;; jcs-css-func.el --- CSS related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun jcs-css-smart-indent-up ()
  "CSS smart indent up."
  (interactive)
  (jcs-previous-line)
  (let (deactivate-mark) (save-excursion (indent-for-tab-command)))
  (when (jcs-is-infront-first-char-at-line-p) (jcs-goto-first-char-in-line))
  (when (jcs-current-line-empty-p) (end-of-line)))

;;;###autoload
(defun jcs-css-smart-indent-down ()
  "CSS smart indent down."
  (interactive)
  (jcs-next-line)
  (let (deactivate-mark) (save-excursion (indent-for-tab-command)))
  (when (jcs-is-infront-first-char-at-line-p) (jcs-goto-first-char-in-line))
  (when (jcs-current-line-empty-p) (end-of-line)))

;;;###autoload
(defun jcs-css-return-key ()
  "CSS return key."
  (interactive)
  (if (jcs-is-end-of-line-p)
      (call-interactively #'jcs-smart-context-line-break)
    (newline-and-indent)
    (jcs-beginning-of-line)
    (when (jcs-is-infront-first-char-at-line-p) (indent-for-tab-command))))

;;;###autoload
(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  ;; NOTE: after using this, I think is better if I bind this function/command
  ;; to another key.
  ;;(com-css-sort-attributes-document)
  (jcs-untabify-save-buffer))

;;----------------------------------------------------------------------------

(defun jcs-init-css-faces ()
  "CSS Faces Highlighting."
  (let ((missing-modes '(css-mode
                         less-css-mode
                         sass-mode
                         scss-mode
                         ssass-mode)))
    (dolist (mode missing-modes)
      (font-lock-add-keywords
       mode
       '(("[ \t]*\\([#][a-zA-Z0-9_-]*\\)[ \t\n]*[(\[*:>+~,{]" 1 'jcs-css-id-face t)
         ("[ \t]*\\([.][a-zA-Z0-9_-]*\\)[ \t\n]*[(\[*:>+~,{]" 1 'jcs-css-class-face t)
         ("\\([:][a-zA-Z0-9>+~:_-]+\\)[ \t\n]*[,{]" 1 'jcs-css-event-face t)
         ;; Number
         ("[ \t;,)]*\\([+-]*[0-9]*[.]*[0-9]+[a-z%]*\\)[ \t\n;,)]" . 'jcs-css-number-face)
         ;; Variable
         ("[$@][^ \t\n:;]+" . 'jcs-css-variable-face))
       'end)))
  (jcs--set-common-face 'css-selector (face-foreground 'jcs-css-selector-face))
  (jcs--set-common-face 'font-lock-variable-name-face (face-foreground 'jcs-css-variable-face))
  (jcs--set-common-face 'css-property (face-foreground 'jcs-css-type-face)))

(provide 'jcs-css-func)
;;; jcs-css-func.el ends here
