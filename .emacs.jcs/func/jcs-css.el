;;; jcs-css.el --- CSS related  -*- lexical-binding: t -*-
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
(defun jcs-css-save-buffer ()
  "Save buffer in `css-mode'."
  (interactive)
  ;; NOTE: after using this, I think is better if I bind this function/command
  ;; to another key.
  ;;(com-css-sort-attributes-document)
  (jcs-untabify-save-buffer))

(provide 'jcs-css)
;;; jcs-css.el ends here
