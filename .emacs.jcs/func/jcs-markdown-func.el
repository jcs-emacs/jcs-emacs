;;; jcs-markdown-func.el --- Markdown related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-markdown-return-key ()
  "Return key for Markdown mode."
  (interactive)
  (let ((did-ret-key nil) (close-tag-found nil))
    (when (and (jcs-first-forward-char-in-line-p "<")
               (jcs-first-backward-char-in-line-p ">"))
      ;; Check closing tag.
      (save-excursion
        (jcs-move-to-forward-a-char "<")
        (forward-char 1)
        (setq close-tag-found (jcs-current-char-equal-p "/")))

      (when close-tag-found
        (newline-and-indent)
        (newline-and-indent)
        (jcs-smart-indent-up)
        (setq did-ret-key t)))

    (unless did-ret-key
      (newline)
      (indent-for-tab-command))))


(provide 'jcs-markdown-func)
;;; jcs-markdown-func.el ends here
