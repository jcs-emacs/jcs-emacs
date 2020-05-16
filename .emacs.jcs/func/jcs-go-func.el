;;; jcs-go-func.el --- Go related.  -*- lexical-binding: t -*-
;;; Commentary: When editing the Go related file.
;;; Code:

;;;###autoload
(defun jcs-go-maybe-insert-codedoc ()
  "Insert common Go document/comment string."
  (interactive)
  (insert "/")
  (let ((active-comment nil) (next-line-not-empty nil))
    (save-excursion
      (backward-char 1)
      (when (jcs-current-char-equal-p "/")
        (backward-char 1)
        (when (jcs-is-beginning-of-line-p)
          (setq active-comment t)))
      (jcs-next-line)
      (unless (jcs-current-line-empty-p) (setq next-line-not-empty t)))

    (when (and active-comment next-line-not-empty)
      (jcs-insert-comment-style-by-current-line "[{]")
      (end-of-line))))

(provide 'jcs-go-func)
;;; jcs-go-func.el ends here
