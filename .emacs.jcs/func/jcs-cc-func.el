;;; jcs-cc-func.el --- C/C++ related.  -*- lexical-binding: t -*-
;;; Commentary: Functions for C/C++ common.
;;; Code:

;;;###autoload
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)
  (cond ((equal major-mode 'c-mode) (progn (c++-mode)))
        ((equal major-mode 'c++-mode) (progn (c-mode)))))

;;;###autoload
(defun jcs-toggle-c-comment-style ()
  "Toggle comment style between /* */ and //."
  (interactive)
  (when (or (jcs-is-current-major-mode-p "c-mode")
            (jcs-is-current-major-mode-p "c++-mode"))
    (if (string= comment-start "// ")
        (setq comment-start "/*"
              comment-start-skip "/\\*+[ \t]*"
              comment-end "*/"
              comment-end-skip "[ \t]*\\*+/")
      (setq comment-start "// "
            comment-end ""))))

(provide 'jcs-cc-func)
;;; jcs-cc-func.el ends here
