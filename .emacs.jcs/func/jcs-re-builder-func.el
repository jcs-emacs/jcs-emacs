;;; jcs-re-builder-func.el --- RE-Builder functions.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-reb-maybe-kill-this-buffer ()
  "Kill this buffer in `re-builder' mode."
  (interactive)
  (let ((is-killed nil))
    ;; maybe kill this buffer.
    (setq is-killed (jcs-maybe-kill-this-buffer))
    (when is-killed
      ;; then delete this window.
      (delete-window))))

;;;###autoload
(defun jcs-re-builder (type)
  "Rewrap `re-builder' function.
TYPE : enable/disable case sensitive?"
  (interactive
   (list (completing-read
          "Enable case sensitive?" '("Case Sensitive"
                                     "Case Insensitive"))))

  (if (string= type "Case Sensitive")
      (setq case-fold-search nil)
    (setq case-fold-search t))  ;; This is default.

  ;; Start `RE-Builder' mode.
  (re-builder)

  ;; Set back to default.
  (setq case-fold-search t))


(provide 'jcs-re-builder-func)
;;; jcs-re-builder-func.el ends here
