;;; jcs-ivy-func.el --- Ivy function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-counsel-find-files-slash ()
  "Find files slash key."
  (interactive)
  (unless (jcs-current-char-equal-p "/")
    (insert "/")))

;;;###autoload
(defun jcs-counsel-find-files-backspace ()
  "Find files backspace key."
  (interactive)
  (if (jcs-current-char-equal-p "/")
      (counsel-up-directory)
    (backward-delete-char 1)))

;;;###autoload
(defun jcs-counsel-find-files-enter ()
  "Find files enter key."
  (interactive)
  (unless (counsel-down-directory)
    (ivy-done)))

;;;###autoload
(defun jcs-counsel-find-files-other-window ()
  "Find files on other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (setq found-file (counsel-find-file))
    (unless found-file
      (select-window starting-window))))

;;;###autoload
(defun jcs-counsel-projectile-find-file-other-window ()
  "Find files in project on other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (setq found-file (counsel-projectile-find-file))
    (unless found-file
      (select-window starting-window))))


(provide 'jcs-ivy-func)
;;; jcs-ivy-func.el ends here
