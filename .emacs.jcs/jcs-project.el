;;; jcs-project.el --- Project related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'project)

(defun jcs-project-remember (&optional dir)
  "Remeber the project from DIR.

If optional argument DIR is nil, use variable `default-directory' instead."
  (unless dir (setq dir default-directory))
  (ignore-errors (project-remember-project (project--find-in-directory dir))))

(defun jcs-project-list-clean ()
  "Clean up the project list if the project no longer exists."
  (project--ensure-read-project-list)
  (let (pr-lst)
    (dolist (pr project--list)
      (when (jcs-file-directory-exists-p (nth 0 pr)) (push pr pr-lst)))
    (setq project--list (reverse pr-lst))))

(provide 'jcs-project)
;;; jcs-project.el ends here
