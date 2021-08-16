;;; jcs-project.el --- Project related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'project)

;;
;; (@* "Util" )
;;

(defun jcs-project-current ()
  "Return project directory path."
  (cdr (project-current)))

(defun jcs-project-under-p ()
  "Return non-nil if current file is under a project."
  (and (jcs-project-current)
       (ignore-errors (file-readable-p (buffer-file-name)))))

(defvar jcs-project--cache-opened-projects nil
  "Cache for opened projects.")

(defun jcs-project--record-open-projects ()
  ""
  (setq jcs-project--cache-opened-projects (jcs-project-opened-projects)))

(defun jcs-project-opened-projects (&optional refresh)
  "Return a list of active projects.

If UNIQUIFY is non-nil, refresh the cache once."
  (when (or refresh (not jcs-project--cache-opened-projects))
    (require 'cl-lib)
    (require 'subr-x)
    (require 'f)
    (let (project-lst)
      (dolist (buf (jcs-valid-buffer-list))
        (with-current-buffer buf
          (when-let* ((default-directory (f-parent (buffer-file-name buf)))
                      (name (jcs-project-current)))
            (push name project-lst))))
      (setq jcs-project--cache-opened-projects (delete-dups project-lst))))
  jcs-project--cache-opened-projects)

(defun jcs-project-current-uniquify (&optional buffer)
  "Return a shorten uniquify name from BUFFER."
  (require 'subr-x)
  (require 'f)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (when-let ((default-directory (buffer-file-name buffer))
               (all-projects (jcs-project-opened-projects))
               (current-project (jcs-project-current)))
      (push current-project all-projects)
      (setq all-projects (delete-dups all-projects))
      (nth 0 (f-uniquify all-projects)))))

;;
;; (@* "Core" )
;;

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
    (setq project--list (reverse pr-lst))
    (project--write-project-list)))

(defun jcs-project-remove (dir)
  "Remove project by project DIR."
  (let (pr-lst)
    (dolist (pr project--list) (unless (string= dir (nth 0 pr)) (push pr pr-lst)))
    (setq project--list (reverse pr-lst))
    (project--write-project-list)))

(provide 'jcs-project)
;;; jcs-project.el ends here
