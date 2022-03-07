;;; jcs-project.el --- Project related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Util" )
;;

(defun jcs-project-current ()
  "Return project directory path."
  (nth 0 (last (project-current))))

(defun jcs-project-under-p ()
  "Return non-nil if current file is under a project."
  (and (project-current)
       (ignore-errors (file-readable-p (buffer-file-name)))))

(defvar jcs-project--cache-opened-projects nil
  "Cache to track down list of opened projects.")

(defun jcs-project--track-open-projects ()
  "Track the opened projects once."
  (setq jcs-project--cache-opened-projects (jcs-project-opened-projects)))

(defun jcs-project-opened-projects (&optional refresh)
  "Return a list of active projects.

If UNIQUIFY is non-nil, refresh the cache once."
  (when (or refresh (not jcs-project--cache-opened-projects))
    (jcs-require '(subr-x f))
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
  (jcs-require '(subr-x f))
  (with-current-buffer (or buffer (current-buffer))
    (when-let ((default-directory (buffer-file-name))
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
  (ignore-errors (project-remember-project (project--find-in-directory (or dir default-directory)))))

;;
;; (@* "Version Control" )
;;

(defun jcs-vc-info ()
  "Return vc-mode information."
  (format-mode-line '(vc-mode vc-mode)))

(defun jcs-vc-status ()
  "Return version control status."
  (jcs-require '(subr-x f))
  (when-let* ((project-name (jcs-project-current))
              (info (jcs-vc-info))
              (split (split-string info ":"))
              (name (string-trim (jcs-s-replace-displayable (nth 0 split))))
              (branch (string-trim (jcs-s-replace-displayable (nth 1 split)))))
    (list (f-base project-name) name branch)))

(defun jcs-vc-project ()
  "Return the project name."
  (nth 0 (jcs-vc-status)))

(defun jcs-vc-system ()
  "Return the system name."
  (nth 1 (jcs-vc-status)))

(defun jcs-vc-branch ()
  "Return the branch name."
  (nth 2 (jcs-vc-status)))

(provide 'jcs-project)
;;; jcs-project.el ends here
