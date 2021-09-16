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
  "Cache to track down list of opened projects.")

(defun jcs-project--track-open-projects ()
  "Track the opened projects once."
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
  (require 'project)
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

;;
;; (@* "Files" )
;;

(defun jcs-project-buffer-list ()
  "Return a list of buffers within the current project."
  (let ((project-name (jcs-project-current-uniquify)) buffers)
    (when project-name
      (dolist (buffer (buffer-list))
        (when (equal (jcs-project-current-uniquify buffer) project-name)
          (push buffer buffers))))
    (reverse buffers)))

;;
;; (@* "Version Control" )
;;

(defun jcs-vc-info ()
  "Return vc-mode information."
  (format-mode-line '(vc-mode vc-mode)))

(defun jcs-vc-status ()
  "Return version control status."
  (require 'subr-x) (require 'f)
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

(defun jcs-vc-dir-p (dir-path)
  "Return non-nil if DIR-PATH is version control directory."
  (let (tmp-is-vc-dir)
    (dolist (tmp-vc-type grep-find-ignored-directories)
      (let ((tmp-check-dir (concat dir-path "/" tmp-vc-type)))
        (when (jcs-file-directory-exists-p tmp-check-dir)
          (setq tmp-is-vc-dir t))))
    tmp-is-vc-dir))

(provide 'jcs-project)
;;; jcs-project.el ends here
