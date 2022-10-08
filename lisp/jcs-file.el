;;; jcs-file.el --- File handle  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Find files" )
;;

(defun jcs-project-find-file-other-window ()
  "Find files in project on other window."
  (interactive)
  (jcs-with-other-window (project-find-file)))

;;
;; (@* "Path" )
;;

(defun jcs--path-guess (lst predicate &optional last-result)
  "Guess the path by LST and PREDICATE.
Optional argument LAST-RESULT is the last output result from recursive function."
  (require 'f)
  (let* ((path-current (nth 0 lst)) (path-next (nth 1 lst))
         (path-next-next (nth 2 lst))
         (dirs (if last-result "" (jcs-dir-to-dirname path-current)))
         new-path result)
    (if last-result
        (dolist (path last-result)
          (setq dirs (jcs-dir-to-dirname path))
          (dolist (dirname dirs)
            (when (funcall predicate dirname)
              (setq new-path (f-join (concat path dirname (or path-current "/"))))
              (push new-path result))))
      (pop lst)
      (dolist (dirname dirs)
        (when (funcall predicate dirname)
          (setq new-path (f-join (concat path-current dirname (or path-next "/"))))
          (push new-path result))))
    (when path-next-next
      (pop lst) (setq result (jcs--path-guess lst predicate result)))
    result))

;;
;; (@* "Util" )
;;

(defun jcs-dir-to-dirname (path &optional full)
  "Return list of directory by PATH.

If optional argument FULL is non-nil; return full path."
  (let ((files (ignore-errors (directory-files path t))) types fn)
    (dolist (file files)
      (when (jcs-directory-p file)
        (setq fn (file-name-nondirectory file))
        (unless (member fn '("." ".."))
          (unless full (setq file fn))
          (push file types))))
    (sort types #'string-lessp)))

(defun jcs-dir-to-filename (path &optional ext full with-ext)
  "Return list of filename by PATH.

Optional argument EXT is the extension filter.

If optional argument FULL is non-nil; return full path.
If optional argument WITH-EXT is non-nil; return path with extension."
  (let ((files (ignore-errors
                 (directory-files path t (when ext (format "\\%s$" ext)))))
        types fn)
    (dolist (file files)
      (when (jcs-file-p file)
        (setq fn (file-name-nondirectory file))
        (unless (member fn '("." ".."))
          (unless full (setq file fn))
          (unless with-ext (setq file (file-name-sans-extension file)))
          (push file types))))
    (sort types #'string-lessp)))

(provide 'jcs-file)
;;; jcs-file.el ends here
