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

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; IMPORTANT: Keep core function at the top of this file.
;;
;;   * `jcs-find-corresponding-file'
;;   * `jcs-find-corresponding-file-other-window'
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun jcs-file--corresponding-filename ()
  "Return corresponding file."
  (let ((name (f-filename (file-name-sans-extension (buffer-name))))
        (ext (file-name-extension (buffer-name)))
        (fnc (cl-case major-mode
               ((or c-mode c++-mode) #'jcs-cc-corresponding-file)
               (`objc-mode #'jcs-objc-corresponding-file)
               ((or csharp-mode  ; For ASP.NET -> [file-name].aspx.cs
                    web-mode)    ; For ASP.NET -> [file-name].aspx
                #'jcs-web-corresponding-file))))
    (when fnc (funcall fnc name ext))))

(defun jcs-find-corresponding-file (&optional ow)
  "Find the file that corresponds to this one.
If OW is non-nil, open it in other window"
  (interactive)
  (jcs-require '(subr-x f))
  (if-let* ((corresponding-name (jcs-file--corresponding-filename))
            (found-fp (jcs-find-file-in-project-and-current-dir
                       corresponding-name "Corresponding file: "))
            (fnc (if ow #'find-file-other-window #'find-file)))
      (funcall fnc found-fp)
    (user-error "[WARNING] Unable to find a corresponding file")))

(defun jcs-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive) (jcs-find-corresponding-file t))

;;; C/C++

(defun jcs-cc-corresponding-file (name ext)
  "Find the corresponding file for C/C++ file."
  (concat
   name "."
   (pcase ext
     ("hin" "cin")
     ("hpp" "cpp")
     ("h" (if (file-exists-p (concat name ".c")) "c" "cpp"))
     ("cin" "hin")
     ((or "cpp" "c") "h"))
   "$"))

;;; Objective-C

(defun jcs-objc-corresponding-file (name ext)
  "Find the corresponding file for Objective-C related file."
  (concat
   name "."
   (pcase ext
     ("m" "h")
     (_ (jcs-cc-corresponding-file)))
   "$"))

;;; Web

(defun jcs-web-corresponding-file (name &rest _)
  "Find the corresponding file for WEB related file."
  (concat
   (cond ((string-match "\\.aspx.cs" buffer-file-name) name)
         ((string-match "\\.aspx" buffer-file-name) (concat name ".aspx.cs"))
         ;; NOTE: If is ASP.NET, just open the current file itself
         (t buffer-file-name))
   "$"))

(provide 'jcs-file)
;;; jcs-file.el ends here
