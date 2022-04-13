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
;; (@* "Project" )
;;

(defun jcs-select-find-file-current-dir (filename title)
  "Find FILENAME in current directory.

Argument FILENAME accept regular expression string.

Argument TITLE is a string used when there are more than one matches."
  (require 'f)
  (let* ((target-files
          (jcs-f-files-ignored-dir default-directory
                                   (lambda (file)
                                     (string-match-p filename (f-filename file)))))
         (target-files-len (length target-files)))
    (when (zerop target-files-len)
      (user-error "[ERROR] No file '%s' found in the current directory" filename))
    (if (= target-files-len 1)
        (nth 0 target-files)  ; If only one file found, just get that file.
      (completing-read title target-files))))  ; Get the selected file.

(defun jcs-select-find-file-in-project (filename title)
  "Find FILENAME in current project.

Argument FILENAME accept regular expression string.

Argument TITLE is a string used when there are more than one matches."
  (require 'f)
  (let ((project-dir (jcs-project-root)) target-files target-files-len)
    ;; Do the find file only when the project directory exists.
    (when project-dir
      (setq target-files
            (jcs-f-files-ignored-dir project-dir
                                     (lambda (file)
                                       (string-match-p filename (f-filename file)))
                                     t)))
    (when target-files (setq target-files-len (length target-files)))
    (unless target-files-len
      (user-error "[ERROR] No file '%s' found in project, make sure the project root exists" filename))
    (if (= target-files-len 1)
        (nth 0 target-files)  ; If only one file found, just get that file.
      (completing-read title target-files))))  ; Get the selected file.

(defun jcs-find-file-in-project-and-current-dir (filename title)
  "Find the file from project root, if not found find it in current directory.
Return full path if found, else error prompt.  FILENAME to search in project
or current directory.  TITLE search uses regexp, meaning it could found
multiple files at a time.  We need a title to present which file to select."
  (if-let ((filepath
            (or (ignore-errors (jcs-select-find-file-current-dir filename title))
                (ignore-errors (jcs-select-find-file-in-project filename title)))))
      filepath
    (user-error
     (concat "[ERROR] Can't find file '%s' in the project or current directory "
             ", make sure the project root exists or the '%s' file exists in the "
             "current directory")
     filename filename)))

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

(defun jcs-f-directories-ignored-dir (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (require 'dash)
  (let ((dirs (f-directories path)) valid-dirs final-dirs)
    (dolist (dir dirs)
      (unless (member (f-filename (f-slash dir)) project-vc-ignores)
        (push dir valid-dirs)))
    (when rec
      (dolist (dir valid-dirs)
        (push (jcs-f-directories-ignored-dir dir rec) final-dirs)))
    (setq valid-dirs (reverse valid-dirs)
          final-dirs (reverse final-dirs))
    (-flatten (append valid-dirs final-dirs))))

(defun jcs-f-files-ignored-dir (path &optional fn rec)
  "Find all files in PATH by ignored common directories with FN and REC."
  (require 'dash)
  (let ((dirs (append (list path) (jcs-f-directories-ignored-dir path rec)))
        files)
    (dolist (dir dirs)
      (when-let ((fs (ignore-errors (f-files dir fn))))
        (push fs  files)))
    (-flatten (reverse files))))

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
