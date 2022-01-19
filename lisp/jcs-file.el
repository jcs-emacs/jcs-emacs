;;; jcs-file.el --- File handle  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Find files" )
;;

(defun jcs-project-find-file-other-window ()
  "Find files in project on other window."
  (interactive)
  (let ((buf (current-buffer)) found-file target-buf)
    (unwind-protect (setq found-file (project-find-file))
      (when found-file
        (setq target-buf (concat (cdr (project-current)) (buffer-name found-file)))
        (switch-to-buffer buf)
        (find-file-other-window target-buf)))))

;;
;; (@* "Display" )
;;

(defun jcs-html-preview (&optional filepath title not-ow)
  "Preview html FILEPATH other window with TITLE.
NOT-OW : Default is other window, not other window."
  (interactive)
  (require 'f)
  (let ((buf-str "") (default-directory default-directory))
    (if filepath
        (setq buf-str (jcs-get-string-from-file filepath)
              default-directory (f-dirname filepath))
      (setq buf-str (buffer-string)))
    (unless title (setq title (format "*html-preview - %s*" (buffer-name))))
    (jcs-switch-to-buffer title (not not-ow))
    (read-only-mode -1)
    (erase-buffer)
    (save-excursion
      (insert buf-str)
      ;; NOTE: Start swapping html:src url path.
      (goto-char (point-min))
      (while (not (eobp))
        (jcs-move-to-forward-a-word "src")
        (unless (eobp)
          (forward-char 2)
          (let ((start-ch (jcs-get-current-char-string))
                (start-pt (point)) (end-pt -1)
                (url-path "")
                (relative-ulr-path t))
            (save-excursion
              (when (jcs-current-word-equal-p '("http" "https" "file"))
                (setq relative-ulr-path nil)))
            (when relative-ulr-path
              (jcs-move-to-forward-a-char start-ch)
              (forward-char -1)
              (setq end-pt (point))

              (setq url-path (buffer-substring start-pt end-pt))
              (delete-region start-pt end-pt)

              (setq url-path (format "file:///%s" (expand-file-name url-path)))
              (insert url-path)))))
      (shr-render-region (point-min) (point-max)))
    (read-only-mode 1)
    (special-mode)))

(defun jcs-display-file (filepath title &optional not-ow)
  "Display a file with FILEPATH with TITLE.
NOT-OW : Default is other window, not other window."
  (jcs-switch-to-buffer title (not not-ow))
  (let (buffer-read-only)
    (erase-buffer)
    (save-excursion
      (if (file-exists-p filepath)
          (insert (jcs-get-string-from-file filepath))
        (insert (format "Missing table file: '%s'" filepath)))))
  (special-mode))

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
        ;; If only one file found, just get that file.
        (nth 0 target-files)
      ;; Get the selected file.
      (completing-read title target-files))))

(defun jcs-select-find-file-in-project (filename title)
  "Find FILENAME in current project.

Argument FILENAME accept regular expression string.

Argument TITLE is a string used when there are more than one matches."
  (require 'f)
  (let ((project-dir (jcs-project-current)) target-files target-files-len)
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
        ;; If only one file found, just get that file.
        (nth 0 target-files)
      ;; Get the selected file.
      (completing-read title target-files))))

(defun jcs-find-file-in-project-and-current-dir (filename title)
  "Find the file from project root, if not found find it in current directory.
Return full path if found, else error prompt.  FILENAME to search in project
or current directory.  TITLE search uses regexp, meaning it could found
multiple files at a time.  We need a title to present which file to select."
  (let ((filepath
         (or (ignore-errors (jcs-select-find-file-current-dir filename title))
             (ignore-errors (jcs-select-find-file-in-project filename title)))))
    (unless filepath
      (user-error
       (concat "[ERROR] Can't find file '%s' in the project or current directory "
               ", make sure the project root exists or the '%s' file exists in the "
               "current directory")
       filename
       filename))
    filepath))

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

(defun jcs-path-info-at-point ()
  "Return the current path info at point."
  (interactive)
  (jcs-require '(ffap f))
  (let ((path (ffap-string-at-point)) content name d-or-f exists (timeout 300))
    (unless (string-match-p (ffap-file-at-point) path)
      (setq path nil))
    (setq exists (jcs-file-directory-exists-p path))
    (when path
      (setq name (f-filename path))
      (cond ((f-file-p path) (setq d-or-f "file"))
            ((f-dir-p path) (setq d-or-f "directory"))
            (t (setq d-or-f "unknown")))
      (setq content
            (format "%s\n%s\n%s\n%s"
                    (format "[NAME] %s" name)
                    (format "[PATH] %s" (expand-file-name path))
                    (format "[EXISTENCE] %s" exists)
                    (if exists (format "[TYPE] %s" d-or-f) "")))
      (jcs-pop-tooltip content :point (point) :timeout timeout))))

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

(defun jcs-find-corresponding-file (&optional ow)
  "Find the file that corresponds to this one.
If OW is non-nil, open it in other window"
  (interactive)
  (jcs-require '(subr-x f))
  (if-let* ((corresponding-file-name
             ;; NOTE: Add your corresponding file here.
             (cl-case major-mode
               ((or c-mode c++-mode) (jcs-cc-corresponding-file))
               (`objc-mode (jcs-objc-corresponding-file))
               ((or csharp-mode  ; For ASP.NET -> [file-name].aspx.cs
                    web-mode)    ; For ASP.NET -> [file-name].aspx
                (jcs-web-corresponding-file))))
            (found-fp (jcs-find-file-in-project-and-current-dir
                       corresponding-file-name "Corresponding file: "))
            (fnc (if ow #'jcs-find-file-other-window #'find-file)))
      (funcall fnc found-fp)
    (user-error "[WARNING] Unable to find a corresponding file")))

(defun jcs-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (jcs-find-corresponding-file t))

;;; C/C++

(defun jcs-cc-corresponding-file ()
  "Find the corresponding file for C/C++ file."
  (let ((name (f-filename (file-name-sans-extension (buffer-name))))
        (ext (file-name-extension (buffer-name))))
    (concat
     name "."
     (pcase ext
       ("hin" "cin")
       ("hpp" "cpp")
       ("h" (if (file-exists-p (concat name ".c")) "c" "cpp"))
       ("cin" "hin")
       ((or "cpp" "c") "h")))))

;;; Objective-C

(defun jcs-objc-corresponding-file ()
  "Find the corresponding file for Objective-C related file."
  (let ((name (file-name-sans-extension buffer-file-name))
        (ext (file-name-extension (buffer-name))))
    (concat
     name "."
     (pcase ext
       ("m" "h")
       (_ (jcs-cc-corresponding-file))))))

;;; Web

(defun jcs-web-corresponding-file ()
  "Find the corresponding file for WEB related file."
  (let ((name (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.aspx.cs" buffer-file-name) name)
          ((string-match "\\.aspx" buffer-file-name) (concat name ".aspx.cs"))
          ;; NOTE: If is ASP.NET, just open the current file itself
          (t buffer-file-name))))

(provide 'jcs-file)
;;; jcs-file.el ends here
