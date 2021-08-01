;;; jcs-file.el --- File handle  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Find files" )
;;

;;;###autoload
(defun jcs-counsel-find-files-other-window ()
  "Find files on other window."
  (interactive)
  (let ((buf (current-buffer)) found-file target-buf)
    (unwind-protect (setq found-file (counsel-find-file))
      (when found-file
        (setq target-buf found-file)
        (switch-to-buffer buf)
        (find-file-other-window target-buf)))))

;;;###autoload
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

;;;###autoload
(defun jcs-html-preview (&optional filepath title not-ow)
  "Preview html FILEPATH other window with TITLE.
NOT-OW : Default is other window, not other window."
  (interactive)
  (require 'f)
  (let ((buf-str "") (default-directory default-directory))
    (if filepath
        (progn
          (setq buf-str (jcs-get-string-from-file filepath))
          (setq default-directory (f-dirname filepath)))
      (setq buf-str (buffer-string)))
    (unless title (setq title (format "*html-preview - %s*" (buffer-name))))
    (jcs-switch-to-buffer title (not not-ow))
    (read-only-mode -1)
    (erase-buffer)
    (save-excursion
      (insert buf-str)
      ;; NOTE: Start swapping html:src url path.
      (goto-char (point-min))
      (while (not (jcs-is-end-of-buffer-p))
        (jcs-move-to-forward-a-word "src")
        (unless (jcs-is-end-of-buffer-p)
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
  (read-only-mode -1)
  (erase-buffer)
  (save-excursion
    (if (file-exists-p filepath)
        (insert (jcs-get-string-from-file filepath))
      (insert (format "Missing table file: '%s'" filepath))))
  (read-only-mode 1)
  (special-mode))

;;
;; (@* "Project" )
;;

(defun jcs-select-find-file-current-dir (in-filename in-title)
  "Find IN-FILENAME in current directory.

Argument IN-FILENAME accept regular expression string.

Argument IN-TITLE is a string used when there are more than one matches."
  (require 'f)
  (let* ((target-files
          (jcs-f-files-ignore-directories default-directory
                                          (lambda (file)
                                            (string-match-p in-filename (f-filename file)))))
         (target-files-len (length target-files)) (target-filepath ""))
    (when (zerop target-files-len)
      (user-error "[ERROR] No '%s' file found in the current directory" in-filename))
    (if (= target-files-len 1)
        ;; If only one file found, just get that file.
        (nth 0 target-files)
      ;; Get the selected file.
      (completing-read in-title target-files))))

(defun jcs-select-find-file-in-project (in-filename in-title)
  "Find IN-FILENAME in current project.

Argument IN-FILENAME accept regular expression string.

Argument IN-TITLE is a string used when there are more than one matches."
  (require 'f)
  (let ((project-dir (jcs-project-current)) target-files target-files-len)
    ;; Do the find file only when the project directory exists.
    (when project-dir
      (setq target-files
            (jcs-f-files-ignore-directories project-dir
                                            (lambda (file)
                                              (string-match-p in-filename (f-filename file)))
                                            t)))
    (when target-files (setq target-files-len (length target-files)))
    (unless target-files-len
      (user-error (concat
                   "[ERROR] No '%s' file found in the project, make sure "
                   "the project directory exists")
                  in-filename))
    (if (= target-files-len 1)
        ;; If only one file found, just get that file.
        (nth 0 target-files)
      ;; Get the selected file.
      (completing-read in-title target-files))))

(defun jcs-find-file-in-project-and-current-dir (in-filename in-title)
  "Find the file from project root, if not found find it in current directory.
Return full path if found, else error prompt.  IN-FILENAME to search in project
or current directory.  IN-TITLE search uses regexp, meaning it could found
multiple files at a time.  We need a title to present which file to select."
  (let ((filepath
         (or (ignore-errors (jcs-select-find-file-current-dir in-filename in-title))
             (ignore-errors (jcs-select-find-file-in-project in-filename in-title)))))
    (unless filepath
      (user-error
       (concat "Can't find '%s' file either in the project or current "
               "directory, make sure the project directory exists or "
               "the '%s' file exists in the current directory")
       in-filename
       in-filename))
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
  (let ((types '())
        (files (ignore-errors (directory-files path t)))
        fn)
    (dolist (file files)
      (when (jcs-is-directory-p file)
        (setq fn (file-name-nondirectory file))
        (unless (or (string= "." fn) (string= ".." fn))
          (unless full (setq file fn))
          (push file types))))
    (sort types #'string-lessp)))

(defun jcs-dir-to-filename (path &optional ext full with-ext)
  "Return list of filename by PATH.

Optional argument EXT is the extension filter.

If optional argument FULL is non-nil; return full path.
If optional argument WITH-EXT is non-nil; return path with extension."
  (let ((types '()) fn
        (files (ignore-errors
                 (directory-files path t (if ext (format "\\%s$" ext) nil)))))
    (dolist (file files)
      (when (jcs-is-file-p file)
        (setq fn (file-name-nondirectory file))
        (unless (or (string= "." fn) (string= ".." fn))
          (unless full (setq file fn))
          (unless with-ext (setq file (file-name-sans-extension file)))
          (push file types))))
    (sort types #'string-lessp)))

;;;###autoload
(defun jcs-select-file ()
  "Select the file and return that path."
  (interactive)
  (let ((ivy-inhibit-action t)) (counsel-find-file)))

;;;###autoload
(defun jcs-path-info-at-point ()
  "Return the current path info at point."
  (interactive)
  (require 'f)
  (require 'ffap)
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

(defun jcs-f-directories-ignore-directories (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (let ((dirs (f-directories path)) (valid-dirs '()) (final-dirs '()))
    (dolist (dir dirs)
      (unless (jcs-contain-list-string project-vc-ignores (f-filename (f-slash dir)))
        (push dir valid-dirs)))
    (when rec
      (dolist (dir valid-dirs)
        (push (jcs-f-directories-ignore-directories dir rec) final-dirs)))
    (setq valid-dirs (reverse valid-dirs)
          final-dirs (reverse final-dirs))
    (jcs-flatten-list (append valid-dirs final-dirs))))

(defun jcs-f-files-ignore-directories (path &optional fn rec)
  "Find all files in PATH by ignored common directories with FN and REC."
  (let ((dirs (append (list path) (jcs-f-directories-ignore-directories path rec)))
        (files '()))
    (dolist (dir dirs) (push (f-files dir fn) files))
    (jcs-flatten-list (reverse files))))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; IMPORTANT: Keep core function at the top of this file.
;;
;;   * `jcs-find-corresponding-file'
;;   * `jcs-find-corresponding-file-other-window'
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-find-corresponding-file (&optional ow)
  "Find the file that corresponds to this one.
OW : Open file other window."
  (interactive)
  (require 'f)
  (let (corresponding-file-name found-fp)
    ;; NOTE: Add your corresponding file here.
    (cond ((jcs-is-current-major-mode-p
            '("c-mode"
              "c++-mode"))
           (setq corresponding-file-name (jcs-cc-corresponding-file)))
          ((jcs-is-current-major-mode-p
            '("objc-mode"))
           (setq corresponding-file-name (jcs-objc-corresponding-file)))
          ((jcs-is-current-major-mode-p
            '("csharp-mode"  ; For ASP.NET -> [file-name].aspx.cs
              "web-mode"))   ; For ASP.NET -> [file-name].aspx
           (setq corresponding-file-name (jcs-web-corresponding-file))))

    ;; Error check before return it value.
    (if corresponding-file-name
        (progn
          (setq found-fp
                (jcs-find-file-in-project-and-current-dir corresponding-file-name
                                                          "Corresponding file: "))
          (if ow (jcs-find-file-other-window found-fp) (find-file found-fp)))
      (user-error "[WARNING] Unable to find a corresponding file"))))

;;;###autoload
(defun jcs-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (jcs-find-corresponding-file t))

;;----------------------------------------------------------------------------
;; C/C++

(defun jcs-cc-corresponding-file ()
  "Find the corresponding file for C/C++ file."
  (let ((tmp-base-file-name (f-filename (file-name-sans-extension (buffer-name))))
        corresponding-file-name)
    (cond ((string-match "\\.hin" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".cin")))
          ((string-match "\\.hpp" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".cpp")))
          ((string-match "\\.h" buffer-file-name)
           (if (file-exists-p (concat tmp-base-file-name ".c"))
               (setq corresponding-file-name (concat tmp-base-file-name ".c"))
             (setq corresponding-file-name (concat tmp-base-file-name ".cpp"))))
          ((string-match "\\.cin" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".hin")))
          ((string-match "\\.cpp" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".h")))
          ((string-match "\\.c" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
    corresponding-file-name))

;;----------------------------------------------------------------------------
;; Objective-C

(defun jcs-objc-corresponding-file ()
  "Find the corresponding file for Objective-C related file."
  (let ((tmp-base-file-name (file-name-sans-extension buffer-file-name))
        corresponding-file-name)
    (cond ((string-match "\\.m" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
    ;; If Objective-C corresponding file not found, use C/C++ corresponding
    ;; file instead.
    (when (string-empty-p corresponding-file-name)
      (setq corresponding-file-name (jcs-cc-corresponding-file)))
    ;; Return file name.
    corresponding-file-name))

;;----------------------------------------------------------------------------
;; Web Related

(defun jcs-web-corresponding-file ()
  "Find the corresponding file for WEB related file."
  (let ((tmp-base-file-name (file-name-sans-extension buffer-file-name))
        corresponding-file-name)
    (cond ((string-match "\\.aspx.cs" buffer-file-name)
           (setq corresponding-file-name tmp-base-file-name))
          ((string-match "\\.aspx" buffer-file-name)
           (setq corresponding-file-name (concat tmp-base-file-name ".aspx.cs"))))
    ;; NOTE: If is ASP.NET, just open the current file itself.
    (when (string-empty-p corresponding-file-name)
      (setq corresponding-file-name buffer-file-name))
    ;; Return file name.
    corresponding-file-name))

(provide 'jcs-file)
;;; jcs-file.el ends here
