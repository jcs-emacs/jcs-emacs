;;; jcs-file.el --- File handle.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun jcs-html-preview (&optional filepath title not-ow)
  "Preview html FILEPATH other window with TITLE.
NOT-OW : Default is other window, not other window."
  (interactive)
  (let ((buf-str "")
        (default-directory default-directory))
    (if filepath
        (progn
          (setq buf-str (jcs-get-string-from-file filepath))
          (setq default-directory (f-dirname filepath)))
      (setq buf-str (buffer-string)))
    (unless title (setq title (format "*html-preview - %s*" (buffer-name))))
    (if not-ow (switch-to-buffer title) (switch-to-buffer-other-window title))
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
                (start-pt (point))
                (end-pt -1)
                (url-path "")
                (relative-ulr-path t))
            (save-excursion
              (when (or (jcs-current-word-equal-p "http")
                        (jcs-current-word-equal-p "https")
                        (jcs-current-word-equal-p "file"))
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
  (if not-ow (switch-to-buffer title) (switch-to-buffer-other-window title))
  (read-only-mode -1)
  (erase-buffer)
  (save-excursion
    (if (file-exists-p filepath)
        (insert (jcs-get-string-from-file filepath))
      (insert (format "Missing table file: '%s'" filepath))))
  (read-only-mode 1)
  (special-mode))


(defun jcs-select-find-file-current-dir (in-filename)
  "Find the IN-FILENAME in the current directory.  Return the absolute filepath."
  (let ((target-filepath "")
        (current-source-dir default-directory))
    (setq target-filepath (concat current-source-dir in-filename))
    (if (file-exists-p target-filepath)
        target-filepath  ;; Return if the target file exists.
      (error (format "No '%s' file found in the current directory"
                     in-filename)))))

(defun jcs-select-find-file-in-project (in-filename in-title)
  "Find IN-FILENAME in the project with displayed IN-TITLE for `completing-read'.
Version Control directory must exists in order to make it work.
Return the absolute filepath."
  (let ((target-files '())
        (project-source-dir (jcs-vc-root-dir)))
    ;; Do the find file only when the project directory exists.
    (unless (string= project-source-dir "")
      (setq target-files (f--files project-source-dir (string-match-p in-filename (f-filename it)) t)))

    (let ((target-files-len (length target-files))
          (target-filepath ""))
      (if (= target-files-len 0)
          (error (format "No '%s' file found in the project, make sure the project directory exists"
                         in-filename))
        (if (= target-files-len 1)
            ;; If only one file found, just get that file.
            (setq target-filepath (nth 0 target-files))
          ;; Get the selected file.
          (setq target-filepath (completing-read
                                 in-title target-files))))
      target-filepath)))

(defun jcs-find-file-in-project-and-current-dir (in-filename in-title)
  "First find the file from the whole project, if not found find it in the \
current directory then.
Return full path if found, else error prompt.
IN-FILENAME : filename to search in project or current directory.
TITLE : Search uses regexp, meaning it could found multiple files at a time.
We need a title to present which file to select."
  (let ((filepath ""))
    (unless (ignore-errors
              (setq filepath (jcs-select-find-file-in-project in-filename
                                                              in-title)))
      (unless (ignore-errors
                (setq filepath (jcs-select-find-file-current-dir in-filename)))
        (error (format (concat "Cannot find '%s' file either in the project or current "
                               "directory, make sure the project directory exists or "
                               "the '%s' file exists in the current directory")
                       in-filename
                       in-filename))))
    ;; Return the path.
    filepath))


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
  (let ((corresponding-file-name "")
        (found-filepath nil))
    ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    ;; NOTE: Add your corresponding file here.
    ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

    ;; NOTE: Find C/C++ corresponding file.
    (when (or (jcs-is-current-major-mode-p "c++-mode")
              (jcs-is-current-major-mode-p "c-mode"))
      (setq corresponding-file-name (jcs-cc-corresponding-file)))

    ;; NOTE: Find Objective-C corresponding file.
    (when (or (jcs-is-current-major-mode-p "objc-mode"))
      (setq corresponding-file-name (jcs-objc-corresponding-file)))

    ;; NOTE: Find WEB corresponding file.
    (when (or
           ;; For ASP.NET -> [file-name].aspx.cs
           (jcs-is-current-major-mode-p "csharp-mode")
           ;; For ASP.NET -> [file-name].aspx
           (jcs-is-current-major-mode-p "web-mode"))
      (setq corresponding-file-name (jcs-web-corresponding-file)))

    ;; Error check before return it value.
    (if corresponding-file-name
        (progn
          (setq found-filepath
                (jcs-find-file-in-project-and-current-dir corresponding-file-name
                                                          "Corresponding file: "))
          (if ow
              (find-file-other-window found-filepath)
            (find-file found-filepath)))
      (error "Unable to find a corresponding file.."))))

;;;###autoload
(defun jcs-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (jcs-find-corresponding-file t))


;;-----------------------------------------------------------
;; C/C++
;;-----------------------------------------------------------

(defun jcs-cc-corresponding-file ()
  "Find the corresponding file for C/C++ file."
  (let ((corresponding-file-name "")
        (tmp-base-file-name (f-filename (file-name-sans-extension (buffer-name)))))
    (cond ((string-match "\\.hin" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".cin"))))
          ((string-match "\\.hpp" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".cpp"))))
          ((string-match "\\.h" buffer-file-name)
           (progn
             (if (file-exists-p (concat tmp-base-file-name ".c"))
                 (setq corresponding-file-name (concat tmp-base-file-name ".c"))
               (setq corresponding-file-name (concat tmp-base-file-name ".cpp")))))
          ((string-match "\\.cin" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".hin"))))
          ((string-match "\\.cpp" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
          ((string-match "\\.c" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
          )
    ;; Return file name.
    corresponding-file-name))


;;-----------------------------------------------------------
;; Objective-C
;;-----------------------------------------------------------

(defun jcs-objc-corresponding-file ()
  "Find the corresponding file for Objective-C related file."
  (let ((corresponding-file-name "")
        (tmp-base-file-name (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.m" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".h"))))
          )

    ;; If Objective-C corresponding file not found, use C/C++ corresponding
    ;; file instead.
    (when (string= corresponding-file-name "")
      (setq corresponding-file-name (jcs-cc-corresponding-file)))

    ;; Return file name.
    corresponding-file-name))


;;-----------------------------------------------------------
;; Web Related
;;-----------------------------------------------------------

(defun jcs-web-corresponding-file ()
  "Find the corresponding file for WEB related file."
  (let ((corresponding-file-name "")
        (tmp-base-file-name (file-name-sans-extension buffer-file-name)))
    (cond ((string-match "\\.aspx.cs" buffer-file-name)
           (progn
             (setq corresponding-file-name tmp-base-file-name)))
          ((string-match "\\.aspx" buffer-file-name)
           (progn
             (setq corresponding-file-name (concat tmp-base-file-name ".aspx.cs"))))
          )

    ;; NOTE: If is ASP.NET, just open the current file itself.
    (when (string= corresponding-file-name "")
      (setq corresponding-file-name buffer-file-name))

    ;; Return file name.
    corresponding-file-name))


(provide 'jcs-file)
;;; jcs-file.el ends here
