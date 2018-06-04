;; ========================================================================
;; $File: jcs-dev.el $
;; $Date: 2018-06-04 20:02:35 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Development related stuff put here..
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


(defun jcs-select-find-file-in-project (in-filename in-title)
  "Find the file in the project.
Return the absolute filepath.
IN-FILENAME : file name to find.
IN-TITLE : title for `completing-read' function call."
  (let ((target-files '())
        (project-source-dir (concat (cdr (project-current)))))
    ;; Do the find file.
    (setq target-files (f--files project-source-dir (string-match-p in-filename (f-filename it)) t))

    (let ((target-files-len (length target-files))
          (target-filepath ""))
      (if (= target-files-len 0)
          (error (format "No '%s' file found in the project, make sure the project directory exists"
                         in-filename))
        (if (= target-files-len 1)
            ;; If only one file found, just get that file.
            (setq target-filepath (nth 0 target-files))
          (progn
            ;; Get the selected file.
            (setq target-filepath (completing-read
                                   in-title target-files)))))
      target-filepath)))

;;;###autoload
(defun jcs-make-without-asking ()
  "Make the current build."
  (interactive)
  (let ((makescript-path ""))
    ;; Get the file.
    (setq makescript-path (jcs-select-find-file-in-project jcs-makescript
                                                           "Makescript: "))
    ;; Do the compile
    (compile makescript-path)))

;;;###autoload
(defun jcs-run-without-asking ()
  "Run the current build program. - JenChieh
IN-FILE : selected file."
  (interactive)
  (let ((runscript-path ""))
    ;; Get the file.
    (setq runscript-path (jcs-select-find-file-in-project jcs-runscript
                                                          "Runscript: "))
    ;; Do the compile
    (compile runscript-path)))

;;;###autoload
(defun jcs-open-project-todo-file ()
  "Open the TODO list from this project. - JenChieh"
  (interactive)
  (let ((project-todo-file-path ""))
    ;; Get the file.
    (setq project-todo-file-path (jcs-select-find-file-in-project jcs-project-todo-file
                                                                  "TODO file: "))
    (find-file project-todo-file-path)))

;;;###autoload
(defun jcs-open-project-update-log-file ()
  "Open the Update Log from this project. - JenChieh"
  (interactive)
  (let ((project-update-log-file-path ""))
    ;; Get the file.
    (setq project-update-log-file-path (jcs-select-find-file-in-project jcs-project-update-log-file
                                                                        "Update Log file: "))
    (find-file project-update-log-file-path)))
