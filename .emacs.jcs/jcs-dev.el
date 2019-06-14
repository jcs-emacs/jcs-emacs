;;; jcs-dev.el --- Development related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-compile (in-op)
  "Compile command rewrapper.
IN-OP : inpuit operation script."
  ;; NOTE: First we need to get the script directory. In order
  ;; to change execute/workspace directory to the current target script's
  ;; directory path.
  (let ((script-dir (f-dirname in-op)))
    ;; NOTE: Change the current execute/workspace directory
    ;; to the script directory temporary. So the script will execute
    ;; within the current directory the script is currently in.
    ;;
    ;; Without these lines of code, the script will execute in the
    ;; `default-directory' variables. The `default-directory' variables
    ;; will be the directory path where you start the Emacs. For instance,
    ;; if you start Emacs at path `/usr/home', then the default directory
    ;; will be at `usr/home' directory.
    ;;
    ;; Adding these lines of code if your scirpt is at `/usr/home/project/some-script.sh',
    ;; Then your `default-directory' became `usr/home/project'. Hurray!
    (let ((default-directory script-dir))
      ;; Compile/Execute the target script.
      (compile in-op)
      (jcs-update-line-number-each-window))))


;;;###autoload
(defun jcs-make-without-asking ()
  "Make the current build."
  (interactive)
  (let ((makescript-path "")
        (target-script jcs-makescript))
    ;; Get the file.
    (setq makescript-path (jcs-find-file-in-project-and-current-dir target-script
                                                                    "Makescript: "))
    ;; Do the compile
    (jcs-compile makescript-path)))

;;;###autoload
(defun jcs-run-without-asking ()
  "Run the current build program. - JenChieh
IN-FILE : selected file."
  (interactive)
  (let ((runscript-path "")
        (target-script jcs-runscript))
    ;; Get the file.
    (setq runscript-path (jcs-find-file-in-project-and-current-dir target-script
                                                                   "Runscript: "))
    ;; Do the compile
    (jcs-compile runscript-path)))

;;;###autoload
(defun jcs-open-project-todo-file ()
  "Open the TODO list from this project. - JenChieh"
  (interactive)
  (let ((project-todo-file-path "")
        (target-script jcs-project-todo-file))
    ;; Get the file.
    (setq project-todo-file-path (jcs-find-file-in-project-and-current-dir target-script
                                                                           "TODO file: "))
    ;; Open the file. [Default other window]
    (find-file-other-window project-todo-file-path)))

;;;###autoload
(defun jcs-open-project-update-log-file ()
  "Open the Update Log from this project. - JenChieh"
  (interactive)
  (let ((project-update-log-file-path "")
        (target-script jcs-project-update-log-file))
    ;; Get the file.
    (setq project-update-log-file-path (jcs-find-file-in-project-and-current-dir target-script
                                                                                 "Update Log file: "))
    ;; Open the file. [Default other window]
    (find-file-other-window project-update-log-file-path)))


(provide 'jcs-dev)
;;; jcs-dev.el ends here
