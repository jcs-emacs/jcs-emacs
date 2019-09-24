;;; jcs-dev.el --- Development related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; Eval Elisp

(defun jcs--eval-region--advice-after (&rest _)
  "Advice execute after `eval-region' function."
  (deactivate-mark))
(advice-add 'eval-region :after #'jcs--eval-region--advice-after)

;;----------------------------------------------------------------------------
;; Build & Run

;;;###autoload
(defun jcs-open-project-file (in-filename title &optional ow)
  "Open the IN-FILENAME from this project with TITLE.
OW : Opened it in other window."
  (interactive)
  (let ((filepath
         (jcs-find-file-in-project-and-current-dir in-filename
                                                   title)))
    (if ow
        (find-file-other-window filepath)
      (find-file filepath))))

;;;###autoload
(defun jcs-compile-project-file (in-filename title)
  "Compile IN-FILENAME from the project"
  (interactive)
  (let ((filepath
         (jcs-find-file-in-project-and-current-dir in-filename
                                                   title)))
    (jcs-compile filepath)))


(defun jcs-compile (in-op)
  "Compile command rewrapper.
IN-OP : inpuit operation script."
  (require 'f)
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
  (jcs-compile-project-file jcs-makescript "Make script: "))

;;;###autoload
(defun jcs-run-without-asking ()
  "Run the current build program."
  (interactive)
  (jcs-compile-project-file jcs-runscript "Run script: "))

;;;###autoload
(defun jcs-open-project-todo-file ()
  "Open the TODO list from this project."
  (interactive)
  (jcs-open-project-file jcs-project-todo-file "TODO file: " t))

;;;###autoload
(defun jcs-open-project-update-log-file ()
  "Open the Update Log from this project."
  (interactive)
  (jcs-open-project-file jcs-project-update-log-file "Update Log file: " t))


(provide 'jcs-dev)
;;; jcs-dev.el ends here
