;;; jcs-ivy-func.el --- Ivy function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'ffap)

(defun jcs--ivy-skip-input-selection-p ()
  "Decide weather to skip the input selection.
Return non-nil, to skip the input selection.
Return nil, to NOT to skip the input selection."
  (let ((do-skip nil))
    (cond ((or (string= ivy-text "") (jcs-is-finding-file-p))
           (setq do-skip t))
          ((and (jcs-is-renaming-p)
                (string= (f-filename (nth 0 ivy--all-candidates)) ivy-text))
           (setq do-skip t)))
    do-skip))

(defun jcs--ivy-previous-line--advice-after (&rest _)
  "Advice execute after `ivy-previous-line' function."
  (when (and (= ivy--index -1) (jcs--ivy-skip-input-selection-p))
    (call-interactively #'ivy-previous-line)))
(advice-add 'ivy-previous-line :after #'jcs--ivy-previous-line--advice-after)

;;;###autoload
(defun jcs-counsel-find-files--slash ()
  "Find files slash key."
  (interactive)
  ;; NOTE: For some reason, slash does something else so override it.
  (insert "/")
  (cond ((save-excursion (ignore-errors (search-backward "///")))
         (ivy--cd (f-root)))
        ((save-excursion (ignore-errors (search-backward "/!/")))
         (if (jcs-project-current)
             (ivy--cd (jcs-project-current))
           (backward-delete-char 2)
           (message "[INFO] Project root not defined, return to current directory")))))

;;;###autoload
(defun jcs-counsel-find-files-backspace ()
  "Find files backspace key."
  (interactive)
  (if (or (jcs-current-char-equal-p "/")
          (jcs-current-line-empty-p))  ; Fix for deep directory tree on newline.
      (counsel-up-directory)
    (backward-delete-char 1)))

;;;###autoload
(defun jcs-counsel-find-files-enter ()
  "Find files enter key."
  (interactive)
  (unless (counsel-down-directory)
    (ivy-done)))

;;;###autoload
(defun jcs-counsel-find-files-other-window ()
  "Find files on other window."
  (interactive)
  (let ((record-dd default-directory)
        (path-at-point (ffap-file-at-point))
        (found-file nil)
        (starting-window (selected-window)))
    (when path-at-point (setq path-at-point (f-dirname path-at-point)))
    (jcs-other-window-next 1 t)
    (unwind-protect
        (setq found-file
              (counsel-find-file (if path-at-point path-at-point record-dd)))
      (unless found-file
        (select-window starting-window)))))

;;;###autoload
(defun jcs-counsel-projectile-find-file-other-window ()
  "Find files in project on other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (let ((default-directory record-dd))
      (unwind-protect (setq found-file (counsel-projectile-find-file))
        (unless found-file
          (select-window starting-window))))))

(provide 'jcs-ivy-func)
;;; jcs-ivy-func.el ends here
