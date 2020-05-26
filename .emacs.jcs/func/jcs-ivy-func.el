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
  (let ((buf (buffer-name)) (found-file nil)
        (target-buf nil))
    (unwind-protect (setq found-file (counsel-find-file))
      (when found-file
        (setq target-buf found-file)
        (switch-to-buffer buf)
        (find-file-other-window target-buf)))))

;;;###autoload
(defun jcs-counsel-projectile-find-file-other-window ()
  "Find files in project on other window."
  (interactive)
  (let ((buf (buffer-name)) (found-file nil)
        (target-buf nil))
    (unwind-protect (setq found-file (counsel-projectile-find-file))
      (when found-file
        (setq target-buf (concat (projectile-project-root) found-file))
        (switch-to-buffer buf)
        (find-file-other-window target-buf)))))

(provide 'jcs-ivy-func)
;;; jcs-ivy-func.el ends here
