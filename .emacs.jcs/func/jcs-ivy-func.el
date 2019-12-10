;;; jcs-ivy-func.el --- Ivy function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-ivy-immediate-done ()
  "Exit the minibuffer return empty input."
  (interactive)
  (delete-minibuffer-contents)
  (setf (ivy-state-current ivy-last)
        (cond ((or (not ivy--directory)
                   (eq (ivy-state-history ivy-last) 'grep-files-history))
               ivy-text)
              ((and (string= ivy-text "")
                    (eq (ivy-state-collection ivy-last)
                        #'read-file-name-internal))
               (if (ivy-state-def ivy-last)
                   (if (and
                        (file-exists-p (ivy-state-def ivy-last))
                        (/= (length ivy--directory)
                            (1+ (length (expand-file-name (ivy-state-def ivy-last))))))
                       ivy--directory
                     (copy-sequence (ivy-state-def ivy-last)))
                 ivy--directory))
              (t
               (expand-file-name ivy-text ivy--directory))))
  ;; NOTE: This is the only line I changed. Not sure why this is blocking
  ;; the user return the empty input.
  ;;(insert (ivy-state-current ivy-last))
  (setq ivy-completion-beg ivy-completion-end)
  (setq ivy-exit 'done)
  (exit-minibuffer))

;;;###autoload
(defun jcs-counsel-find-files-slash ()
  "Find files slash key."
  (interactive)
  (unless (jcs-current-char-equal-p "/")
    (insert "/")))

;;;###autoload
(defun jcs-counsel-find-files-backspace ()
  "Find files backspace key."
  (interactive)
  (if (jcs-current-char-equal-p "/")
      (counsel-up-directory)
    (backward-delete-char 1)))

;;;###autoload
(defun jcs-counsel-find-files-enter ()
  "Find files enter key."
  (interactive)
  (unless (counsel-down-directory)
    (if (not jcs-file--selecting-file)
        (ivy-done)
      (let ((default-directory default-directory))
        (save-window-excursion
          (goto-char (point-min))
          (search-forward "Find file:")
          (setq default-directory (substring (buffer-string) (point) (- (line-end-position) 2))))
        (setq jcs-file--selected-file (expand-file-name (nth ivy--index ivy--all-candidates))))
      (jcs-ivy-immediate-done))))

;;;###autoload
(defun jcs-counsel-find-files-other-window ()
  "Find files on other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (setq found-file (counsel-find-file))
    (unless found-file
      (select-window starting-window))))

;;;###autoload
(defun jcs-counsel-projectile-find-file-other-window ()
  "Find files in project on other window."
  (interactive)
  (let ((record-dd default-directory)
        (found-file nil)
        (starting-window (selected-window)))
    (jcs-other-window-next 1 t)
    (setq default-directory record-dd)
    (setq found-file (counsel-projectile-find-file))
    (unless found-file
      (select-window starting-window))))


(provide 'jcs-ivy-func)
;;; jcs-ivy-func.el ends here
