;;; jcs-ivy.el --- Ivy function related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'ffap)

(defun jcs-ivy--window-size-change ()
  "`window-size-change-functions' for `ivy-mode'."
  (setq ivy-height (round (* (frame-height) jcs-ivy-height-ratio)))
  (when jcs-minibuf-enabled-p
    (with-selected-window (minibuffer-window)
      (jcs-mute-apply
        (ivy--resize-minibuffer-to-fit)
        (ivy-shrink-after-dispatching)
        (ignore-errors (ivy--exhibit))))))

(defun jcs--ivy-skip-input-selection-p ()
  "Decide weather to skip the input selection.
Return non-nil, to skip the input selection.
Return nil, to NOT to skip the input selection."
  (let ((first-cand (nth 0 ivy--old-cands)) do-skip)
    (cond ((string-empty-p ivy-text)
           (setq do-skip t))
          ((and (stringp first-cand)
                (or (jcs-is-finding-file-p) (jcs-is-renaming-p))
                (string= (f-filename first-cand) ivy-text))
           (setq do-skip t)))
    do-skip))

(defun jcs--ivy-previous-line--advice-after (&rest _)
  "Advice execute after `ivy-previous-line' function."
  (when (and (= ivy--index -1) (jcs--ivy-skip-input-selection-p))
    (call-interactively #'ivy-previous-line)))
(advice-add 'ivy-previous-line :after #'jcs--ivy-previous-line--advice-after)

(defun jcs--counsel-up-directory ()
  "Fixed just calling `counsel-up-directory' wouldn't go anywhere issue."
  (let ((cur-dir (directory-file-name (expand-file-name ivy--directory))))
    (ivy--cd cur-dir))
  (counsel-up-directory))

(defun jcs-counsel-find-files--slash ()
  "Find files slash key."
  (interactive)
  ;; NOTE: For some reason, slash does something else so override it.
  (insert "/")
  (cond ((save-excursion (search-backward "///" nil t))  ; Root
         (ivy--cd (f-root)))
        ((save-excursion (search-backward "/!/" nil t))  ; Project
         (if (jcs-project-current)
             (ivy--cd (jcs-project-current))
           (backward-delete-char 2)
           (message "[INFO] Project root not found, return to previous directory")))
        ((save-excursion (search-backward "/./" nil t))   ; Current
         (backward-delete-char 2))
        ((save-excursion (search-backward "/../" nil t))  ; Up one
         (backward-delete-char 3)
         (jcs--counsel-up-directory))))

(defun jcs-counsel-find-files-backspace ()
  "Find files backspace key."
  (interactive)
  (if (or (jcs-current-char-equal-p "/")
          (jcs-current-line-empty-p))  ; Fix for deep directory tree on newline.
      (counsel-up-directory)
    (backward-delete-char 1)))

(defun jcs-counsel-find-files-enter ()
  "Find files enter key."
  (interactive)
  (if (string= (nth ivy--index ivy--old-cands) "./") (ivy--exhibit)
    (unless (counsel-down-directory) (ivy-done))))

(provide 'jcs-ivy)
;;; jcs-ivy.el ends here
