;;; jcs-vertico.el --- Vertico function related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(jcs-add-hook 'window-size-change-functions
  (setq vertico-count (floor (* (frame-height) jcs-vertico-height-ratio))))

(jcs-add-hook jcs-minibuffer-post-command-hook
  (when vertico-mode
    (cond ((jcs-is-finding-file-p)
           (when (and (save-excursion (search-backward "~//" nil t))
                      (not (jcs-current-char-equal-p "/")))
             (save-excursion
               (forward-char -1)
               (backward-delete-char 1)))))))

(defun jcs-vertico--cd (path)
  "Move to PATH."
  (delete-minibuffer-contents)
  (insert path))

(defun jcs-vertico-find-files--slash ()
  "Find files slash key."
  (interactive)
  ;; NOTE: For some reason, slash does something else so override it.
  (insert "/")
  (cond ((save-excursion (search-backward "///" nil t))  ; Root
         (jcs-vertico--cd (f-root)))
        ((save-excursion (search-backward "/~/" nil t))  ; Home
         (jcs-vertico--cd "~/"))
        ((save-excursion (search-backward "/!/" nil t))  ; Project
         (if (jcs-project-current)
             (jcs-vertico--cd (jcs-project-current))
           (backward-delete-char 2)
           (message "[INFO] Project root not found, return to previous directory")))
        ((save-excursion (search-backward "/./" nil t))   ; Current
         (backward-delete-char 2))
        ((save-excursion (search-backward "/../" nil t))  ; Up one
         (backward-delete-char 3)
         (vertico-directory-up))))

(provide 'jcs-vertico)
;;; jcs-vertico.el ends here
