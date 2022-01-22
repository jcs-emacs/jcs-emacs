;;; jcs-vertico.el --- Vertico function related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Util" )
;;

(defconst jcs-ffap-commands '(ffap ffap-other-window)
  "List of ffap commands.")

(defun jcs-vertico--index (candidate)
  "Return candidate's index."
  (cl-position candidate vertico--candidates :test 'string=))

(defun jcs-vertico--goto (candidate)
  "Select candidate with CANDIDATE."
  (when-let ((index (jcs-vertico--index candidate)))
    (jcs-vertico--recenter index)))

(defun jcs-vertico--recenter (index)
  "Recentering the current candidate."
  (jcs-with-no-redisplay
    (let ((center (/ vertico-count 2)) vertico-cycle)
      (vertico-last) (vertico--exhibit)
      (vertico--goto (- index center)) (vertico--exhibit)
      (vertico--goto index) (vertico--exhibit))))

(defun jcs-vertico--cd (path)
  "Move to PATH."
  (delete-minibuffer-contents)
  (insert path))

;;
;; (@* "Functions" )
;;

(defun jcs-vertico-find-files--slash ()
  "Find files slash key."
  (interactive)
  ;; NOTE: For some reason, slash does something else so override it.
  (insert "/")
  (cond ((save-excursion (search-backward "//" nil t))  ; Root
         (jcs-vertico--cd (f-root)))
        ;; New root, changing disk
        ((when-let* ((root
                      (save-excursion
                        (forward-char -1) (search-backward "/" nil t)
                        (buffer-substring (1+ (point)) (line-end-position))))
                     (_ (f-root-p root)))
           (jcs-vertico--cd root)))
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

(jcs-advice-add 'vertico-directory-delete-char :override
  (let ((content (minibuffer-contents)))
    (if (vertico-directory-up)
        (jcs-with-no-redisplay
          (vertico--exhibit)
          (jcs-vertico--goto (concat (file-name-nondirectory (directory-file-name content)) "/")))
      (if (f-root-p content)
          (progn (jcs-vertico--cd (f-root)) (vertico-first))
        (call-interactively #'backward-delete-char)))))

;;
;; (@* "Sorting" )
;;

(defun jcs-vertico-sort (all)
  "Sort candidates ALL."
  (let ((input (minibuffer-contents))
        (base #'vertico-sort-history-length-alpha))
    (cond ((jcs-is-finding-file-p)
           (setq base #'vertico-sort-length-alpha
                 input (if (and (string-suffix-p "/" input) (jcs-directory-p input))
                           ""
                         (f-filename input)))))
    (if (string-empty-p (string-trim input)) (funcall base all)
      (jcs-sort-candidates-by-function all input #'flx-score))))

;;
;; (@* "Registry" )
;;

(jcs-add-hook 'window-size-change-functions
  (setq vertico-count (floor (* (frame-height) jcs-vertico-height-ratio))))

(jcs-add-hook 'jcs-minibuffer-post-command-hook
  (when vertico-mode
    (cond ((jcs-is-finding-file-p)
           (when (memq this-command jcs-ffap-commands)
             (let* ((start (point)) (end (line-end-position))
                    (file (buffer-substring-no-properties start end)))
               (unless (string-empty-p file)
                 (jcs-with-no-redisplay
                   (delete-region start end)
                   (vertico--exhibit)
                   (jcs-vertico--goto file)))))
           (when (and (save-excursion (search-backward "~//" nil t))
                      (not (jcs-current-char-equal-p "/")))
             (save-excursion
               (forward-char -1)
               (backward-delete-char 1)))))))

(provide 'jcs-vertico)
;;; jcs-vertico.el ends here
