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

(defun jcs-vertico--active-p ()
  "Return non-nil, only when vertico is active."
  (overlayp vertico--count-ov))

(defun jcs-vertico--goto (candidate)
  "Select candidate with CANDIDATE."
  (when (jcs-vertico--active-p)
    (jcs-with-no-redisplay
      (vertico--exhibit)
      (when-let ((index (jcs-vertico--index candidate)))
        (jcs-vertico--recenter index)))))

(defun jcs-vertico--recenter (index)
  "Recentering the current candidate."
  (jcs-with-no-redisplay
    (let ((center (/ vertico-count 2)) vertico-cycle)
      (vertico-last) (vertico--exhibit)
      (vertico--goto (- index center)) (vertico--exhibit)
      (vertico--goto index) (vertico--exhibit))))

(defun jcs-vertico--cd (path)
  "Move to PATH."
  (delete-minibuffer-contents) (insert path))

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
        ((save-excursion (search-backward "/~/" nil t))  ; Home
         (jcs-vertico--cd "~/"))
        ((save-excursion (search-backward "/!/" nil t))  ; Project
         (if (jcs-project-under-p)
             (jcs-vertico--cd (jcs-project-root))
           (backward-delete-char 2)
           (message "[INFO] Project root not found, return to previous directory")))
        ((save-excursion (search-backward "/./" nil t))   ; Current
         (backward-delete-char 2))
        ((save-excursion (search-backward "/../" nil t))  ; Up one
         (backward-delete-char 3)
         (vertico-directory-up))
        ;; New root, changing disk
        ((when-let* ((root
                      (save-excursion
                        (forward-char -1) (search-backward "/" nil t)
                        (buffer-substring (1+ (point)) (line-end-position))))
                     (_ (f-root-p root)))
           (jcs-vertico--cd root)))))

(jcs-advice-add 'vertico-directory-delete-char :override
  (let ((content (minibuffer-contents)))
    (cond ((vertico-directory-up)  ; preselect after up directory
           (jcs-vertico--goto (concat (file-name-nondirectory (directory-file-name content)) "/")))
          ((f-root-p content)  ; limit to root dir
           (jcs-vertico--cd (f-root)) (vertico-first))
          (t (call-interactively #'backward-delete-char)))))

;;
;; (@* "Sorting" )
;;

(defun jcs-vertico--sort-file-directory (input all)
  "Sort directory on top."
  (if (string-empty-p input)
      (sort (sort all #'string-lessp)
            (lambda (var1 var2)
              (and (string-suffix-p "/" var1)
                   (not (string-suffix-p "/" var2)))))
    #'vertico-sort-length-alpha))

(defvar jcs-vertico--sorting nil
  "Return non-nil if currently sorting.")

(defun jcs-vertico--sort-function (all)
  "Sort candidates ALL."
  (setq jcs-vertico--sorting nil)
  (let ((input (minibuffer-contents)) base)
    (cond
     ((jcs-M-x-p) (setq base #'vertico-sort-history-length-alpha))
     ((jcs-finding-file-p)
      (setq input (if (and (string-suffix-p "/" input) (jcs-directory-p input)) ""
                    (f-filename input))
            base (jcs-vertico--sort-file-directory input all))))
    ;; Final output
    (if (string-empty-p input)  ; Empty, return raw
        (if (null base) all
          (cond ((functionp base) (funcall base all))
                ((listp base) base)))
      (setq jcs-vertico--sorting t)
      ;; Return fuzzy order
      (jcs-sort-candidates-by-function all input #'flx-score))))

;;
;; (@* "Registry" )
;;

(defconst jcs-vertico-height-ratio 0.3
  "Ratio that respect to `frame-height' and `vertico-count'.")

(jcs-add-hook 'window-size-change-functions
  (setq vertico-count (floor (* (frame-height) jcs-vertico-height-ratio))))

(jcs-add-hook 'jcs-minibuffer-post-command-hook
  (when vertico-mode
    (cond ((jcs-finding-file-p)
           (when (memq this-command jcs-ffap-commands)
             (let* ((start (point)) (end (line-end-position))
                    (file (buffer-substring-no-properties start end)))
               (unless (string-empty-p file)
                 (delete-region start end)
                 (jcs-vertico--goto file))))
           (when (and (save-excursion (search-backward "~//" nil t))
                      (not (jcs-current-char-equal-p "/")))
             (save-excursion
               (forward-char -1)
               (backward-delete-char 1)))))
    (when jcs-vertico--sorting
      ;; Select first candidate (highest score) immediately after sorting!
      (jcs-with-no-redisplay (vertico--goto 0) (vertico--exhibit))
      (setq jcs-vertico--sorting nil))))  ; cancel it afterward

(jcs-add-hook 'minibuffer-setup-hook
  ;; Preselect file, on startup
  (when (and vertico-mode (memq this-command jcs-ffap-commands))
    (let (bfn path)
      (with-selected-window (minibuffer-selected-window)  ; collect data
        (setq bfn (buffer-file-name)
              path (ffap-guesser)))
      (cond
       ;; Preselect directory
       ((and path (file-directory-p path))
        (unless (string-suffix-p "/" (minibuffer-contents)) (insert "/"))
        (vertico-directory-delete-char))
       ;; Preselect file
       (bfn (jcs-vertico--goto (file-name-nondirectory bfn)))))))

(provide 'jcs-vertico)
;;; jcs-vertico.el ends here
