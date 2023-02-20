;;; completion/vertico/config.el  -*- lexical-binding: t; -*-

(use-package vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :hook (on-first-input . vertico-mode)
  :bind ( :map vertico-map
          ("\177"     . (lambda () (interactive)
                          (if (eq (char-before) ?/)
                              (vertico-directory-up 1)
                            (backward-delete-char 1))))
          ("<return>" . vertico-directory-enter)
          ("/"        . jcs-vertico-/))
  :init
  (setq vertico-cycle t
        vertico-resize t
        vertico-scroll-margin 0
        vertico-sort-function #'vertico-flx-sort-default))

(use-package marginalia
  :hook (on-first-input . marginalia-mode)
  :init
  (setq marginalia-align 'right))

(use-package vertico-multiform
  :hook (vertico-mode . vertico-multiform-mode)
  :init
  (setq vertico-multiform-categories
        '((file (vertico-sort-function . vertico-flx-sort-files)))))

(use-package vertico-flx
  :hook (vertico-mode . vertico-flx-mode))

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

(defun jcs-vertico--goto-cand (candidate)
  "Select candidate with CANDIDATE."
  (when (jcs-vertico--active-p)
    (jcs-with-no-redisplay
      (vertico--exhibit)
      (when-let ((index (jcs-vertico--index candidate)))
        (jcs-vertico--recenter index)))))

(defun jcs-vertico--goto (index)
  "Select candidate with INDEX."
  (when (jcs-vertico--active-p)
    (jcs-with-no-redisplay (vertico--exhibit) (vertico--goto index) (vertico--exhibit))))

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

(defun jcs-vertico-/ ()
  "Vertico slash key."
  (interactive)
  (insert "/")
  (when (mbs-finding-file-p)
    (jcs-vertico-find-files-/)))

(defun jcs-vertico-find-files-/ ()
  "After inserting slash."
  (cond ((save-excursion (search-backward "//" nil t))   ; Root
         (jcs-vertico--cd (f-root)))
        ((save-excursion (search-backward "/~/" nil t))  ; Home
         (jcs-vertico--cd "~/"))
        ((save-excursion (search-backward "/!/" nil t))  ; Project
         (if (jcs-project-under-p)
             (jcs-vertico--cd (jcs-project-root))
           (delete-char -2)
           (message "[INFO] Project root not found, return to previous directory")))
        ((save-excursion (search-backward "/./" nil t))   ; Current
         (delete-char -2))
        ((save-excursion (search-backward "/../" nil t))  ; Up one
         (delete-char -3)
         (vertico-directory-up 1))
        ;; New root, changing disk
        ((when-let* ((root
                      (save-excursion
                        (forward-char -1) (search-backward "/" nil t)
                        (buffer-substring (1+ (point)) (line-end-position))))
                     (_ (f-root-p root)))
           (jcs-vertico--cd root)))))

(jcs-advice-add 'vertico-directory-delete-char :override
  (let ((content (minibuffer-contents)))
    (cond ((vertico-directory-up 1)  ; preselect after up directory
           (jcs-vertico--goto-cand (concat (file-name-nondirectory (directory-file-name content)) "/")))
          ((and (mbs-finding-file-p) (f-root-p content))  ; limit to root dir
           (jcs-vertico--cd (f-root)) (vertico-first))
          (t (call-interactively #'backward-delete-char)))))

;;
;; (@* "Registry" )
;;

(defconst jcs-vertico-height-ratio 0.3
  "Ratio that respect to `frame-height' and `vertico-count'.")

(jcs-add-hook 'window-size-change-functions
  (setq vertico-count (floor (* (frame-height) jcs-vertico-height-ratio)))
  (when (jcs-vertico--active-p)
    (jcs-with-no-redisplay (ignore-errors (vertico--exhibit)))))

(defun jcs-vertico--post-command ()
  "Post command for vertico."
  (when (and vertico-mode (mbs-finding-file-p))
    (when (memq this-command jcs-ffap-commands)
      (let* ((start (point)) (end (line-end-position))
             (file (buffer-substring-no-properties start end)))
        (unless (string-empty-p file)
          (delete-region start end)
          (jcs-vertico--goto-cand file))))
    (when (and (save-excursion (search-backward "~//" nil t))
               (not (jcs-current-char-equal-p "/")))
      (save-excursion
        (forward-char -1)
        (delete-char -1)))
    (when (or vertico-flx--sorting
              (and (eq this-command 'vertico-directory-delete-char)
                   (string-empty-p (minibuffer-contents))))
      ;; Select first candidate (highest score) immediately after sorting!
      (jcs-vertico--goto 0)
      (setq vertico-flx--sorting nil))))  ; cancel it afterward

(jcs-advice-add 'vertico--setup :before
  (add-hook 'post-command-hook #'jcs-vertico--post-command nil 'local))

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
        (jcs-with-no-redisplay (vertico--exhibit))
        (vertico-directory-delete-char))
       ;; Preselect file
       (bfn (jcs-vertico--goto-cand (file-name-nondirectory bfn)))))))

;;
;; (@* "Minibuffer" )
;;

(jcs-add-hook 'minibuffer-setup-hook
  (jcs-reload-active-mode)
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command nil t))

(jcs-add-hook 'minibuffer-exit-hook
  (jcs-dashboard-refresh-buffer))

(defvar jcs-minibuffer-post-command-hook nil
  "Post command hook inside minibuffer.")

(defun jcs-minibuffer--post-command ()
  "Minibuffer post command hook."
  (run-hooks 'jcs-minibuffer-post-command-hook))
