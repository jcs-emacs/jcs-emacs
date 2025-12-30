;;; completion/vertico/config.el  -*- lexical-binding: t; -*-

(use-package vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :hook (on-first-input . vertico-mode)
  :bind ( :map vertico-map
          ("\177"     . vertico-directory-delete-char)
          ("<return>" . vertico-directory-enter)
          ("/"        . jcs-vertico-/)
          (":"        . jcs-vertico-:))
  :init
  (setq vertico-cycle t
        vertico-resize t
        vertico-scroll-margin 0
        vertico-sort-function #'vertico-flx-sort-default
        ;; Don't mess up the order.
        ;;
        ;; See https://github.com/minad/vertico/discussions/585#discussioncomment-12878149
        vertico-sort-history-duplicate 0))

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

(when elenv-graphic-p
  (use-package nerd-icons-completion
    :hook (vertico-mode . nerd-icons-completion-mode)
    :init
    (setq nerd-icons-completion-icon-size 0.85)))

(use-package mbs
  :hook (vertico-mode . mbs-mode))

;;
;; (@* "Util" )
;;

(defun jcs-vertico--index (candidate)
  "Return candidate's index."
  (cl-position candidate vertico--candidates :test 'string=))

(defun jcs-vertico--active-p ()
  "Return non-nil, only when vertico is active."
  (overlayp vertico--count-ov))

(defun jcs-vertico--goto-cand (candidate)
  "Select candidate with CANDIDATE."
  (when (jcs-vertico--active-p)
    (elenv-with-no-redisplay
      (vertico--exhibit)
      (when-let* ((index (jcs-vertico--index candidate)))
        (jcs-vertico--recenter index)))))

(defun jcs-vertico--goto (index)
  "Select candidate with INDEX."
  (when (jcs-vertico--active-p)
    (elenv-with-no-redisplay
      (vertico--exhibit) (vertico--goto index) (vertico--exhibit))))

(defun jcs-vertico--recenter (index)
  "Recentering the current candidate."
  (elenv-with-no-redisplay
    (let ((center (/ vertico-count 2)) vertico-cycle)
      (vertico-last) (vertico--exhibit)
      (vertico--goto (- index center)) (vertico--exhibit)
      (vertico--goto index) (vertico--exhibit))))

(defun jcs-vertico--cd (path)
  "Move to PATH."
  (delete-minibuffer-contents) (insert path))

(defun jcs-vertico--reading-file-name-p ()
  "Return non-nil when reading file name."
  (and vertico-mode (mbs-reading-file-name-p)))

;;
;; (@* "Functions" )
;;

(defun jcs-vertico-refresh ()
  "Refresh vertico content."
  (when (jcs-vertico--active-p)
    (let ((vertico--input)) (vertico--exhibit))))

(defun jcs-vertico-: ()
  "Vertico colon key."
  (interactive)
  (insert ":")
  (when (mbs-reading-file-name-p)
    (jcs-vertico-find-files-:)))

(defun jcs-vertico-find-files-: ()
  "After inserting colon."
  (when-let* ((check (s-replace (f-root) "" (minibuffer-contents)))
              (matches (= 1 (s-count-matches ":" check)))
              (input (file-name-nondirectory (minibuffer-contents))))
    (jcs-vertico--cd (concat "/" input))))

(defun jcs-vertico-/ ()
  "Vertico slash key."
  (interactive)
  (insert "/")
  (when (mbs-reading-file-name-p)
    (jcs-vertico-find-files-/)))

(defun jcs-vertico-find-files-/ ()
  "After inserting slash."
  (cond ((save-excursion (search-backward "//" nil t))   ; Root
         (jcs-vertico--cd (f-root)))
        ((save-excursion (search-backward "/~/" nil t))  ; Home
         (jcs-vertico--cd "~/"))
        ((save-excursion (search-backward "/!/" nil t))  ; Project
         (if (project-current)
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
  (mbs-with-minibuffer-env
    (cond ((and (eq (char-before) ?/) (vertico-directory-up 1))  ; preselect after up directory
           (jcs-vertico--goto-cand (concat (file-name-nondirectory (directory-file-name contents)) "/")))
          ((and (mbs-reading-file-name-p) (f-root-p contents))  ; limit to root dir
           (jcs-vertico--cd (f-root)) (vertico-first))
          (t (call-interactively #'backward-delete-char)))))

(jcs-advice-add 'vertico-directory-enter :after
  ;; If we just enter a directory, always select the prompt.
  (when (jcs-vertico--active-p) (jcs-vertico--goto -1)))

;;
;; (@* "Registry" )
;;

(defconst jcs-vertico-height-ratio 0.3
  "Ratio that respect to `frame-height' and `vertico-count'.")

(jcs-add-hook 'window-size-change-functions
  (setq vertico-count (floor (* (frame-height) jcs-vertico-height-ratio)))
  (when (jcs-vertico--active-p)
    (elenv-with-no-redisplay (ignore-errors (vertico--exhibit)))))

(defun jcs-vertico--post-command ()
  "Post command for vertico."
  (when (jcs-vertico--reading-file-name-p)
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
      (setq vertico-flx--sorting nil))))

(jcs-advice-add 'vertico--setup :before
  (add-hook 'post-command-hook #'jcs-vertico--post-command 95 'local))

(defun jcs-minibuffer-setup-hook (&rest _)
  "Setup minibuffer hook."
  ;; Preselect file, on startup
  (when (jcs-vertico--reading-file-name-p)
    ;; We already have preselect functionality; therefore, we don't need to
    ;; the candidate to be shown in the input field.
    (when-let* ((start (point)) (end (line-end-position))
                (file (buffer-substring-no-properties start end))
                ((not (string-empty-p file))))
      (delete-region start end)       ; Delete the input field
      (jcs-vertico--goto-cand file))  ; Then preselect
    (let (bfn path)
      (with-selected-window (minibuffer-selected-window)  ; collect data
        (setq bfn (buffer-file-name)
              path (ffap-guesser)))
      (cond
       ;; Preselect directory
       ((and path (file-directory-p path))
        (unless (string-suffix-p "/" (minibuffer-contents)) (insert "/"))
        (elenv-with-no-redisplay (ignore-errors (vertico--exhibit)))
        (vertico-directory-delete-char))
       ;; Preselect file
       (bfn (jcs-vertico--goto-cand (file-name-nondirectory bfn)))))))

(add-hook 'minibuffer-setup-hook #'jcs-minibuffer-setup-hook 95)

;;
;; (@* "Minibuffer" )
;;

(jcs-add-hook 'minibuffer-setup-hook
  (jcs-reload-active-mode)
  (add-hook 'post-command-hook #'jcs-minibuffer--post-command 95 t))

(jcs-add-hook 'minibuffer-exit-hook
  (jcs-dashboard-refresh-buffer))

(defvar jcs-minibuffer-post-command-hook nil
  "Post command hook inside minibuffer.")

(defun jcs-minibuffer--post-command ()
  "Minibuffer post command hook."
  (run-hooks 'jcs-minibuffer-post-command-hook))

(defun jcs-clear-M-x-history ()
  "Clear M-x command history."
  (interactive)
  (let ((count (1- (length extended-command-history))))  ; Don't include itself!
    (setq extended-command-history nil)
    (msgu-inhibit-log
      (message "[INFO] Command history cleared: %s" count))))
