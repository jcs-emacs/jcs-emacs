;;; jcs-dashboard.el --- Functions in dashboard-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Util" )
;;

(defun jcs-dashboard-page-break-list ()
  "Get the list of page break position."
  (let ((pb-lst '()))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward dashboard-page-separator nil t)
        (push (line-number-at-pos) pb-lst)))
    (setq pb-lst (reverse pb-lst))
    pb-lst))

;;
;; (@* "Navigation" )
;;

;;;###autoload
(defun jcs-dashboard-previous-blank-line ()
  "Dashboard previous blank line key."
  (interactive)
  (let ((blank-pt (save-excursion (jcs-previous-blank-line) (point))))
    (if (and (forward-line -2)
             (search-backward dashboard-page-separator nil t)
             (< blank-pt (point)))
        (progn
          (forward-line 2)
          (beginning-of-line))
      (goto-char blank-pt))))

;;;###autoload
(defun jcs-dashboard-next-blank-line ()
  "Dashboard next blank line key."
  (interactive)
  (let ((blank-pt (save-excursion (jcs-next-blank-line) (point))))
    (if (and (forward-line 2)
             (search-forward dashboard-page-separator nil t)
             (<= (point) blank-pt))
        (progn
          (forward-line -2)
          (beginning-of-line))
      (goto-char blank-pt))))

;;
;; (@* "Refresh" )
;;

(defun jcs-dashboard-before-setup ()
  "Execution before dashboard setup."
  (jcs-dashboard-init-info)
  (jcs-project-list-clean))
(advice-add 'dashboard-insert-startupify-lists :before #'jcs-dashboard-before-setup)

(defun jcs-dashboard-after-setup ()
  "Execution after dashboard setup."
  (with-current-buffer dashboard-buffer-name
    (setq-local revert-buffer-function 'jcs-dashboard-revert)))
(advice-add 'dashboard-insert-startupify-lists :after #'jcs-dashboard-after-setup)

(defun jcs-dashboard-init-info ()
  "Initialize startup information for variable `dashboard-init-info'."
  (setq dashboard-init-info
        (format "%s packages loaded in %0.1f seconds"
                (length package-activated-list)
                (string-to-number (emacs-init-time)))))

(defun jcs-dashboard-revert (&rest _)
  "Revert for dashboard buffer."
  (unless (minibuffer-prompt) (jcs-dashboard-safe-refresh-buffer t)))

;;
;; (@* "Remove Items" )
;;

;;;###autoload
(defun jcs-dashboard-remove-current-item ()
  "Remove a item from the current item section."
  (interactive)
  (let ((pg-lst (jcs-dashboard-page-break-list))
        (index 0) (is-id -1) (item-title ""))
    (while (< index (1- (length pg-lst)))
      (let ((min-pg-ln (nth index pg-lst))
            (max-pg-ln (nth (1+ index) pg-lst)))
        (when (jcs-in-range-p (line-number-at-pos) min-pg-ln max-pg-ln)
          (setq is-id (1+ index))))
      (setq index (1+ index)))
    (unless (= is-id -1)
      (save-excursion
        (jcs-dashboard-goto-item-section is-id)
        (setq item-title (string-trim (thing-at-point 'line t))))
      (cond ((string-match-p "Recent Files:" item-title)
             (jcs-dashboard-remove-recent-files-item))
            ((string-match-p "Projects:" item-title)
             (jcs-dashboard-remove-projects-item))
            ((string-match-p "Bookmarks:" item-title)
             (jcs-dashboard-remove-bookmarks-item))
            ((string-match-p "Agenda for today:" item-title)
             (jcs-dashboard-remove-agenda-item))
            ((string-match-p  "Registers:" item-title)
             (jcs-dashboard-remove-registers-item))
            (t
             (user-error "[INFO] Can't remove item at current line: %s" item-title))))))

;;;###autoload
(defun jcs-dashboard-remove-recent-files-item ()
  "Remove a file from `recentf-list'."
  (interactive)
  (when (boundp 'recentf-list)
    (let ((path (save-excursion (end-of-line) (ffap-guesser))))
      (setq recentf-list (delete path recentf-list)))
    (jcs-dashboard-refresh-buffer)))

;;;###autoload
(defun jcs-dashboard-remove-projects-item ()
  "Remove a path from `project--list'."
  (interactive)
  (when (boundp 'project--list)
    (let ((path (save-excursion (end-of-line) (ffap-guesser))))
      (jcs-project-remove path))
    (jcs-dashboard-refresh-buffer)))

;;;###autoload
(defun jcs-dashboard-remove-bookmarks-item ()
  "Remove a bookmarks from `bookmark-alist'."
  (interactive)
  ;; TODO: implements this..
  )

;;;###autoload
(defun jcs-dashboard-remove-agenda-item ()
  "Remove an agenda from `org-agenda-files'."
  (interactive)
  ;; TODO: implements this..
  )

;;;###autoload
(defun jcs-dashboard-remove-registers-item ()
  "Remove a registers from `register-alist'."
  (interactive)
  ;; TODO: implements this..
  )

;;
;; (@* "Functions" )
;;

;;;###autoload
(defun jcs-dashboard-goto-item-section (id)
  "Navigate to item section by ID."
  (interactive)
  (let* ((pg-lst (jcs-dashboard-page-break-list))
         (items-id (1- id))
         (items-pg (nth items-id pg-lst))
         (items-len (- (length pg-lst) 2)))
    (when (and items-pg (<= items-id items-len))
      (jcs-goto-line items-pg)
      (call-interactively #'recenter))))

;;;###autoload
(defun jcs-dashboard-item-section-1 ()
  "Navigate to item 1."
  (interactive)
  (jcs-dashboard-goto-item-section 1))

;;;###autoload
(defun jcs-dashboard-item-section-2 ()
  "Navigate to item 2."
  (interactive)
  (jcs-dashboard-goto-item-section 2))

;;;###autoload
(defun jcs-dashboard-item-section-3 ()
  "Navigate to item 3."
  (interactive)
  (jcs-dashboard-goto-item-section 3))

;;;###autoload
(defun jcs-dashboard-item-section-4 ()
  "Navigate to item 4."
  (interactive)
  (jcs-dashboard-goto-item-section 4))

;;;###autoload
(defun jcs-dashboard-item-section-5 ()
  "Navigate to item 5."
  (interactive)
  (jcs-dashboard-goto-item-section 5))

;;;###autoload
(defun jcs-dashboard-item-section-6 ()
  "Navigate to item 6."
  (interactive)
  (jcs-dashboard-goto-item-section 6))

;;;###autoload
(defun jcs-dashboard-item-section-7 ()
  "Navigate to item 7."
  (interactive)
  (jcs-dashboard-goto-item-section 7))

;;;###autoload
(defun jcs-dashboard-item-section-8 ()
  "Navigate to item 8."
  (interactive)
  (jcs-dashboard-goto-item-section 8))

;;;###autoload
(defun jcs-dashboard-item-section-9 ()
  "Navigate to item 9."
  (interactive)
  (jcs-dashboard-goto-item-section 9))

;;
;; (@* "Truncate" )
;;

(defun jcs-dashboard--real-path-alist (index alist)
  "Return real path by INDEX from dahsboard path ALIST."
  (when (<= 0 index) (cdr (nth index (reverse alist)))))

(defun jcs-dashboard-get-path-alist ()
  "Return path from current point."
  (or (when-let* ((items (assoc 'recents dashboard-items))
                  (cnt (cdr items))
                  (index (jcs-dashboard-path-index 'recents)))
        (when (< index cnt)
          (jcs-dashboard--real-path-alist index dashboard-recentf-alist)))
      (when-let* ((items (assoc 'projects dashboard-items))
                  (cnt (cdr items))
                  (index (jcs-dashboard-path-index 'projects)))
        (when (< index cnt)
          (jcs-dashboard--real-path-alist index dashboard-projects-alist)))))

(defun jcs-dashboard--goto-section (name)
  "Move to section NAME declares in variable `dashboard-item-shortcuts'."
  (let ((key (cdr (assoc name dashboard-item-shortcuts))))
    (when key (execute-kbd-macro (kbd key)))))

(defun jcs-dashboard-path-index (name &optional pos)
  "Return the idex by NAME from POS."
  (let (target-ln section-line)
    (save-excursion
      (when pos (goto-char pos))
      (setq target-ln (line-number-at-pos))
      (jcs-dashboard--goto-section name)
      (setq section-line (line-number-at-pos)))
    (global-hl-line-mode 1)
    (- target-ln section-line)))

(defun jcs-dashboard--on-path-item-p ()
  "Return non-nil if current point is on the item path from dashboard."
  (save-excursion
    (when (jcs-is-end-of-line-p) (ignore-errors (forward-char -1)))
    (jcs-is-current-point-face 'dashboard-items-face)))

(defun jcs--ffap-guesser--advice-around (fnc &rest args)
  "Advice execution around function `ffap-guesser'.

This advice is used when function `counsel--preselect-file' trying to
get the truncate path from dashboard buffer (ffap)."
  (if (jcs-dashboard--on-path-item-p)
      (or (jcs-dashboard-get-path-alist)
          (let ((ls-path (f-join jcs-dashboard--last-ls-path (ffap-string-at-point))))
            (when (file-exists-p ls-path) ls-path))
          (apply fnc args))
    (apply fnc args)))
(advice-add 'ffap-guesser :around #'jcs--ffap-guesser--advice-around)

;;
;; (@* "Centering" )
;;

(defvar jcs-dashboard--last-window-width -1
  "Record the last window width from dashbord buffer.")

(defun jcs-dashboard--window-width ()
  "Return dashboard buffer's window width."
  (let (ww)
    (jcs-safe-jump-shown-to-buffer
     dashboard-buffer-name :type 'strict
     :success (lambda () (setq ww (window-width))))
    ww))

;;
;; (@* "Registry" )
;;

(defun jcs-dashboard--window-size-change ()
  "`window-size-change-functions' for `dashboard'."
  (let ((new-ww (jcs-dashboard--window-width)))
    (when (and new-ww (not (= new-ww jcs-dashboard--last-window-width)))
      (setq jcs-dashboard--last-window-width new-ww)
      (jcs-dashboard-safe-refresh-buffer t))))

(provide 'jcs-dashboard)
;;; jcs-dashboard.el ends here
