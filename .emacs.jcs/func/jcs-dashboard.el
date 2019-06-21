;;; jcs-dashboard.el --- Functions in dashboard-mode. (*Buffer List*).  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jcs-dashboard-page-break-list ()
  "Get the list of page break position."
  (let ((pb-lst '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
        (jcs-move-to-forward-a-char "\f")
        (push (line-number-at-pos) pb-lst)))
    (setq pb-lst (reverse pb-lst))
    pb-lst))

;;;###autoload
(defun jcs-dashboard-previous-blank-line ()
  "Dashboard previous blank line key."
  (interactive)
  (let* ((pg-lst (jcs-dashboard-page-break-list))
         (pg-start-ln (nth 0 pg-lst))
         (pg-end-ln (nth (- (length pg-lst) 2) pg-lst))
         (break-cond 0)
         (pg-cnt 0))
    (when (jcs-in-range-p (line-number-at-pos) pg-start-ln pg-end-ln)
      (forward-line -1)
      (setq pg-cnt 1))
    (while (and (not (jcs-is-beginning-of-buffer-p))
                (= break-cond 0))
      (forward-line -1)
      (when (jcs-current-line-empty-p)
        (setq break-cond 1))
      (save-excursion
        (end-of-line)
        (when (jcs-current-char-equal-p "\f")
          (setq break-cond 0)
          (setq pg-cnt (1+ pg-cnt))
          (when (>= pg-cnt 2)
            (setq break-cond 2)))))
    (when (= break-cond 2)
      (forward-line 1)
      (let* ((pg-lst (jcs-dashboard-page-break-list))
             (pg-start-ln (nth 0 pg-lst)))
        (when (< (line-number-at-pos) pg-start-ln)
          (call-interactively #'jcs-previous-blank-line))))))

;;;###autoload
(defun jcs-dashboard-next-blank-line ()
  "Dashboard next blank line key."
  (interactive)
  (let* ((pg-lst (jcs-dashboard-page-break-list))
         (pg-start-ln (nth 0 pg-lst))
         (pg-end-ln (nth (- (length pg-lst) 2) pg-lst))
         (break-cond 0)
         (pg-cnt 0))
    (when (jcs-in-range-p (line-number-at-pos) pg-start-ln pg-end-ln)
      (forward-line 1)
      (setq pg-cnt 1))
    (while (and (not (jcs-is-end-of-buffer-p))
                (= break-cond 0))
      (forward-line 1)
      (when (jcs-current-line-empty-p)
        (setq break-cond 1))
      (save-excursion
        (end-of-line)
        (when (jcs-current-char-equal-p "\f")
          (setq break-cond 0)
          (setq pg-cnt (1+ pg-cnt))
          (when (>= pg-cnt 2)
            (setq break-cond 2)))))
    (when (= break-cond 2)
      (forward-line -1))))

;;;###autoload
(defun jcs-dashboard-remove-item ()
  "Remove a item from the current item section."
  (interactive)
  (message "hel")
  )

;;;###autoload
(defun jcs-dashboard-remove-recent-files-item ()
  "Remove a file from `recentf-list'."
  (interactive)
  (when (boundp 'recentf-list)
    (let ((path (expand-file-name (string-trim (thing-at-point 'line t)))))
      (setq recentf-list (delete path recentf-list)))
    (jcs-dashboard-refresh-buffer)))

;;;###autoload
(defun jcs-dashboard-remove-projects-item ()
  "Remove a file from `recentf-list'."
  (interactive)
  (when (boundp 'projectile-known-projects)
    (let ((path (expand-file-name (string-trim (thing-at-point 'line t)))))
      (setq projectile-known-projects (delete path projectile-known-projects)))
    (jcs-dashboard-refresh-buffer)))


;;;###autoload
(defun jcs-dashboard-goto-item-section (id)
  "Navigate to item section by ID."
  (interactive)
  (let* ((pg-lst (jcs-dashboard-page-break-list))
         (items-id (1- id))
         (items-pg (nth items-id pg-lst))
         (items-len (- (length pg-lst) 2)))
    (when (and items-pg
               (< items-id items-len))
      (goto-char (point-min))
      (forward-line (1- items-pg))
      (forward-line 1))))

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
(defun jcs-dashboard-items-5 ()
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


(provide 'jcs-dashboard)
;;; jcs-dashboard.el ends here
