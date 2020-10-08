;;; jcs-dashboard.el --- Functions in dashboard-mode. (*Buffer List*).  -*- lexical-binding: t -*-
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
    (let ((path (expand-file-name (string-trim (thing-at-point 'line t)))))
      (setq recentf-list (delete path recentf-list)))
    (jcs-dashboard-refresh-buffer)))

;;;###autoload
(defun jcs-dashboard-remove-projects-item ()
  "Remove a path from `projectile-known-projects'."
  (interactive)
  (when (boundp 'projectile-known-projects)
    (let ((path (string-trim (thing-at-point 'line t))))
      (setq projectile-known-projects (delete path projectile-known-projects)))
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

(provide 'jcs-dashboard)
;;; jcs-dashboard.el ends here
