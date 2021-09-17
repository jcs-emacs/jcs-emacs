;;; jcs-org-mode.el --- Org mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'org)
(require 'org-bullets)

(add-hook 'org-mode-hook 'org-bullets-mode)

(setq org-startup-folded nil)

(setq org-todo-keywords '((sequence "TODO" "WAITING" "DONE"))
      org-todo-keyword-faces '(("TODO" :foreground "red")
                               ("WAITING" :foreground "yellow")
                               ("DONE" :foreground "green"))
      org-log-done 'time)

;;
;; (@* "Faces" )
;;

(defun jcs-init-org-faces ()
  "Initialize Org mode faces highlihgting."
  (let ((org-font-lock-comment-face-modes '(org-mode)))
    (dolist (mode org-font-lock-comment-face-modes)
      (font-lock-add-keywords
       mode
       '(("\\(#[[:blank:][:graph:]]*\\)" 1 'font-lock-comment-face))
       'end)))
  ;; Code Block
  (set-face-attribute 'org-block nil
                      :inherit nil
                      :foreground (face-foreground 'default)
                      :background "#2B2B2B"
                      :extend t)
  ;; Header
  (set-face-attribute 'org-level-1 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-2 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-3 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-4 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-5 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-6 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-7 nil :foreground "#B5CCEB")
  (set-face-attribute 'org-level-8 nil :foreground "#B5CCEB"))

;;
;; (@* "Table" )
;;

(defun jcs-org--is-row-a-dividers-p ()
  "Check if current row is a dividers row."
  (save-excursion
    (let (tmp-end-of-line-point tmp-ret-val)
      (end-of-line)
      (setq tmp-end-of-line-point (point))
      (beginning-of-line)
      (while (< (point) tmp-end-of-line-point)
        (when (jcs-current-char-equal-p '("-" "+")) (setq tmp-ret-val t))
        (forward-char 1))
      tmp-ret-val)))

(defun jcs-org--is-good-row-p ()
  "Check if is a good row to move the cursor up or down."
  (and (not (jcs-current-line-empty-p))
       (not (jcs-org--is-row-a-dividers-p))))

(defun jcs-org--count-current-column ()
  "Count the current cursor in which column in the table."
  (save-excursion
    (let ((tmp-column-count 0) tmp-end-of-line-point)
      ;; If is a good row to check
      (when (jcs-org--is-good-row-p)
        (end-of-line)
        (setq tmp-end-of-line-point (point))

        (beginning-of-line)

        (while (< (point) tmp-end-of-line-point)
          (when (jcs-current-char-equal-p "|")
            (setq tmp-column-count (1+ tmp-column-count)))
          (forward-char 1)))
      tmp-column-count)))

(defun jcs-org-table-up ()
  "Move cursor up one row if in the table."
  (interactive)
  (let ((tmp-column-count (jcs-org--count-current-column))
        (cycle-counter 0))
    (while (< cycle-counter tmp-column-count)
      (jcs-org-table-left)
      (setq cycle-counter (1+ cycle-counter)))))

(defun jcs-org-table-down ()
  "Move cursor down one row if in the table."
  (interactive)
  (let ((tmp-column-count (jcs-org--count-current-column))
        (cycle-counter 0))
    (while (< cycle-counter tmp-column-count)
      (jcs-org-table-right)
      (setq cycle-counter (1+ cycle-counter)))))

(defun jcs-org-table-left ()
  "Move cursor left one column if in the table."
  (interactive)
  (org-shifttab))

(defun jcs-org-table-right ()
  "Move cursor right one column if in the table."
  (interactive)
  (org-cycle))

(defun jcs-org-smart-cycle ()
  "Try current cycle at point if available."
  (interactive)
  (let ((keywords (jcs-flatten-list org-todo-keywords)))
    (cond
     ((memq (thing-at-point 'word) keywords)
      (org-todo)
      (forward-char 1)
      (unless (memq (thing-at-point 'word) keywords)
        (org-todo)
        (forward-word -1)))
     (t (org-cycle)))))

;;
;; (@* "Hook" )
;;

(defun jcs-org-mode-hook ()
  "Org mode hook."
  ;; Normal
  (jcs-bind-key (kbd "C-a") #'jcs-mark-whole-buffer)
  (jcs-bind-key [tab] #'jcs-tab-key)

  (jcs-bind-key (kbd "C-y") #'jcs-redo)

  (jcs-bind-key [S-tab] #'jcs-org-smart-cycle)
  (jcs-bind-key (kbd "C-k") nil)
  (jcs-bind-key (kbd "C-<return>") #'jcs-ctrl-return-key)

  ;; `org-nav'
  (jcs-bind-key (kbd "S-<up>") #'jcs-org-table-up)
  (jcs-bind-key (kbd "S-<down>") #'jcs-org-table-down)
  (jcs-bind-key (kbd "S-<left>") #'jcs-org-table-left)
  (jcs-bind-key (kbd "S-<right>") #'jcs-org-table-right))

(add-hook 'org-mode-hook 'jcs-org-mode-hook)

(provide 'jcs-org-mode)
;;; jcs-org-mode.el ends here
