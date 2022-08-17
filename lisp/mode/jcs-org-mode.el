;;; jcs-org-mode.el --- Org mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require'dash)
(require 's)
(require 'org)

(setq org-startup-folded nil)

(setq org-todo-keywords '((sequence "TODO" "WAITING" "DONE"))
      org-todo-keyword-faces '(("TODO" :foreground "red")
                               ("WAITING" :foreground "yellow")
                               ("DONE" :foreground "green"))
      org-log-done 'time)

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
  (let ((keywords (-flatten org-todo-keywords)))
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

(jcs-add-hook 'org-mode-hook
  (org-indent-mode 1)

  (setq-local line-spacing 2)

  (jcs-key-local
    `(((kbd "C-a")        . mark-whole-buffer)
      ([tab]              . jcs-tab-key)
      ((kbd "C-y")        . jcs-redo)
      ([S-tab]            . jcs-org-smart-cycle)
      ((kbd "C-k"))
      ((kbd "C-<return>") . jcs-ctrl-return-key)
      ((kbd "S-<up>")     . jcs-org-table-up)  ; `org-nav'
      ((kbd "S-<down>")   . jcs-org-table-down)
      ((kbd "S-<left>")   . jcs-org-table-left)
      ((kbd "S-<right>")  . jcs-org-table-right))))

;;
;; (@* "Extensions" )
;;

(leaf org-superstar
  :hook (org-mode-hook . org-superstar-mode)
  :init
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-hide-leading-stars nil
        org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("[ ]"  . 9744)
                                          ("DONE" . 9745)
                                          ("[X]"  . 9745))
        org-superstar-headline-bullets-list '(?☰ ?☱ ?☲ ?☳ ?☴ ?☵ ?☶ ?☷)
        org-superstar-item-bullet-alist '((?* . ?•)
                                          (?+ . ?➤)
                                          (?- . ?╘))))

(leaf org-fancy-priorities
  :hook ((org-mode . org-fancy-priorities-mode)
         (org-agenda-mode . org-fancy-priorities-mode))
  :init
  (setq org-fancy-priorities-list '("╒" "↑" "■")))

(provide 'jcs-org-mode)
;;; jcs-org-mode.el ends here
