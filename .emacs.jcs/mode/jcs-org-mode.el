;;; jcs-org-mode.el --- Org mode. -*- lexical-binding: t -*-
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

(defvar jcs-org-table--timer nil
  "Timer for refreshing org table.")

(defvar jcs-org-table--delay 0.4
  "Time delay for refresh org table.")

(defun jcs-org-table-inside-p (&optional point)
  "Return non-nil if POINT currently inside the org table."
  (and (< (org-table-begin) (point)) (> (org-table-end) (point))))

(defun jcs-org-table--refresh ()
  "Refresh org mode table."
  (when (jcs-org-table-inside-p)
    (let* ((word-start (save-excursion (search-backward " " nil t)))
           (cursor-pt (if word-start (- (point) word-start) nil))
           (field-start (save-excursion (search-backward "|" nil t)))
           field-prefix field-prefix-word-cnt)
      (when field-start
        (setq field-prefix (buffer-substring field-start word-start)
              field-prefix-word-cnt (1- (length (s-split " " field-prefix t))))
        (ignore-errors (org-table-align))
        (when (and (numberp field-prefix-word-cnt) (< 0 field-prefix-word-cnt))
          (forward-symbol field-prefix-word-cnt)
          (forward-char 1))
        (when (numberp cursor-pt) (forward-char (1- cursor-pt)))))))

(defun jcs-org-table--start-timer ()
  "Start org table refresh timer."
  (jcs-safe-kill-timer jcs-org-table--timer)
  (setq jcs-org-table--timer (run-with-idle-timer jcs-org-table--delay nil
                                                  #'jcs-org-table--refresh)))

(defun jcs-org--post-command (&rest _)
  "Hook for post command in `org-mode'."
  (unless undo-in-progress (jcs-org-table--start-timer)))

;;----------------------------------------------------------------------------

(defun jcs-org-mode-hook ()
  "Org mode hook."
  (goto-address-mode 1)
  (auto-highlight-symbol-mode t)

  (add-hook 'post-command-hook #'jcs-org--post-command nil t)

  ;; Normal
  (define-key org-mode-map (kbd "C-a") #'jcs-mark-whole-buffer)
  (define-key org-mode-map [tab] #'jcs-tab-key)

  (define-key org-mode-map (kbd "C-y") #'jcs-redo)

  (define-key org-mode-map [S-tab] #'jcs-org-smart-cycle)
  (define-key org-mode-map (kbd "C-k") nil)
  (define-key org-mode-map (kbd "C-<return>") #'jcs-ctrl-return-key)

  ;; `org-nav'
  (define-key org-mode-map (kbd "S-<up>") #'jcs-org-table-up)
  (define-key org-mode-map (kbd "S-<down>") #'jcs-org-table-down)
  (define-key org-mode-map (kbd "S-<left>") #'jcs-org-table-left)
  (define-key org-mode-map (kbd "S-<right>") #'jcs-org-table-right))

(add-hook 'org-mode-hook 'jcs-org-mode-hook)

(provide 'jcs-org-mode)
;;; jcs-org-mode.el ends here
