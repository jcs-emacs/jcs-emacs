;;; ui/dashboard/config.el  -*- lexical-binding: t; -*-

(leaf dashboard
  :init
  (setq dashboard-banner-logo-title "[J C S • E M A C S]"
        dashboard-footer-icon ""
        dashboard-footer-messages
        `(,(format "Copyright (c) %s %s" (format-time-string "%Y") "Shen, Jen-Chieh"))
        dashboard-items '((ls-directories . 5)
                          (ls-files       . 5)
                          (recents        . 5)
                          (projects       . 5)
                          ;;(bookmarks      . 5)
                          ;;(agenda         . 5)
                          ;;(registers      . 5)
                          )
        dashboard-item-shortcuts '((recents        . "r")
                                   (bookmarks      . "m")
                                   (projects       . "p")
                                   (agenda         . "a")
                                   (registers      . "e")
                                   (ls-directories . "d")
                                   (ls-files       . "f"))
        dashboard-center-content t
        dashboard-set-navigator nil
        dashboard-projects-backend 'project-el
        ;; Truncate style
        dashboard-path-style 'truncate-middle
        dashboard-recentf-show-base 'align
        dashboard-projects-show-base 'align
        dashboard-bookmarks-show-base 'align
        dashboard-bookmarks-item-format "%s  %s"
        dashboard-shorten-by-window-width t
        dashboard-shorten-path-offset 15)
  :defer-config
  (jcs-require '(project dashboard-ls))

  (jcs-add-hook 'jcs-after-load-theme-hook
    (setq dashboard-startup-banner (jcs-dashboard--get-banner-path))
    (jcs-dashboard-refresh-buffer))

  (dashboard-setup-startup-hook))

;;
;; (@* "Entry" )
;;

(defun jcs-dashboard ()
  "Jump to dashboard buffer; if doesn't exists create one."
  (interactive)
  (switch-to-buffer dashboard-buffer-name)
  (unless (eq major-mode 'dashboard-mode) (dashboard-mode))
  (jcs-dashboard-refresh-buffer))

(defun jcs-dashboard-other-window ()
  "Switch to dashboard buffer on other window."
  (interactive)
  (jcs-with-other-window (jcs-dashboard)))

(defun jcs-dashboard-refresh-buffer ()
  "Refresh dashboard buffer."
  (interactive)
  (jcs-when-buffer-window dashboard-buffer-name
    (jcs-with-dashboard-last-path
      (msgu-silent
        (jcs-save-window-excursion (dashboard-refresh-buffer))))))

(jcs-advice-add 'dashboard-remove-item-under :around
  (jcs-with-dashboard-last-path (apply arg0 args)))

(defun jcs-dashboard--get-banner-path ()
  "Return banner path."
  (concat
   user-emacs-directory "banners/"
   (cond (elenv-graphic-p (if (jcs-light-theme-p) "sink/black.png" "sink/white.png"))
         (t "sink.txt"))))

;;
;; (@* "Util" )
;;

(defmacro jcs-with-dashboard-last-path (&rest body)
  "Execute BODY with preserving dashboard current path."
  (declare (indent 0) (debug t))
  `(let ((dashboard-ls-path (jcs-last-default-directory))) ,@body))

;;
;; (@* "Refresh" )
;;

(jcs-advice-add 'dashboard-insert-startupify-lists :before
  ;; Execution before dashboard setup.
  (jcs-dashboard-init-info))

(jcs-advice-add 'dashboard-insert-startupify-lists :after
  ;; Execution after dashboard setup.
  (with-current-buffer dashboard-buffer-name
    (setq-local revert-buffer-function 'jcs-dashboard-revert)))

(defun jcs-dashboard-init-info ()
  "Initialize startup information for variable `dashboard-init-info'."
  (setq dashboard-init-info
        (format "%s packages loaded in %0.1f seconds"
                (length package-activated-list)
                (string-to-number (emacs-init-time)))))

(defun jcs-dashboard-revert (&rest _)
  "Revert for dashboard buffer."
  (unless (active-minibuffer-window) (jcs-dashboard-refresh-buffer)))

;;
;; (@* "Truncate" )
;;

(defun jcs-dashboard-current-list (name)
  "Return the list of current dashboard by NAME."
  (cl-case name
    (`recents recentf-list)
    (`bookmarks (bookmark-all-names))
    (`projects (dashboard-projects-backend-load-projects))
    (`ls-directories (mapcar #'f-slash (f-directories dashboard-ls--record-path)))
    (`ls-files (f-files dashboard-ls--record-path))
    (t (user-error "Unknown section for search: %s" name))))

(defun jcs-dashboard-current-item-in-path ()
  "Return the path from current dashboard section in path."
  (let ((section (dashboard--current-section)) path)
    (cl-case section
      (`bookmarks (setq path (bookmark-get-filename path)))
      (t
       (let ((lst (jcs-dashboard-current-list section))
             (index (jcs-dashboard-current-index section)))
         (setq path (nth index lst)))))
    path))

(defun jcs-dashboard--goto-section (name)
  "Move to section NAME declares in variable `dashboard-item-shortcuts'."
  (jcs-funcall-fboundp (intern (format "dashboard-jump-to-%s" name))))

(defun jcs-dashboard-current-index (name &optional pos)
  "Return the idex by NAME from POS."
  (let (target-ln section-line)
    (save-excursion
      (when pos (goto-char pos))
      (setq target-ln (line-number-at-pos))
      (jcs-dashboard--goto-section name)
      (setq section-line (line-number-at-pos)))
    (jcs-re-enable-mode 'global-hl-line-mode)
    (- target-ln section-line)))

(defun jcs-dashboard--on-path-item-p ()
  "Return non-nil if current point is on the item path from dashboard."
  (save-excursion
    (when (eolp) (ignore-errors (forward-char -1)))
    (jcs-current-point-face 'dashboard-items-face)))

(jcs-advice-add 'ffap-guesser :around
  ;; This advice is used when function `counsel--preselect-file' trying to
  ;; get the truncate path from dashboard buffer (ffap)
  (cl-case major-mode
    (`dashboard-mode
     (or (and (jcs-dashboard--on-path-item-p)
              (jcs-dashboard-current-item-in-path))
         (apply arg0 args)))  ; fallback
    (t (apply arg0 args))))

;;
;; (@* "Registry" )
;;

(defvar jcs-dashboard--last-window-width -1
  "Record the last window width from dashbord buffer.")

(defun jcs-dashboard--window-width ()
  "Return dashboard buffer's window width."
  (jcs-when-buffer-window dashboard-buffer-name (window-width)))

(jcs-add-hook 'window-size-change-functions
  (when-let ((new-ww (jcs-dashboard--window-width)))
    (unless (= new-ww jcs-dashboard--last-window-width)
      (setq jcs-dashboard--last-window-width new-ww)
      (jcs-dashboard-refresh-buffer))))
