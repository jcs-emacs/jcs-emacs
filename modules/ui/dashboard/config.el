;;; ui/dashboard/config.el  -*- lexical-binding: t; -*-

(require 'nerd-icons)

(use-package dashboard
  :bind ( :map dashboard-mode-map
          ("<up>"    . previous-line)
          ("<down>"  . next-line)
          ("C-k C-p" . package-list-packages)
          ("M-K"     . jcs-dashboard-refresh-buffer))
  :hook (dashboard-after-initialize
         . (lambda ()
             (unless noninteractive
               ;; Split windows depends on the display size!
               (if (elenv-monitor-vertical-p)
                   (ignore-errors (split-window-vertically))
                 (ignore-errors (split-window-horizontally)))
               ;; Switch to scratch buffer for other window
               (save-selected-window
                 (switch-to-buffer-other-window (get-scratch-buffer-create)))
               ;; Make sure dashboard buffer left most!
               (centaur-tabs-move-current-tab-to-left)
               ;; Make messages buffer to the right most.
               (with-current-buffer (messages-buffer)
                 (centaur-tabs-move-current-tab-to-right)
                 (centaur-tabs-move-current-tab-to-right)))))
  :init
  (setq dashboard-banner-logo-title
        (concat "[J C S " (if elenv-graphic-p "•" "-") " E M A C S]")
        dashboard-startupify-list '( dashboard-insert-banner
                                     dashboard-insert-newline
                                     dashboard-insert-banner-title
                                     dashboard-insert-newline
                                     dashboard-insert-navigator
                                     dashboard-insert-newline
                                     dashboard-insert-init-info
                                     dashboard-insert-items
                                     dashboard-insert-newline
                                     dashboard-insert-footer)
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
        dashboard-vertically-center-content t
        dashboard-projects-backend 'project-el
        ;; Icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons nil
        dashboard-icon-type 'nerd-icons
        dashboard-heading-icons '((recents        . "nf-oct-history")
                                  (bookmarks      . "nf-oct-bookmark")
                                  (agenda         . "nf-oct-calendar")
                                  (projects       . "nf-oct-rocket")
                                  (registers      . "nf-oct-database")
                                  (ls-files       . "nf-oct-file")
                                  (ls-directories . "nf-oct-file_directory"))
        dashboard-heading-icon-height 0.9
        dashboard-heading-icon-v-adjust 0.05
        dashboard-icon-file-height 0.8
        dashboard-icon-file-v-adjust 0.0
        dashboard-navigator-buttons
        `(((,(when elenv-graphic-p
               (nerd-icons-faicon "nf-fa-github"
                                  :height dashboard-heading-icon-height
                                  :v-adjust dashboard-heading-icon-v-adjust))
            "Homepage" "Browse homepage"
            (lambda (&rest _) (browse-url jcs-homepage)))
           (,(when elenv-graphic-p
               (nerd-icons-faicon "nf-fae-tools"
                                  :height dashboard-heading-icon-height
                                  :v-adjust dashboard-heading-icon-v-adjust))
            "Settings" "Open custom file"
            (lambda (&rest _) (find-file custom-file)))
           (,(when elenv-graphic-p
               (nerd-icons-mdicon "nf-md-update"
                                  :height dashboard-heading-icon-height
                                  :v-adjust dashboard-heading-icon-v-adjust))
            "Update" "Update JCS-Emacs"
            (lambda (&rest _) (jcs-update-config)))
           (,(if elenv-graphic-p
                 (nerd-icons-faicon "nf-fa-question"
                                    :height dashboard-heading-icon-height
                                    :v-adjust dashboard-heading-icon-v-adjust)
               "?")
            "" "Help (?/h)"
            (lambda (&rest _) (help))
            font-lock-string-face)))
        ;; Truncate style
        dashboard-path-style 'truncate-middle
        dashboard-recentf-show-base 'align
        dashboard-projects-show-base 'align
        dashboard-bookmarks-show-base 'align
        dashboard-bookmarks-item-format "%s  %s"
        dashboard-shorten-by-window-width t
        dashboard-shorten-path-offset 15
        ;; Footer
        dashboard-footer-icon (if elenv-graphic-p
                                  (nerd-icons-octicon "nf-oct-heart" :face 'error)
                                "")
        dashboard-footer-messages
        `(,(concat "I showed you my source code, pls respond"
                   "\n\n"
                   (when elenv-graphic-p " ")
                   "       "
                   (format "Powered by %s, %s"
                           "Jen-Chieh"
                           (format-time-string "%Y"))))
        dashboard-navigation-cycle t
        dashboard-remove-missing-entry t)
  :config
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
        (elenv-save-window-excursion (dashboard-refresh-buffer))))))

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
  `(let ((dashboard-ls-path (if-let* ((buffers (nth 0 (jcs-valid-buffer-list))))
                                (file-name-directory (buffer-file-name buffers))
                              default-directory)))
     ,@body))

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
        (format "%d packages installed; Emacs started in %0.1f seconds."
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
  (jcs-fboundp-apply (intern (format "dashboard-jump-to-%s" name))))

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
  (when-let* ((new-ww (jcs-dashboard--window-width)))
    (unless (= new-ww jcs-dashboard--last-window-width)
      (setq jcs-dashboard--last-window-width new-ww)
      (jcs-dashboard-refresh-buffer))))
