;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-icons-prefix ""
        centaur-tabs-icon-scale-factor 0.9
        centaur-tabs-set-modified-marker t
        centaur-tabs-buffer-groups-function #'jcs-tab-buffer-groups
        centaur-tabs-custom-buffer-groups #'jcs-tab-custom-buffer-groups
        centaur-tabs-hide-predicate #'elenv-frame-util-p
        centaur-tabs-hide-tab-function #'centaur-tabs-hide-tab
        centaur-tabs-excluded-prefixes `(" *which")
        centaur-tabs-show-count t
        centaur-tabs-count-format " %d/%d"
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-down-tab-text " ▾ "
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  :config
  (advice-add 'centaur-tabs-buffer-track-killed :override #'ignore))

;;
;;; Buffer Groups

(defvar jcs-tab-line--group-cache (make-hash-table :test #'equal)
  "Cache for buffer groups.")

(defun jcs-tab-clear-dead-buffers ()
  "Remove all dead buffers from group cache."
  (ht-map (lambda (buffer _)
            (unless (buffer-live-p buffer)
              (ht-remove jcs-tab-line--group-cache buffer)))
          jcs-tab-line--group-cache))

(defun jcs-tab-buffer-groups ()
  "Group tabs with cache."
  (let* ((name (buffer-name))
         (buffer (current-buffer))
         (group (or (ht-get jcs-tab-line--group-cache buffer)
                    (car (funcall #'centaur-tabs-buffer-groups)))))
    (jcs-tab-clear-dead-buffers)
    (ht-set jcs-tab-line--group-cache buffer group)
    `(,group)))

(defun jcs-tab-custom-buffer-groups ()
  "Group tabs."
  (let ((name (buffer-name)))
    (cond ((string-prefix-p "*cider" name) "Cider")
          ((string-prefix-p "*sly" name) "Sly")
          ((string-prefix-p "*esup" name) "ESUP")
          ((string-match-p "lsp" name) "LSP-mode")
          ((and (featurep 'buffer-menu-filter)
                (diminish-buffer--filter name))
           "Hidden"))))
