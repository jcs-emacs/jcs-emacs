;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-modified-marker t
        centaur-tabs-buffer-groups-function #'jcs-tab-buffer-groups-function
        centaur-tabs-hide-tab-function #'ignore
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-down-tab-text " ▾ "
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  :config
  (advice-add 'centaur-tabs-buffer-track-killed :override #'ignore)

  (jcs-add-hook 'jcs-after-load-theme-hook
    (jcs-re-enable-mode-if-was-enabled #'centaur-tabs-mode)))

;;
;;; Buffer Groups

(defvar jcs-tab-line--group-cache (make-hash-table)
  "Cache for buffer groups.")

(defun jcs-tab-buffer-groups-function ()
  "Group tabs."
  (let* ((name (buffer-name))
         (group (or (ht-get jcs-tab-line--group-cache name)
                    (cond ((and (featurep 'buffer-menu-filter)
                                (diminish-buffer--filter name))
                           "Hidden")
                          (t
                           (car (funcall #'centaur-tabs-buffer-groups)))))))
    (ht-set jcs-tab-line--group-cache name group)
    `(,group)))
