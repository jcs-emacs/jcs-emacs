;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-gray-out-icons nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-hide-tab-function #'jcs-hide-tabs
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

(defun jcs-hide-tabs (x &rest _)
  "Hide tabs."
  (or (centaur-tabs-hide-tab x)
      (and (featurep 'buffer-menu-filter)
           (not (memql x `(,(get-buffer buffer-menu-filter-name))))
           diminish-buffer-mode
           (diminish-buffer--filter x))))
