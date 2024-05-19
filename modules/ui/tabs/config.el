;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-cycle-scope 'tabs
        centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-set-modified-marker t
        centaur-tabs-hide-tab-function (lambda (x &rest _)
                                         (or (centaur-tabs-hide-tab x)
                                             (and (not (memql x `(,(get-buffer buffer-menu-filter-name))))
                                                  diminish-buffer-mode
                                                  (diminish-buffer--filter x))))
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-down-tab-text " ▾ "
        centaur-tabs-backward-tab-text "⏴"
        centaur-tabs-forward-tab-text "⏵")
  :config
  (advice-add 'centaur-tabs-buffer-track-killed :override #'ignore))
