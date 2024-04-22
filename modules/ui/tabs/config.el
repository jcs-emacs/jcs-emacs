;;; ui/tabs/config.el  -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-cycle-scope 'tabs
        centaur-tabs-style "wave"
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"
        centaur-tabs-hide-tab-function (lambda (x &rest _)
                                         (or (centaur-tabs-hide-tab x)
                                             (and (memq major-mode '(buffer-menu-mode))
                                                  diminish-buffer-mode
                                                  (diminish-buffer--filter x))))))
