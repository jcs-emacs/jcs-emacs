;;; editor/file-templates/config.el  -*- lexical-binding: t; -*-

(leaf file-header
  :init
  (setq file-header-template-config-filepath (concat user-emacs-directory "templates/config.properties")
        file-header-template-dir (concat user-emacs-directory "templates/")))
