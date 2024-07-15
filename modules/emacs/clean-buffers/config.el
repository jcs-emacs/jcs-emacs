;;; emacs/clean-buffers/config.el  -*- lexical-binding: t; -*-

(use-package clean-buffers
  :init
  (setq clean-buffers-kill-active-buffer t
        clean-buffers-useless-buffer-names diminish-buffer-list
        clean-buffers-useful-buffer-names `("[*]Echo Area" "[*]Minibuf-"
                                            ,buffer-menu-filter-name
                                            "[*]eldoc"
                                            "[*]company")
        clean-buffers-judge-useless-buffer-functions
        '( clean-buffers-judge-useless-buffer-by-time
           clean-buffers-judge-useless-buffer-by-name
           diminish-buffer--filter))
  :config
  (jcs-advice-add 'clean-buffers-kill-useless-buffers :after
    (jcs-buffer-menu-refresh-buffer)))
