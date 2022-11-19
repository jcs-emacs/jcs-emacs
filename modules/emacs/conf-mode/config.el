;;; emacs/conf-mode/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hooks" )
;;

(jcs-add-hook 'conf-mode-hook
  (setq-local electric-pair-open-newline-between-pairs nil))

(jcs-add-hook 'conf-toml-mode-hook
  (eldoc-toml-mode 1)
  (eldoc-mode 1))
