;;; reader/log/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'logview-mode-hook
  (setq buffer-invisibility-spec t)
  (ansi-colorful-mode t))
