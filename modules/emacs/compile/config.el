;;; emacs/compile/config.el  -*- lexical-binding: t; -*-

(use-package compile
  :init
  (setq compilation-context-lines t
        compilation-scroll-output t)

  (msg-clean-add-echo-commands '( compilation-handle-exit))
  :config
  (require 'ansi-color)
  (jcs-add-hook 'compilation-filter-hook
    (let (buffer-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(use-package comint
  :init
  (setq comint-prompt-read-only t
        comint-process-echoes t
        comint-scroll-to-bottom-on-input t
        comint-move-point-for-output t))

;;
;; (@* "Hook" )
;;

(jcs-add-hook '(compilation-mode-hook comint-mode-hook)
  (setq truncate-lines nil)

  (buffer-disable-undo)
  (goto-address-mode 1)

  (jcs-buffer-face-setup 'comint)

  (jcs-key-local
    `(((kbd "C-S-<f11>") . compilation-previous-error)
      ((kbd "C-S-<f12>") . compilation-next-error))))
