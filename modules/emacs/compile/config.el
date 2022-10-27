;;; emacs/compile/config.el  -*- lexical-binding: t; -*-

(leaf compile
  :init
  (setq compilation-context-lines t
        compilation-scroll-output t)
  :defer-config
  (require 'ansi-color)
  (jcs-add-hook 'compilation-filter-hook
    (let (buffer-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(leaf comint
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

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  (jcs-key-local
    `(((kbd "C-S-<f11>") . compilation-previous-error)
      ((kbd "C-S-<f12>") . compilation-next-error))))
