;;; term/shell/config.el  -*- lexical-binding: t; -*-

(require 'compile)

(msg-clean-add-echo-commands '( shell-dirstack-message))

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'shell-mode-hook
  (modify-syntax-entry ?> "!")

  (jcs-key-local
    `(((kbd "M-k")      . shell-pop)  ; Close it
      ((kbd "M-K")      . comint-clear-buffer)
      ((kbd "M-<up>")   . comint-previous-input)
      ((kbd "M-<down>") . comint-next-input)
      ((kbd "<tab>")    . comint-next-matching-input-from-input)
      ([C-up]           . block-travel-up)
      ([C-down]         . block-travel-down)
      ((kbd "C-~")      . (lambda () (interactive)
                            (shell-pop (multi-shell--next-valid-index))))
      ((kbd "C-_")      . multi-shell-prev)
      ((kbd "C-+")      . multi-shell-next)
      ((kbd "M-b")      . multi-shell-select))))

;;
;; (@* "Extensions" )
;;

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

(use-package multi-shell
  :init
  (setq multi-shell-prefer-shell-type 'shell))  ; Accept `shell' or `eshll'.

(use-package shell-pop
  :init
  (setq shell-pop-window-size 60
        shell-pop-last-shell-buffer-index 0
        shell-pop-shell-type '("shell" "*shell: <>*" (lambda () (multi-shell)))
        shell-pop-autocd-to-working-dir nil
        shell-pop-restore-window-configuration nil)
  :config
  ;; Fix issue from https://github.com/kyagi/shell-pop-el/issues/51
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist))

(use-package comint-fold
  :init
  (setq comint-fold-remap-tab nil
        comint-fold-fringe-indicator nil)
  (comint-fold-mode 1))
