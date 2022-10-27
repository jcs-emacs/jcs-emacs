;;; term/shell/config.el  -*- lexical-binding: t; -*-

(require 'exec-path-from-shell)

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'shell-mode-hook
  (modify-syntax-entry ?> "!")

  (jcs-key-local
    `(((kbd "M-k")    . shell-pop)  ; Close it
      ((kbd "M-K")    . comint-clear-buffer)
      ((kbd "<up>")   . comint-previous-input)
      ((kbd "<down>") . comint-next-input)
      ((kbd "<tab>")  . comint-next-matching-input-from-input)
      ([C-up]         . block-travel-up)
      ([C-down]       . block-travel-down)
      ((kbd "C-~")    . (lambda () (interactive)
                          (shell-pop (multi-shell--next-valid-index))))
      ((kbd "C-_")    . multi-shell-prev)
      ((kbd "C-+")    . multi-shell-next)
      ((kbd "M-b")    . multi-shell-select))))

;;
;; (@* "Extensions" )
;;

(leaf exec-path-from-shell
  :defer-config
  (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

(leaf multi-shell
  :init
  (setq multi-shell-prefer-shell-type 'shell))  ; Accept `shell' or `eshll'.

(leaf shell-pop
  :init
  (setq shell-pop-window-size 60
        shell-pop-last-shell-buffer-index 0
        shell-pop-shell-type '("shell" "*shell: <>*" (lambda () (multi-shell))))
  :defer-config
  ;; Fix issue from https://github.com/kyagi/shell-pop-el/issues/51
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist))
