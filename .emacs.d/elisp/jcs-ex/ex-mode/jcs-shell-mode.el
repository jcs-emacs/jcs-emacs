;; ========================================================================
;; $File: jcs-shell-mode.el $
;; $Date: 2018-05-13 22:16:44 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'shell)
(defun jcs-shell-mode-hook ()
  "Shell mode hook."

  (define-key shell-mode-map (kbd "RET") 'jcs-shell-return)

  (define-key shell-mode-map (kbd "C-<backspace>") 'jcs-shell-backward-delete-word)
  (define-key shell-mode-map (kbd "C-S-<backspace>") 'jcs-shell-forward-delete-word)
  (define-key shell-mode-map (kbd "M-<backspace>") 'jcs-shell-backward-kill-word-capital)
  (define-key shell-mode-map (kbd "M-S-<backspace>") 'jcs-shell-forward-kill-word-capital)

  (define-key shell-mode-map (kbd "C-d") 'jcs-shell-kill-whole-line)
  (define-key shell-mode-map (kbd "<backspace>") 'jcs-shell-backspace)
  )
(add-hook 'shell-mode-hook 'jcs-shell-mode-hook)
