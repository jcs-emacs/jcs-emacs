;; ========================================================================
;; $File: jcs-shell-mode.el $
;; $Date: 2018-05-13 22:16:44 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'company)
(require 'shell)
(defun jcs-shell-mode-hook ()
  "Shell mode hook."

  ;; Key
  (define-key shell-mode-map (kbd "DEL") #'jcs-delete-backward-char)

  ;; Completion
  (define-key shell-mode-map [tab] #'jcs-company-manual-begin)

  ;; Command Input
  (define-key shell-mode-map (kbd "RET") #'jcs-shell-return)

  ;; Navigation
  (define-key shell-mode-map (kbd "<up>") 'jcs-shell-up-key)
  (define-key shell-mode-map (kbd "<down>") 'jcs-shell-down-key)
  (define-key shell-mode-map [C-up] #'previous-blank-line)
  (define-key shell-mode-map [C-down] #'next-blank-line)

  ;; Editing
  (define-key shell-mode-map "\C-c\C-c" #'kill-ring-save)
  (define-key shell-mode-map "\C-x\C-x" #'kill-ring-save)

  ;; Deletion
  (define-key shell-mode-map (kbd "C-<backspace>") #'jcs-shell-backward-delete-word)
  (define-key shell-mode-map (kbd "C-S-<backspace>") #'jcs-shell-forward-delete-word)
  (define-key shell-mode-map (kbd "M-<backspace>") #'jcs-shell-backward-kill-word-capital)
  (define-key shell-mode-map (kbd "M-S-<backspace>") #'jcs-shell-forward-kill-word-capital)

  (define-key shell-mode-map (kbd "C-d") #'jcs-shell-kill-whole-line)
  (define-key shell-mode-map (kbd "<backspace>") #'jcs-shell-backspace)
  )
(add-hook 'shell-mode-hook #'jcs-shell-mode-hook)
(add-hook 'shell-mode-hook #'company-mode)
