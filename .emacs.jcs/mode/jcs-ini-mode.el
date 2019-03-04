;; ========================================================================
;; $File: jcs-ini-mode.el $
;; $Date: 2018-12-24 16:23:41 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'ini-mode)
(defun jcs-ini-mode-hook ()
  "INI mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")


  ;; jcs ini mode key binding
  (define-key ini-mode-map (kbd "<up>") #'previous-line)
  (define-key ini-mode-map (kbd "<down>") #'next-line)

  )
(add-hook 'ini-mode-hook 'jcs-ini-mode-hook)

(add-to-list 'auto-mode-alist '("\\.properties'?\\'" . ini-mode))
(add-to-list 'auto-mode-alist '("\\.ini'?\\'" . ini-mode))
