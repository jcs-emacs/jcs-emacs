;; ========================================================================
;; $File: jcs-markdown-mode.el $
;; $Date: 2018-10-18 19:38:00 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


(require 'markdown-mode)
(defun jcs-markdown-mode-hook ()
  "Markdown mode hook."

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for Lua here
  (electric-pair-mode 1)

  ;; highlight URL and clickable.
  (goto-address-mode 1)


  ;; jcs Markdown key binding
  (define-key markdown-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key markdown-mode-map "\C-c\C-c" #'kill-ring-save)

  (define-key markdown-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key markdown-mode-map (kbd "<down>") #'jcs-next-line)
  )
(add-hook 'markdown-mode-hook 'jcs-markdown-mode-hook)

(add-to-list 'auto-mode-alist '("\\.md'?\\'" . markdown-mode))
