;; ========================================================================
;; $File: jcs-message-mode.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


(defun jcs-message-mode-hook ()
  "Message mode hook."

  (abbrev-mode 1)

  (electric-pair-mode nil)

  ;; highlight URL and clickable.
  (goto-address-mode 1)

  ;; Auto highlight the same word.
  (auto-highlight-symbol-mode t)

  (define-key message-mode-map (kbd "<up>") #'previous-line)
  (define-key message-mode-map (kbd "<down>") #'next-line)
  )
(add-hook 'message-mode-hook 'jcs-message-mode-hook)
