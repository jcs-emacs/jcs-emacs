;;; jcs-re-builder-mode.el --- RE-Builder mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 're-builder)


(defun jcs-re-builder-mode-hook ()
  "Mode hook for `RE-Builder-mode'."

  ;; Normal
  (define-key reb-mode-map (kbd "C-d") #'jcs-kill-whole-line)
  (define-key reb-mode-map (kbd "C-c C-c") #'kill-ring-save)

  (define-key reb-mode-map (kbd "<up>") #'previous-line)
  (define-key reb-mode-map (kbd "<down>") #'next-line)

  (define-key reb-mode-map (kbd "M-k") #'jcs-reb-maybe-kill-this-buffer)
  )

(add-hook 'reb-mode-hook 'jcs-re-builder-mode-hook)


(provide 'jcs-re-builder-mode)
;;; jcs-re-builder-mode.el ends here
