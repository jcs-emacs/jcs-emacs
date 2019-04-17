;;; jcs-after-init.el --- Do stuff after initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Override all the mode's key bindings.
(load-file "~/.emacs.jcs/jcs-key.el")
;; Override all the mode's face.
(load-file "~/.emacs.jcs/jcs-face.el")


;; Call once the depends mode as default.
;; NOTE(jayces): Since I often use my own machine, set the online
;; mode as the default.
(call-interactively #'jcs-depend-mode)


;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'flycheck-mode)
  (diminish 'outline-minor-mode)
  (diminish 'overwrite-mode)
  (diminish 'page-break-lines-mode))


(provide 'jcs-after-init)
;;; jcs-after-init.el ends here
