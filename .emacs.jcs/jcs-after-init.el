;;; jcs-after-init.el --- Do stuff after initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Override all the mode's key bindings.
(load-file "~/.emacs.jcs/jcs-global-key.el")
;; Override all the mode's face.
(load-file "~/.emacs.jcs/jcs-face.el")


;; Call once the depends mode as default.
;; NOTE(jayces): Since I often use my own machine, set the online
;; mode as the default.
(call-interactively #'jcs-depend-mode)


;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-highlight-symbol-mode)
(diminish 'auto-rename-tag-mode)
(diminish 'undo-tree-mode)
(diminish 'company-mode)
(diminish 'eldoc-mode)
(diminish 'flycheck-mode)
(diminish 'flymake-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'impatient-mode)
(diminish 'js2-refactor-mode)
(diminish 'js2r)
(diminish 'line-reminder-mode)
(diminish 'outline-minor-mode)
(diminish 'overwrite-mode)
(diminish 'right-click-context-mode)
(diminish 'skewer-mode)
(diminish 'which-key-mode)
(diminish 'yas-minor-mode)


(provide 'jcs-after-init)
;;; jcs-after-init.el ends here
