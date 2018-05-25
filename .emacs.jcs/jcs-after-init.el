;; ========================================================================
;; $File: jcs-after-init.el $
;; $Date: 2017-08-04 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Do stuff after initialize.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Override all the mode's key bindings.
(load-file "~/.emacs.jcs/jcs-global-key.el")


;; Call once the depends mode as default.
;; NOTE(jayces): Since I often use my own machine, set the online
;; mode as the default.
(call-interactively 'jcs-depend-mode)


;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
(diminish 'auto-highlight-symbol-mode)
(diminish 'undo-tree-mode)
(diminish 'company-mode)
(diminish 'flycheck-mode)
(diminish 'flymake-mode)
(diminish 'helm-mode)
(diminish 'helm-gtags-mode)
(diminish 'impatient-mode)
(diminish 'js2-refactor-mode)
(diminish 'js2r)
(diminish 'outline-minor-mode)
(diminish 'skewer-mode)
(diminish 'which-key-mode)
(diminish 'yas-minor-mode)
