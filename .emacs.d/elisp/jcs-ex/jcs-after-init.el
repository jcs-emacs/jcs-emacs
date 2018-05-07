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
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-global-key.el")

;;
;; All hook listed.
;; URL(jenchieh): https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;

;;;###autoload
(defun jcs-focus-in-hook ()
  "When window is focus."
  )

;;;###autoload
(defun jcs-focus-out-hook ()
  "When window is not focus."
  )

(add-hook 'focus-in-hook 'jcs-focus-in-hook)
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

;;;###autoload
(defun jcs-find-file-hook ()
  "When temporary buffer shown."

  (save-selected-window
    (ignore-errors
      (jcs-jump-to-window "*Buffer List*"))
    (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
      (jcs-buffer-menu)))
  )
(add-hook 'find-file-hook 'jcs-find-file-hook)

(defun jcs-after-init-hook ()
  "Hook run after initialize."
  (jcs-reload-file-info)
  (jcs-reload-docstring-info)
  )
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;; Diminish
;; NOTE(jenchieh): Do not show theses modes in the mode line.
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'auto-complete-mode)
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
(diminish 'yas-minor-mode)
(diminish 'auto-highlight-symbol-mode)
