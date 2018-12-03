;; ========================================================================
;; $File: jcs-hook.el $
;; $Date: 2018-05-12 08:53:47 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2018 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; All the hook event do here..
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;
;; All hook listed.
;; URL(jenchieh): https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-focus-in-hook ()
  "When window is focus."
  (jcs-revert-all-file-buffers)
  )
(add-hook 'focus-in-hook 'jcs-focus-in-hook)

;;;###autoload
(defun jcs-focus-out-hook ()
  "When window is not focus."
  )
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-find-file-hook ()
  "When temporary buffer shown."
  (save-selected-window
    (ignore-errors
      (jcs-jump-shown-to-buffer "*Buffer List*"))
    (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
      (jcs-buffer-menu)))
  )
(add-hook 'find-file-hook 'jcs-find-file-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-after-init-hook ()
  "Hook run after initialize."
  (jcs-reload-file-info)
  (jcs-reload-docstring-info)
  )
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-post-command-hook ()
  "Hook run after every command."
  (when (jcs-is-current-major-mode-p "web-mode")
    ;; Refresh the syntax highlighting.
    (call-interactively #'jcs-font-lock-fontify-buffer))
  )
(add-hook 'post-command-hook 'jcs-post-command-hook)
