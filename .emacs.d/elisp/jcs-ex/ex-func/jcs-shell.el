;; ========================================================================
;; $File: jcs-shell.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;===================================
;;      Toggle Shell window
;;---------------------------
;; TAG: shell, terminal

;;;###autoload
(defun jcs-toggle-shell-window ()
  "Toggle Shell Command prompt."
  (interactive)

  ;; local variable trigger/boolean
  ;; TOGGLE SOURCE: http://ergoemacs.org/emacs/elisp_toggle_command.html
  (if (get 'jcs-toggle-shell-window 'state)
      (progn
        (jcs-hide-shell-window)
        (put 'jcs-toggle-shell-window 'state nil))
    (progn
      (jcs-show-shell-window)
      (put 'jcs-toggle-shell-window 'state t)))
  )

;;---------------------------------------------
;; Show the shell window.
;;---------------------------------------------

(defun jcs-show-shell-window()
  "Shell Command prompt."
  (interactive)

  (when (not (get-buffer-process "*shell*"))
    (split-window-below)
    (switch-to-buffer-other-window "*shell*")
    (shell)

    ;; active truncate line as default for shell window.
    (toggle-truncate-lines)))

;;---------------------------------------------
;; Hide the shell window.
;;---------------------------------------------

(defun jcs-hide-shell-window ()
  "Kill process prompt."
  (interactive)

  ;; goto this window.
  (jcs-jump-to-window "*shell*")

  ;; kill the process before closing the frame.
  (when (get-buffer-process "*shell*")
    (kill-process)
    (erase-buffer))

  ;; kill the frame.
  (delete-window))
