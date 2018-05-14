;; ========================================================================
;; $File: jcs-shell.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


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
      (put 'jcs-toggle-shell-window 'state t))))

;;;###autoload
(defun jcs-show-shell-window()
  "Shell Command prompt."
  (interactive)

  (when (not (get-buffer-process "*shell*"))
    (split-window-below)

    ;; TODO(jenchieh): I have no idea why the first time would
    ;; not work. So I have to error handle it and do it again
    ;; to just in if something weird happen to Emacs itself.
    ;;
    ;; NOTE(jenchieh): Call it multiple time to just in case
    ;; the shell process will run.
    (jcs-ensure-switch-to-buffer-other-window "*shell*")

    ;; Run shell process.
    (shell)

    ;; active truncate line as default for shell window.
    (jcs-disable-truncate-lines)))

;;;###autoload
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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-shell-backspace ()
  "Backspace key in shell mode."
  (interactive)
  (let ((is-shell-text nil))
    ;; First get the current font.
    (save-excursion
      (forward-char -1)
      (setq is-shell-text (jcs-is-current-point-face "comint-highlight-prompt")))

    ;; Decide weather delete the character or not.
    (when (and (not is-shell-text)
               (not (jcs-is-beginning-of-line-p)))
      (backward-delete-char 1))))
