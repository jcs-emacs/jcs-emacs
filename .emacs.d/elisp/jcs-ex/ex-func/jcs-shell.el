;; ========================================================================
;; $File: jcs-shell.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Toggle Shell Mode
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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

    (erase-buffer)

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

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Shell Commands
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-shell-clear-command ()
  "Clear buffer and make new command prompt."
  (interactive)
  (erase-buffer)
  (comint-send-input))

;;;###autoload
(defun jcs-shell-return ()
  "Shell mode's return key."
  (interactive)
  ;; Goto the end of the command line.
  (goto-char (point-max))

  ;; STUDY(jenchieh): This actually does not
  ;; goes to the beginning of line. It actually
  ;; goto the start of the command prompt. Which
  ;; mean we do not have to code ourselves to the
  ;; start of command line.
  ;;
  ;; >>> Image: <<<
  ;;                             ┌─ It will jump to this point.
  ;; ┌─ In general, will goto    │
  ;; │ this point.               │
  ;; ▼                           ▼
  ;; `c:\to\some\example\dir\path>'
  (beginning-of-line)

  (let ((command-start-point nil)
        (command-string ""))
    (setq command-start-point (point))

    ;; Get the string start from command to end of command.
    (setq command-string (buffer-substring command-start-point (point-max)))

    ;; Execute the command.
    (cond ((string= command-string "exit")
           (progn
             ;; Here toggle, actually close the terminal itself.
             (jcs-toggle-shell-window)))
          ((or (string= command-string "clear")
               (string= command-string "cls"))
           (progn
             ;; Clear the terminal once.
             (jcs-shell-clear-command)))
          ;; Else just send the command to terminal.
          (t
           (progn
             ;; Call default return key.
             (comint-send-input))))))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Deletion
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-shell-highlight-face-name "comint-highlight-prompt"
  "Face name in shell mode that we do not want to delete.")

(defun jcs-shell-is-current-on-command ()
  "Return non-nil if current on command line."
  (and (jcs-last-line-in-buffer)
       (not (jcs-is-beginning-of-line-p))
       (not (jcs-is-current-point-face jcs-shell-highlight-face-name))))

;;;###autoload
(defun jcs-shell-backspace ()
  "Backspace key in shell mode."
  (interactive)
  ;; Only the last line of buffer can do deletion.
  (when (jcs-shell-is-current-on-command)
    (backward-delete-char 1)))

;;;###autoload
(defun jcs-shell-kill-whole-line ()
  "Kill whole line in shell mode."
  (interactive)
  ;; Directly jump to the end of the buffer.
  (goto-char (point-max))
  ;; Delete eveything from current command line.
  (while (and (not (jcs-is-current-point-face jcs-shell-highlight-face-name))
              (not (jcs-is-beginning-of-line-p)))
    (backward-delete-char 1)))


;;;###autoload
(defun jcs-shell-backward-delete-word ()
  "Shell mode's version of backward delete word."
  (interactive)
  (when (jcs-shell-is-current-on-command)
    (call-interactively 'jcs-backward-delete-word)))

;;;###autoload
(defun jcs-shell-forward-delete-word ()
  "Shell mode's version of forward delete word."
  (interactive)
  (when (jcs-shell-is-current-on-command)
    (call-interactively 'jcs-forward-delete-word)))


;;;###autoload
(defun jcs-shell-backward-kill-word-capital ()
  "Shell mode's version of forward delete word."
  (interactive)
  (when (jcs-shell-is-current-on-command)
    (call-interactively 'jcs-backward-kill-word-capital)))

;;;###autoload
(defun jcs-shell-forward-kill-word-capital ()
  "Shell mode's version of forward delete word."
  (interactive)
  (when (jcs-shell-is-current-on-command)
    (call-interactively 'jcs-forward-kill-word-capital)))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Navigation
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-shell-up-key ()
  "Shell mode up key."
  (interactive)
  (if (or (jcs-shell-is-current-on-command)
          (jcs-is-end-of-buffer-p))
      (comint-previous-input 1)
    (jcs-previous-line))

  (when (jcs-last-line-in-buffer)
    (goto-char (point-max))))

;;;###autoload
(defun jcs-shell-down-key ()
  "Shell mode down key."
  (interactive)
  (if (or (jcs-shell-is-current-on-command)
          (jcs-is-end-of-buffer-p))
      (comint-next-input 1)
    (jcs-next-line))

  (when (jcs-last-line-in-buffer)
    (goto-char (point-max))))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Completion
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;;###autoload
(defun jcs-company-manual-begin ()
  "Completion for the shell command."
  (interactive)
  (goto-char (point-max))
  ;; Call default completion function.
  (call-interactively #'company-manual-begin))
