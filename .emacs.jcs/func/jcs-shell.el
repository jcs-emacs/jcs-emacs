;;; jcs-shell.el --- Shell functionaitilies.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'multi-shell)


(defvar jcs-shell--last-selected-shell-index -1
  "Record last selected shell.")

(defun jcs-shell-select-shell-by-index (index)
  "Select the shell by index."
  (let ((sp (nth index multi-shell--live-shells)))
    (multi-shell-select (multi-shell--form-name-by-id (car sp)))))

(defun jcs-shell--multi-shell-new-select--advice-after ()
  "Advcie execute after select multiple shell related commands."
  (setq jcs-shell--last-selected-shell-index (multi-shell--get-current-shell-index-by-id)))
(advice-add 'multi-shell :after #'jcs-shell--multi-shell-new-select--advice-after)
(advice-add 'multi-shell-prev :after #'jcs-shell--multi-shell-new-select--advice-after)
(advice-add 'multi-shell-next :after #'jcs-shell--multi-shell-new-select--advice-after)

;;;###autoload
(defun jcs-show-shell-window ()
  "Shell command prompt."
  (interactive)
  (unless (ignore-errors (jcs-jump-shown-to-buffer (multi-shell--prefix-name)))
    (if (multi-shell-live-p)
        (let ((sp-name nil))
          (save-window-excursion
            (setq sp-name (jcs-shell-select-shell-by-index
                           jcs-shell--last-selected-shell-index)))
          (when (window-full-height-p) (jcs-balance-split-window-vertically))
          (jcs-ensure-switch-to-buffer-other-window sp-name))
      (when (window-full-height-p) (jcs-balance-split-window-vertically))
      ;; Move this to make next shell bufer inside the bottom window.
      (progn (other-window -1) (other-window 1))
      (multi-shell))))

;;;###autoload
(defun jcs-hide-shell-window ()
  "Kill process prompt."
  (interactive)
  (jcs-safe-jump-shown-to-buffer
   (multi-shell--prefix-name)
   (lambda ()
     (jcs-delete-window-downwind))
   (lambda ()
     (user-error (format "No \"%s\" buffer found" (multi-shell--prefix-name)))))
  (balance-windows))

;;;###autoload
(defun jcs-maybe-kill-shell ()
  "Maybe kill shell behaviour."
  (interactive)
  (if (ignore-errors (jcs-jump-shown-to-buffer (multi-shell--prefix-name)))
      (let ((kill-win (= 1 (length multi-shell--live-shells))))
        (multi-shell-kill)
        (if kill-win
            (jcs-delete-window-downwind)
          (when (>= (1- jcs-shell--last-selected-shell-index) 0)
            (setq jcs-shell--last-selected-shell-index (1- jcs-shell--last-selected-shell-index)))
          (jcs-shell-select-shell-by-index jcs-shell--last-selected-shell-index)))
    (jcs-maybe-kill-this-buffer)))

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

  ;; STUDY: This actually does not goes to the
  ;; beginning of line. It actually goto the
  ;; start of the command prompt. Which mean
  ;; we do not have to code ourselves to the
  ;; start of command line.
  ;;
  ;; >>> Image: <<<
  ;;                             ┌─ It will jump to this point.
  ;; ┌─ In general, will goto    │
  ;; │ this point.               │
  ;; ▼                           ▼
  ;; `c:\to\some\example\dir\path>'
  (beginning-of-line)

  (let* ((command-start-point (point))
         ;; Get the string start from command to end of command.
         (command-string (buffer-substring command-start-point (point-max))))
    ;; Execute the command.
    (cond
     ((string= command-string "exit")
      ;; Here toggle, actually close the terminal itself.
      (jcs-maybe-kill-shell))
     ((or (string= command-string "clear")
          (string= command-string "cls"))
      ;; Clear the terminal once.
      (jcs-shell-clear-command))
     ;; Else just send the command to terminal.
     (t
      ;; Call default return key.
      (comint-send-input)))))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Deletion
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defvar jcs-shell-highlight-face-name "comint-highlight-prompt"
  "Face name in shell mode that we do not want to delete.")

(defun jcs-shell-is-current-on-command ()
  "Return non-nil if current on command line."
  (let ((is-shell-prompt-char nil))
    (save-excursion
      (backward-char 1)
      (setq is-shell-prompt-char
            (jcs-is-current-point-face jcs-shell-highlight-face-name)))
    (and (jcs-last-line-in-buffer-p)
         (not (jcs-is-beginning-of-line-p))
         (not is-shell-prompt-char))))

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
  (when (jcs-last-line-in-buffer-p)
    (goto-char (point-max))))

;;;###autoload
(defun jcs-shell-down-key ()
  "Shell mode down key."
  (interactive)
  (if (or (jcs-shell-is-current-on-command)
          (jcs-is-end-of-buffer-p))
      (comint-next-input 1)
    (jcs-next-line))
  (when (jcs-last-line-in-buffer-p)
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


(provide 'jcs-shell)
;;; jcs-shell.el ends here
