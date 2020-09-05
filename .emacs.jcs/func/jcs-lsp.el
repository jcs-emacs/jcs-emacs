;;; jcs-lsp.el --- LSP function related.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;;; For `lsp-mode'

(defun jcs--lsp-lv-buffer-alive-p ()
  "Check if ` *LV*' buffer alive."
  (get-buffer jcs--lsp-lv-buffer-name))

(defun jcs--lsp--execute-command--advice-around (fnc &rest args)
  "Advice execute around `lsp--execute-command'."
  (let ((jcs--lsp--executing-command t))
    (apply fnc args)))

(advice-add 'lsp--execute-command :around #'jcs--lsp--execute-command--advice-around)

;; Enable or Disable for LSP.

(defun jcs--lsp--stuff-on-enabled ()
  "Do stuff when lsp is enabled."
  (setq debug-on-error nil)  ; TODO: Get rid of this after `lsp-mode' is stabled.
  (jcs-re-enable-mode 'company-fuzzy-mode)
  (lsp-origami-mode 1))

(defun jcs--lsp--stuff-on-disabled ()
  "Do stuff when lsp is disabled."
  (setq debug-on-error t)  ; TODO: Get rid of this after `lsp-mode' is stabled.
  (lsp-origami-mode -1))

(defun jcs--lsp-managed-mode-hook ()
  "LSP managed mode hook."
  (if (and lsp-mode lsp-managed-mode) (jcs--lsp--stuff-on-enabled) (jcs--lsp--stuff-on-disabled)))

(defun jcs--lsp-mode-hook ()
  "LSP mode hook."
  (if lsp-mode (jcs--lsp--stuff-on-enabled) (jcs--lsp--stuff-on-disabled)))

(add-hook 'lsp-managed-mode-hook 'jcs--lsp-managed-mode-hook)
(add-hook 'lsp-mode-hook 'jcs--lsp-mode-hook)

;;----------------------------------------------------------------------------
;;; For `lsp-ui'

(defvar jcs--lsp-ui--doc-timer nil "Self timer to show document.")

(defun jcs--lsp-ui-mode--enabled-p ()
  "Check if `lsp-ui-mode' enabled."
  (and (boundp 'lsp-ui-mode) lsp-ui-mode))

(defun jcs--lsp-ui-doc-stop-timer ()
  "Safe way to stop lsp UI document."
  (when (and (boundp 'lsp-ui-doc--timer) (timerp lsp-ui-doc--timer))
    (cancel-timer lsp-ui-doc--timer)))

(defun jcs--lsp-ui-doc-show-safely ()
  "Safe way to show lsp UI document."
  (setq jcs--lsp-ui--doc-timer (jcs-safe-kill-timer jcs--lsp-ui--doc-timer))
  (setq jcs--lsp-ui--doc-timer
        (run-with-idle-timer
         lsp-ui-doc-delay nil
         (lambda ()
           (if (and
                (jcs--lsp-ui-mode--enabled-p)
                (not jcs--lsp--executing-command)
                (not (jcs-is-command-these-commands this-command
                                                    '(save-buffers-kill-terminal))))
               (ignore-errors (call-interactively #'lsp-ui-doc-show))
             (jcs--lsp-current-last-signature-buffer))))))

(defun jcs--lsp-ui-doc--hide-frame ()
  "Safe way to call `lsp-ui-doc--hide-frame' function."
  (when (and (functionp 'lsp-ui-doc--hide-frame) (not jcs--lsp-lv-recording))
    (lsp-ui-doc--hide-frame)))

;;----------------------------------------------------------------------------
;;; Global Registery

(defun jcs-lsp--focus-in-hook ()
  "When window is focus."
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'focus-in-hook 'jcs-lsp--focus-in-hook)

(defun jcs-lsp--focus-out-hook ()
  "When window is not focus."
  (jcs--lsp-ui-doc-stop-timer)
  (jcs--lsp-ui-doc--hide-frame))
(add-hook 'focus-out-hook 'jcs-lsp--focus-out-hook)

(defun jcs-lsp--other-window--advice-before (&rest _args)
  "Advice execute before `other-window' command."
  (unless jcs--no-advice-other-window
    (jcs--lsp-ui-doc-stop-timer)))
(advice-add 'other-window :before #'jcs-lsp--other-window--advice-before)

(defun jcs-lsp--other-window--advice-after (&rest _args)
  "Advice execute after `other-window' command."
  (unless jcs--no-advice-other-window
    ;;--------------------------------------------------------------------
    ;; NOTE: LSP needs this from stopping me navigate through windows.
    (if (jcs-frame-util-p)
        (cl-case this-command
          ('jcs-other-window-next (jcs-other-window-next))
          ('jcs-other-window-prev (jcs-other-window-prev)))
      ;;--------------------------------------------------------------------
      (jcs--lsp-signature-maybe-stop)
      (jcs--lsp-ui-doc-show-safely))))
(advice-add 'other-window :after #'jcs-lsp--other-window--advice-after)

(defun jcs-lsp--post-command-hook ()
  "Hook run after every command."
  (jcs--lsp-ui-doc--hide-frame)
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'post-command-hook 'jcs-lsp--post-command-hook)


(defvar jcs--lsp-lv-was-alive nil
  "Record ` *LV*' buffer was alive.")

(defvar jcs--lsp-lv-recording nil
  "Check if we are recording ")

(defun jcs-window-size-change-functions (&rest _)
  "When window changed size."
  (when (and (boundp 'lsp-mode) lsp-mode)
    (if (jcs--lsp-lv-buffer-alive-p)
        (setq jcs--lsp-lv-was-alive t)
      (if jcs--lsp-lv-was-alive
          (progn
            (when (jcs--lsp-current-last-signature-buffer)
              (let ((pt (point))) (jcs-window-restore-once) (goto-char pt)))
            (setq jcs--lsp-lv-was-alive nil))
        (let ((jcs--lsp-lv-recording t)) (jcs-window-record-once)))))
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'window-size-change-functions 'jcs-window-size-change-functions)

(provide 'jcs-lsp)
;;; jcs-lsp.el ends here
