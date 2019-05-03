;;; jcs-hook.el --- All the hook event do here.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;
;; All hook listed.
;; URL(jenchieh): https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-focus-in-hook ()
  "When window is focus."
  (jcs-revert-all-file-buffers)
  )
(add-hook 'focus-in-hook 'jcs-focus-in-hook)

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

  ;; NOTE(jenchieh): Lower the `GC' back to normal threshold.
  (setq gc-cons-threshold jcs-normal-gc-cons-threshold)

  (save-selected-window
    ;; ATTENTION(jenchieh): First active the correct line number,
    ;; because this would not works in `jcs-after-change-major-mode-hook'.
    (switch-to-buffer "*Messages*")
    (jcs-active-line-numbers-by-mode))
  )
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-post-command-hook ()
  "Hook run after every command."

  ;; Show the last command event id?
  (when jcs-show-last-command-event
    (call-interactively #'jcs-print-last-command-event))

  (when (jcs-is-font-lock-fontify-buffer-mode-p)
    ;; Refresh the syntax highlighting.
    (call-interactively #'jcs-font-lock-fontify-buffer))

  (when (jcs-is-current-major-mode-p "web-mode")
    (when jcs-web-auto-truncate-lines
      (jcs-web-truncate-lines-by-face)))

  (when jcs-marking-whole-buffer
    (setq-local jcs-marking-whole-buffer-cmd-count
                (1+ jcs-marking-whole-buffer-cmd-count))
    (when (>= jcs-marking-whole-buffer-cmd-count 2)
      (deactivate-mark)
      (setq-local jcs-marking-whole-buffer-cmd-count 0)
      (setq-local jcs-marking-whole-buffer nil)))
  )
(add-hook 'post-command-hook 'jcs-post-command-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-after-change-major-mode-hook ()
  "Hook run after major mode changes."
  (unless reload-emacs-reloading
    (jcs-active-line-numbers-by-mode))
  )
(add-hook 'after-change-major-mode-hook 'jcs-after-change-major-mode-hook)


;;-----------------------------------------------------------
;; Minibuffer
;;-----------------------------------------------------------

(defvar jcs-minibuffer-active nil
  "Flag to check if current minibuffer active?")

(add-hook 'minibuffer-setup-hook
          (lambda ()
            ;; Active trigger flag.
            (setq jcs-minibuffer-active t)

            (when (and (not (jcs-current-char-equal-p "/"))
                       ;; SEE(jenchieh): this trigger can be check
                       ;; at `jcs-helm.el' file.
                       jcs-helm-find-files-active)
              ;; NOTE(jenchieh): This will prevent missing the
              ;; slash at the end of the search file path.
              (insert "/"))

            ;; Register hook.
            (add-hook 'post-command-hook #'jcs-minibuffer-post-command-hook nil t)
            ))

(defun jcs-minibuffer-post-command-hook ()
  "Minibuffer post command hook."
  ;; NOTE(jenchieh): reserve usage...
  )

(add-hook 'minibuffer-exit-hook
          (lambda ()
            ;; Deactive trigger flag.
            (setq jcs-minibuffer-active nil)

            (jcs-reload-active-mode)
            ;; NOTE: disable the file after we do close minibuffer.
            (setq jcs-helm-find-files-active nil)

            ;; ATTENTION(jenchieh): no matter what, cancel top level activation
            ;; while minibuffer exit!
            (setq jcs-top-level-active nil)
            ))


(provide 'jcs-hook)
;;; jcs-hook.el ends here
