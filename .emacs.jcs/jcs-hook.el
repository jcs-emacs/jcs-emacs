;;; jcs-hook.el --- All the hook event do here.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;
;; All hook listed.
;; URL: https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-focus-in-hook ()
  "When window is focus."
  (jcs-revert-all-file-buffers)
  (let ((message-log-max nil)
        (inhibit-message t))
    (jcs-dashboard-refresh-buffer))
  )
(add-hook 'focus-in-hook 'jcs-focus-in-hook)

(defun jcs-focus-out-hook ()
  "When window is not focus."
  )
(add-hook 'focus-out-hook 'jcs-focus-out-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-find-file-hook ()
  "Find file hook."
  (unless (jcs-reload-emacs-reloading-p)
    (when (and (jcs-is-contain-list-string jcs-find-file-read-only-paths
                                           (buffer-file-name))
               (not jcs-package-installing))
      (read-only-mode 1))
    (jcs-buffer-menu-refresh-buffer)
    (jcs-active-line-numbers-by-mode))
  )
(add-hook 'find-file-hook 'jcs-find-file-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-advice-switch-to-buffer-after (&rest args)
  "Advice after execute `switch-to-buffer' command."
  )
(advice-add 'switch-to-buffer :after 'jcs-advice-switch-to-buffer-after)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-after-init-hook ()
  "Hook run after initialize."
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Load required packages here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    (require 'auto-highlight-symbol)
    (require 'beacon)
    (require 'company)
    (require 'dashboard)
    (require 'diminish)
    (require 'dimmer)
    (require 'exec-path-from-shell)
    ;; ATTENTION: Haxe-mode is no longer maintaining...
    ;; Consider remove `haxe-mode' from this config.
    ;;
    ;; NOTE: `haxe-mode' does not autoload, loaded manually.
    (require 'haxe-mode)
    (require 'helm)
    (require 'hl-line)
    (require 'hl-todo)
    (require 'indent-info)
    (require 'line-reminder)
    (require 'powerline)
    (require 'preproc-font-lock)
    (require 'reload-emacs)
    (require 'right-click-context)
    (require 'shift-select)
    (require 'undo-tree)
    (require 'use-ttf)
    (require 'which-key)
    (require 'yascroll))

  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Enable util modes here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    ;;-------------------------------- `auto-highlight-symbol'
    (global-auto-highlight-symbol-mode t)
    ;;-------------------------------- `beacon'
    (beacon-mode 1)
    ;;-------------------------------- `delete-selection'
    (delete-selection-mode 1)
    ;;-------------------------------- `dimmer'
    (dimmer-mode)
    ;;-------------------------------- `goto-address'
    (goto-address-mode t)
    ;;-------------------------------- `helm'
    (progn
      (helm-mode 1)
      ;; NOTE: After enabled `helm-mode', diminish it immediately.
      (diminish 'helm-mode))
    (helm-autoresize-mode 1)
    ;;-------------------------------- `helm-projectile'
    (helm-projectile-on)
    ;;-------------------------------- `hl-line'
    (global-hl-line-mode 1)
    ;;-------------------------------- `hl-todo'
    (global-hl-todo-mode 1)
    ;;-------------------------------- `indent-info'
    (global-indent-info-mode +1)
    ;;-------------------------------- `line-reminder'
    (global-line-reminder-mode t)
    ;;-------------------------------- `powerline'
    (progn
      (powerline-default-theme)
      ;;(powerline-center-theme)
      ;;(powerline-center-evil-theme)
      ;;(powerline-vim-theme)
      ;;(powerline-nano-theme)
      )
    ;;-------------------------------- `preproc-font-lock'
    (preproc-font-lock-global-mode t)
    (preproc-font-lock-mode t)
    ;;-------------------------------- `projectile'
    (projectile-mode t)
    ;;-------------------------------- `right-click-context'
    (right-click-context-mode 1)
    ;;-------------------------------- `shift-select'
    (global-shift-select-mode t)
    ;;-------------------------------- `show-paren'
    ;; NOTE: turn on highlight matching brackets when cursor is on one
    (show-paren-mode t)
    ;;-------------------------------- `sublimity'
    ;; Default on or off?
    ;; NOTE: This also trigger the animate scrolling too.
    (sublimity-mode 1)
    ;;-------------------------------- `use-ttf'
    (use-ttf-set-default-font)
    ;;-------------------------------- `which-key'
    (which-key-mode))

  (jcs-setup-default-theme)
  (jcs-command-mode)
  (jcs-depend-mode)

  (jcs-reload-file-info)
  (jcs-reload-docstring-info)

  ;; Font Size
  (set-face-attribute 'default nil :height jcs-default-font-size)

  ;; Frame Title
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  ;; NOTE: Lower the `GC' back to normal threshold.
  (setq gc-cons-threshold jcs-normal-gc-cons-threshold)

  ;; IMPORTANT: This should always be the last thing.
  (setq dashboard-init-info
        (format "[ %s ] [ Total took %s ]" dashboard-init-info (emacs-init-time)))
  )
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-post-command-hook ()
  "Hook run after every command."

  ;; Show the last command event id?
  (when jcs-show-last-command-event
    (jcs-print-last-command-event))

  (when (jcs-is-font-lock-fontify-buffer-mode-p)
    ;; Refresh the syntax highlighting.
    (jcs-font-lock-fontify-buffer))

  ;; TODO: Move this to `web-mode' only.
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
  (unless (jcs-reload-emacs-reloading-p)
    (jcs-active-line-numbers-by-mode))
  )
(add-hook 'after-change-major-mode-hook 'jcs-after-change-major-mode-hook)


;;-----------------------------------------------------------
;; Minibuffer
;;-----------------------------------------------------------

(defun jcs-minibuffer-setup-hook ()
  "Hook when minibuffer setup."
  (when (and (not (jcs-current-char-equal-p "/"))
             ;; SEE: this trigger can be check at `jcs-helm-func.el' file.
             jcs-helm-find-files-active)
    ;; NOTE: This will prevent missing the
    ;; slash at the end of the search file path.
    (insert "/"))

  ;; Register hook.
  (add-hook 'post-command-hook #'jcs-minibuffer-post-command-hook nil t)
  )
(add-hook 'minibuffer-setup-hook 'jcs-minibuffer-setup-hook)

(defun jcs-minibuffer-post-command-hook ()
  "Minibuffer post command hook."
  ;; NOTE: reserve usage...
  )

(defun jcs-minibuffer-exit-hook ()
  "Hook when exit minibuffer."
  ;; NOTE: disable the file after we do close minibuffer.
  (setq jcs-helm-find-files-active nil)

  (jcs-reload-active-mode)
  )
(add-hook 'minibuffer-exit-hook 'jcs-minibuffer-exit-hook)


(provide 'jcs-hook)
;;; jcs-hook.el ends here
