;;; jcs-hook.el --- All the hook event do here.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
  "Find file hook."
  (unless (jcs-reload-emacs-reloading-p)
    (when (and (jcs-is-contain-list-string jcs-find-file-read-only-paths
                                           (buffer-file-name))
               (not jcs-package-installing))
      (read-only-mode 1))
    (jcs-active-line-numbers-by-mode))
  )
(add-hook 'find-file-hook 'jcs-find-file-hook)

(defun jcs-advice-find-file-after (&rest _args)
  "Advice after execute `find-file' command."
  (jcs-buffer-menu-safe-refresh)
  )
(advice-add 'find-file :after 'jcs-advice-find-file-after)

(defun jcs-advice-switch-to-buffer-after (&rest _args)
  "Advice after execute `switch-to-buffer' command."
  (jcs-buffer-menu-safe-refresh)
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
    (require 'alt-codes)
    (require 'auto-highlight-symbol)
    (require 'beacon)
    (require 'company)
    (require 'dashboard)
    (require 'diminish)
    (require 'dimmer)
    (require 'exec-path-from-shell)
    (require 'helm)
    (require 'highlight-indent-guides)
    (require 'hl-line)
    (require 'hl-todo)
    (require 'powerline)
    (require 'preproc-font-lock)
    (require 'region-occurrences-highlighter)
    (require 'right-click-context)
    (require 'shift-select)
    (require 'use-ttf)
    (require 'which-key)
    (require 'yascroll))

  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  ;; NOTE: Enable util modes here.
  ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (progn
    ;;-------------------------------- `alt-codes'
    (global-alt-codes-mode 1)
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
    (helm-mode 1)
    (helm-autoresize-mode 1)
    ;;-------------------------------- `helm-projectile'
    (helm-projectile-on)
    ;;-------------------------------- `hl-line'
    (global-hl-line-mode 1)
    ;;-------------------------------- `hl-todo'
    (global-hl-todo-mode 1)
    ;;-------------------------------- `powerline'
    (powerline-default-theme)
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
    ;;-------------------------------- `use-ttf'
    (use-ttf-set-default-font)
    ;;-------------------------------- `which-key'
    (which-key-mode))

  (jcs-setup-default-theme)
  (jcs-command-mode)
  (jcs-depend-mode)

  (jcs-reload-file-info)
  (jcs-reload-docstring-info)

  (menu-bar-mode -1)
  (when (display-graphic-p) (scroll-bar-mode -1))
  (tool-bar-mode -1)

   ;; Language Environment
  (set-language-environment jcs-language-environment)

  ;; Font Size
  (set-face-attribute 'default nil :height jcs-default-font-size)

  ;; Frame Title
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  ;; NOTE: Lower the `GC' back to normal threshold.
  (jcs-gc-cons-threshold nil)
  (setq file-name-handler-alist jcs-file-name-handler-alist)

  ;; IMPORTANT: This should always be the last thing.
  (setq dashboard-init-info
        (format "[ %s ] [ Total took %0.1f seconds ]"
                dashboard-init-info
                (string-to-number (emacs-init-time))))
  )
(add-hook 'after-init-hook 'jcs-after-init-hook)

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-post-command-hook ()
  "Hook run after every command."
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
  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (jcs-gc-cons-threshold t))

  (when (and (not (jcs-current-char-equal-p "/"))
             ;; SEE: this trigger can be check at `jcs-helm-func.el' file.
             jcs-helm-find-files-active
             (file-directory-p (thing-at-point 'line t)))
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

  ;; NOTE: Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
  (progn
    (garbage-collect)
    (jcs-gc-cons-threshold nil))
  )
(add-hook 'minibuffer-exit-hook 'jcs-minibuffer-exit-hook)


(provide 'jcs-hook)
;;; jcs-hook.el ends here
