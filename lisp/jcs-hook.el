;;; jcs-hook.el --- All the hook event do here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Windows" )
;;

(defun jcs-hook--focus-in ()
  "When window is focus."
  (jcs-reload-active-mode)
  (when (featurep 'recentf) (msgu-silent (recentf-cleanup)))
  (jcs-fboundp-apply #'jcs-diff-hl-update)
  (jcs-fboundp-apply #'jcs-buffer-menu-refresh-buffer)
  (jcs-fboundp-apply #'jcs-dashboard-refresh-buffer)
  (jcs-fboundp-apply #'jcs-vertico-refresh))

(defun jcs-hook--focus-out ()
  "When window is not focus."
  (jcs-reload-active-mode))

(defun jcs-hook--after-focus ()
  "Function runs after focusing the frame."
  (if (frame-focus-state) (jcs-hook--focus-in)
    (jcs-hook--focus-out)))

(jcs-add-hook 'window-buffer-change-functions
  (when (and (not (active-minibuffer-window))
             (not (jcs-fboundp-apply #'company--active-p)))
    (jcs-fboundp-apply #'jcs-buffer-menu-refresh-buffer)
    (jcs-fboundp-apply #'jcs-dashboard-refresh-buffer)))

;;
;; (@* "Find Files" )
;;

(jcs-add-hook 'find-file-hook
  (jcs-project-remember)
  (jcs-project--track-open-projects)
  (jcs-lsp-safe-active))

(jcs-advice-add '(delete-file delete-directory) :after
  (when (called-interactively-p 'interactive)
    (run-hooks 'after-focus-change-function)))

;;
;; (@* "Initialization" )
;;

(jcs-add-hook 'after-init-hook
  (jcs-modules-load-entry)
  (jcs-require '(dashboard))
  (use-ttf-set-default-font)
  (jcs-setup-default-theme)
  (jcs-require '(on))
  (message nil))    ; mute at the very end!

(jcs-add-hook 'on-init-ui-hook
  (add-function :after after-focus-change-function #'jcs-hook--after-focus)
  (auto-scroll-bar-mode 1)
  (back-button-mode 1)
  (centaur-tabs-mode 1)
  (context-menu-mode 1)
  (display-time-mode 1)
  (global-hl-line-mode 1)
  (global-hl-todo-mode 1)
  (indent-control-mode 1)
  (jcs-echobar-mode 1)
  (jcs-frametitle-mode 1)
  (jcs-modeline-mode 1)
  (responsive-window-mode 1)
  (vertico-mode 1)
  (window-divider-mode 1)
  (jcs-require '(jcs-edit))
  (message nil))    ; mute at the very end!

(jcs-add-hook 'on-first-input-hook
  (balanced-windows-mode 1)
  (breadcrumb-mode 1)
  (delete-selection-mode 1)
  (diminish-buffer-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (eval-mark-mode 1)
  (ff-guard-mode 1)
  (gcmh-mode 1)
  (global-goto-address-mode 1)
  (guard-lf-mode 1)
  (lsp-smart-req-mode 1)
  (message-clean-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (global-page-break-lines-mode 1)
  (recentf-excl-mode 1)
  (global-region-occurrences-highlighter-mode 1)
  (repos-window-mode 1)
  (transient-mark-mode t)
  (vs-revbuf-mode 1)
  (which-key-mode 1)
  (jcs-module-load jcs-module-preload))

(jcs-add-hook 'on-first-file-hook
  (auto-read-only-mode 1)
  (envrc-global-mode 1)
  (npm-global-mode 1)
  (global-tree-sitter-mode 1)
  (global-so-long-mode 1)
  (global-foldvis-mode 1)  ; Must load after `tree-sitter-mode'.
  (require 'lsp-mode))

(jcs-add-hook 'on-first-project-hook
  (global-diff-hl-mode 1)
  (editorconfig-mode 1)
  (vc-refresh-mode 1))

;;
;; (@* "Input" )
;;

(jcs-add-hook 'pre-command-hook
  (jcs-fboundp-apply #'jcs--er/record-history))

(jcs-add-hook 'post-command-hook
  (jcs-fboundp-apply #'jcs--er/resolve-region)
  (jcs-reload-active-mode))

;;
;; (@* "Modes" )
;;

(jcs-add-hook '(text-mode-hook prog-mode-hook conf-mode-hook)
  (alt-codes-mode 1)
  (auto-close-block-mode 1)
  (auto-highlight-symbol-mode 1)
  (company-mode t)
  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)
  (when elenv-graphic-p
    (highlight-indent-guides-mode 1)
    ;; XXX: Workaround for https://github.com/DarthFennec/highlight-indent-guides/issues/70
    (jcs-re-enable-mode-if-was-enabled #'highlight-indent-guides-mode))
  (highlight-numbers-mode 1)
  (elenv-when-exec "prettier" nil (prettier-mode 1))
  (sideline-mode 1)
  (vs-edit-mode 1)
  (vsc-edit-mode 1)
  (vs-comment-return-mode 1)
  (whitespace-cleanup-mode 1)
  (whole-line-or-region-local-mode 1)
  (yas-minor-mode 1)

  ;; Ensure indentation level is available
  (indent-control-ensure-indentable))

(jcs-add-hook 'prog-mode-hook
  ;; XXX: See the bug https://github.com/immerrr/lua-mode/issues/172
  (unless (jcs-member "-" (list comment-start comment-end) 'regex)
    (modify-syntax-entry ?- "_"))
  (unless (elenv-buffer-use-spaces-p)
    (msgu-inhibit-log
      (message "[INFO] Detect tabs in buffer `%s'; turn on `%s' automatically"
               (propertize (buffer-name) 'face 'font-lock-type-face)
               (propertize "indent-tabs-mode" 'face 'font-lock-type-face)))
    (indent-tabs-mode 1)))

;;
;; (@* "Daemon" )
;;

(when elenv-daemon-p
  (jcs-add-hook 'server-after-make-frame-hook
    (load-library "elenv")
    (run-hooks 'after-init-hook)
    (run-hooks 'on-init-ui-hook)
    (run-hooks 'on-first-input-hook)
    (run-hooks 'on-first-file-hook)
    (run-hooks 'on-first-project-hook)
    (jcs-dashboard)
    (jcs-run-after-load-theme-hook)))

;;
;; (@* "Quitting" )
;;

(jcs-advice-add '(keyboard-quit top-level) :before
  (jcs-fboundp-apply #'jcs-backtrace-exit))

(provide 'jcs-hook)
;;; jcs-hook.el ends here
