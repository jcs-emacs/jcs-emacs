;;; jcs-hook.el --- All the hook event do here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Windows" )
;;

(defun jcs-hook--focus-in ()
  "When window is focus."
  (jcs-reload-active-mode)
  (jcs-dashboard-refresh-buffer))

(defun jcs-hook--focus-out ()
  "When window is not focus."
  (jcs-reload-active-mode))

(add-function
 :after after-focus-change-function
 (lambda () (if (frame-focus-state) (jcs-hook--focus-in) (jcs-hook--focus-out))))

(jcs-add-hook 'window-state-change-hook
  (when (and (not (active-minibuffer-window))
             (not (jcs-funcall-fboundp #'company--active-p)))
    (jcs-funcall-fboundp #'jcs-buffer-menu-refresh-buffer)
    (jcs-dashboard-refresh-buffer)
    (when echo-bar-mode (jcs-funcall-fboundp #'echo-bar-update))))

;;
;; (@* "Find Files" )
;;

(jcs-add-hook 'find-file-hook
  (jcs-project-remember)
  (jcs-project--track-open-projects)
  (jcs--safe-lsp-active))

;;
;; (@* "Initialization" )
;;

(jcs-add-hook 'after-init-hook
  (jcs-require '(dashboard on))
  (use-ttf-set-default-font)
  (jcs-setup-default-theme))

(jcs-add-hook 'on-init-ui-hook
  (auto-scroll-bar-mode 1)
  (echo-bar-mode 1)
  (global-hl-line-mode 1)
  (global-hl-todo-mode 1)
  (indent-control-mode 1)
  (jcs-modeline-mode 1)
  (right-click-context-mode 1)
  (vertico-mode 1)
  (jcs-require '(jcs-edit))
  (message nil))  ; mute at the very end!

(jcs-add-hook 'on-first-input-hook
  (balanced-windows-mode 1)
  (global-company-mode t)
  (delete-selection-mode 1)
  (diminish-buffer-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (eval-mark-mode 1)
  (ff-guard-mode 1)
  (gcmh-mode 1)
  (global-goto-address-mode 1)
  (message-clean-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (global-page-break-lines-mode 1)
  (recentf-excl-mode 1)
  (global-region-occurrences-highlighter-mode 1)
  (repos-window-mode 1)
  (transient-mark-mode t)
  (vs-revbuf-mode 1)
  (global-vsc-edit-mode 1)
  (which-key-mode 1)
  (global-whitespace-cleanup-mode 1)
  (whole-line-or-region-global-mode 1)
  (jcs-module-load '("emacs/buffer-menu" "tools/lookup")))

(jcs-add-hook 'on-first-file-hook
  (auto-read-only-mode 1)
  (global-tree-sitter-mode 1)
  (global-so-long-mode 1)
  (require 'lsp-mode))

(jcs-add-hook 'jcs-on-project-hook
  (global-diff-hl-mode 1)
  (editorconfig-mode 1)
  (global-prettier-mode 1))

;;
;; (@* "Input" )
;;

(jcs-add-hook 'pre-command-hook
  (jcs-funcall-fboundp #'jcs--er/record-history))

(jcs-add-hook 'post-command-hook
  (jcs-funcall-fboundp #'jcs--er/resolve-region)
  (jcs-reload-active-mode))

;;
;; (@* "Modes" )
;;

(jcs-add-hook 'diff-mode-hook
  (jcs-key-local
    `(((kbd "M-k") . jcs-maybe-kill-this-buffer)
      ((kbd "M-K") . jcs-reopen-this-buffer))))

(jcs-add-hook '(text-mode-hook prog-mode-hook conf-mode-hook)
  (alt-codes-mode 1)
  (auto-highlight-symbol-mode 1)
  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)
  (when elenv-graphic-p (highlight-indent-guides-mode 1))
  (highlight-numbers-mode 1)
  (indent-control-ensure-tab-width)  ; Ensure indentation level is available
  (yas-minor-mode 1)

  (when (jcs-funcall-fboundp #'jcs-project-under-p)
    (run-hooks 'jcs-on-project-hook)))

(jcs-add-hook 'prog-mode-hook
  ;; XXX: See the bug https://github.com/immerrr/lua-mode/issues/172
  (unless (jcs-contain-list-type-str "-" (list comment-start comment-end) 'regex)
    (modify-syntax-entry ?- "_")))

(jcs-add-hook 'conf-mode-hook
  (setq-local electric-pair-open-newline-between-pairs nil))

;;
;; (@* "Quitting" )
;;

(jcs-advice-add '(keyboard-quit top-level) :before
  (deactivate-mark)  ; disable region
  (jcs-funcall-fboundp #'jcs-backtrace-exit))

(provide 'jcs-hook)
;;; jcs-hook.el ends here
