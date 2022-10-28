;;; jcs-plugin.el --- Plugin Configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf balance-windows
  :init
  (setq balanced-windows-commands
        '( delete-window quit-window
           split-window-horizontally split-window-vertically)))

(leaf file-header
  :init
  (setq file-header-template-config-filepath (concat user-emacs-directory "templates/config.properties")
        file-header-template-dir (concat user-emacs-directory "templates/")))

(leaf flx
  :defer-config
  (flx-rs-load-dyn)
  (advice-add 'flx-score :override #'flx-rs-score))

(leaf flycheck
  :init
  (setq flycheck-display-errors-function nil))

(leaf google-translate
  :init
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-TW")
  :defer-config
  (jcs-advice-add 'google-translate--search-tkk :override (list 430675 2721866130)))

(leaf goto-char-preview :hook (goto-char-preview-after-hook . jcs--recenter--advice-after))
(leaf goto-line-preview :hook (goto-line-preview-after-hook . jcs--recenter--advice-after))

(leaf isearch
  :hook
  ((isearch-mode-hook     . better-scroll-revert)
   (isearch-mode-end-hook . better-scroll-setup))
  :init
  (setq isearch-lazy-count t
        lazy-count-prefix-format "[%s:%s] "))

(leaf isearch-project
  :init
  (setq isearch-project-ignore-paths '("bin/"
                                       "build/"
                                       "build.min/"
                                       "res/"))
  :defer-config
  (jcs-add-hook 'isearch-mode-hook
    ;; Paste the current symbol when `isearch' enabled.
    (cond ((use-region-p)
           (progn
             (deactivate-mark)
             (ignore-errors
               (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))))
          ((memq this-command '(jcs-isearch-project-backward-symbol-at-point))
           (when (char-or-string-p isearch-project--thing-at-point)
             (backward-word 1)
             (isearch-project--isearch-yank-string isearch-project--thing-at-point)
             (isearch-repeat-backward))))))

(leaf message-clean-mode
  :init
  (setq message-clean-mode-mute-commands '( push-mark set-mark-command)
        message-clean-mode-echo-commands
        '( mwheel-scroll
           previous-line next-line
           vsc-edit-beginning-of-line vsc-edit-end-of-line
           mark-whole-buffer
           indent-region
           browse-kill-ring-setup
           isearch-done
           undefined
           toggle-truncate-lines
           define-it
           jcs-package-upgrade-all jcs-package--show-upgrades jcs-package-autoremove
           lsp--message)
        message-clean-mode-minor-mode 'echo))

(leaf most-used-words
  :init
  (setq most-used-words-display-type 'table
        most-used-words-word-display 100))

(leaf msgu
  :init
  (setq msgu-sleep-seconds 0.4
        msgu-sit-seconds 100))

(leaf recentf-excl
  :init
  (setq recentf-excl-commands '( jcs-goto-definition
                                 jcs-goto-definition-other-window
                                 jcs-peek-definition
                                 ediff-find-file)))

(leaf right-click-context
  :defer-config
  (jcs-advice-add 'right-click-context-menu :override
    ;; Open Right Click Context menu.
    (let ((popup-menu-keymap (copy-sequence popup-menu-keymap)))
      (define-key popup-menu-keymap [mouse-3] #'right-click-context--click-menu-popup)
      (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree) nil))))
        (when (and (jcs-popup-clicked-on-menu-p) value)
          (if (symbolp value) (call-interactively value t) (eval value)))))))

(leaf turbo-log
  :init
  (setq turbo-log-allow-insert-without-tree-sitter-p t))

(leaf vs-revbuf
  :init
  (setq vs-revbuf-ask-unsaved-changes-only t))

(leaf which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-dont-use-unicode t))

(leaf winum
  :init
  (setq winum-scope 'frame-local))

(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
