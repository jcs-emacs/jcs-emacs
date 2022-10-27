;;; jcs-plugin.el --- Plugin Configurations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf auto-highlight-symbol
  :init
  (setq ahs-idle-interval 0.15))

(leaf auto-read-only
  :init
  (setq auto-read-only-function #'read-only-mode)
  :defer-config
  (nconc auto-read-only-file-regexps
         '("emacs/.*/lisp/"
           "/[.]emacs[.]d/elpa/"))
  (jcs-advice-add 'auto-read-only--hook-find-file :override
    (unless (jcs-project-root) (auto-read-only))))

(leaf auto-scroll-bar
  :init
  (setq auto-scroll-bar-horizontal t
        auto-scroll-bar-disabled-major-modes '(dashboard-mode)))

(leaf balance-windows
  :init
  (setq balanced-windows-commands
        '(delete-window
          quit-window
          split-window-horizontally split-window-vertically)))

(leaf better-scroll
  :init
  (setq better-scroll-align-type 'relative
        better-scroll-allow-boundary-movement t))

(leaf buffer-wrap
  :defer-config
  (defun jcs--buffer-wrap--fixed-window-off ()
    "Fixed windows is off after wrapping."
    (let ((max-ln (+ (line-number-at-pos (point-max)) buffer-wrap--relative-max-line)))
      (when (= max-ln (line-number-at-pos (point)))
        (jcs-recenter-top-bottom 'bottom))))

  (defun jcs--buffer-wrap--fixed-fake-header ()
    "Fixed line offset consider fake header calculation."
    (when tabulated-list-format
      (unless (ignore-errors (tabulated-list-get-entry))
        (cond ((= 0 buffer-wrap--delta-lines)
               (goto-char (point-min)))
              ((< 0 buffer-wrap--delta-lines)
               (ignore-errors (forward-line 1)))
              (t
               (jcs-goto-line (1- (line-number-at-pos (point-max))))))
        (unless (ignore-errors (tabulated-list-get-entry))
          (ignore-errors (forward-line 1))))))

  (jcs-add-hook 'buffer-wrap-post-command-hook
    (jcs--buffer-wrap--fixed-fake-header)
    (jcs--buffer-wrap--fixed-window-off)))

(leaf define-it
  :init
  (setq define-it-output-choice (if elenv-graphic-p 'frame 'view)
        define-it-text-scale-level -2))

(leaf dumb-jump
  :init
  (setq dumb-jump-selector 'completing-read))

(leaf echo-bar
  :init
  (setq echo-bar-right-padding 0
        echo-bar-function
        (lambda ()  ; String to display in echo bar
          (format "%s: %s  %s  %s  %s"
                  (jcs-buffer-spaces-to-tabs)
                  (indent-control-get-indent-level-by-mode)
                  buffer-file-coding-system
                  (show-eol-get-eol-mark-by-system)
                  (format-time-string "%b %d, %Y %H:%M:%S")))
        echo-bar-minibuffer nil))

(leaf electric-cursor
  :init
  (setq electric-cursor-alist '((overwrite-mode . hbar)
                                (t              . box))))

(leaf electric-indent-sexp
  :hook (electric-indent-mode-hook . electric-indent-sexp-mode)
  :init
  (setq electric-indent-sexp-auto-chars t))

(leaf execrun
  :init
  (setq execrun-kill-buffer-function #'jcs-maybe-kill-this-buffer))

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

(leaf flycheck-grammarly      :hook (flycheck-mode-hook . flycheck-grammarly-setup))
(leaf flycheck-languagetool   :hook (flycheck-mode-hook . flycheck-languagetool-setup))
(leaf gitlab-ci-mode-flycheck :hook (flycheck-mode-hook . gitlab-ci-mode-flycheck-enable))

(leaf gcmh
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)))

(leaf google-translate
  :init
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-TW")
  :defer-config
  (jcs-advice-add 'google-translate--search-tkk :override (list 430675 2721866130)))

(leaf goto-char-preview :hook (goto-char-preview-after-hook . jcs--recenter--advice-after))
(leaf goto-line-preview :hook (goto-line-preview-after-hook . jcs--recenter--advice-after))

(leaf highlight-doxygen-mode
  :hook (ts-docstr-mode-hook . highlight-doxygen-mode))

(leaf highlight-numbers
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

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

(leaf line-reminder
  :hook (display-line-numbers-mode-hook
         . (lambda () (line-reminder-mode (if display-line-numbers-mode 1 -1))))
  :init
  (setq line-reminder-show-option (if elenv-graphic-p 'indicators 'linum)
        line-reminder-thumbnail t)
  (unless elenv-graphic-p
    (setq line-reminder-saved-sign " |"
          line-reminder-modified-sign " |")))

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

(leaf minions
  :init
  (setq minions-mode-line-delimiters nil
        minions-mode-line-lighter ""))

(leaf moody
  :init
  (setq x-underline-at-descent-line t)
  :defer-config
  (require 'jcs-disp)
  ;; XXX For issue, https://github.com/tarsius/moody/pull/41
  (jcs-advice-add 'moody-redisplay :around
    (let ((inhibit-redisplay t)) (apply arg0 args)))
  (unless elenv-graphic-p (jcs-advice-add 'moody-tab :override arg0)))

(leaf most-used-words
  :init
  (setq most-used-words-display-type 'table
        most-used-words-word-display 100))

(leaf msgu
  :init
  (setq msgu-sleep-seconds 0.4
        msgu-sit-seconds 100))

(leaf page-break-lines
  :init
  (setq page-break-lines-modes '( browse-kill-ring-mode
                                  emacs-lisp-mode lisp-mode
                                  scheme-mode
                                  outline-mode
                                  help-mode)))

(leaf preview-it
  :init
  (setq preview-it-render-md t))

(leaf project
  :defer-config
  (setq project-vc-ignores
        (append project-vc-ignores
                '(".idea" ".vscode"
                  ".ensime_cache" ".eunit"
                  ".git" ".hg" ".fslckout"
                  "_FOSSIL_" ".bzr" "_darcs"
                  ".tox" ".svn"
                  ".stack-work" ".ccls-cache" ".cache" ".clangd")
                '(".log" ".vs" "node_modules"))))

(leaf quickrun
  :init
  (setq quickrun-focus-p nil
        quickrun-truncate-lines nil))

(leaf recentf-excl
  :init
  (setq recentf-excl-commands '( jcs-goto-definition
                                 jcs-goto-definition-other-window
                                 jcs-peek-definition
                                 ediff-find-file)))

(leaf region-occurrences-highlighter
  :init
  (setq region-occurrences-highlighter-min-size 1))

(leaf region-state
  :hook (activate-mark-hook . region-state-mode))

(leaf right-click-context
  :defer-config
  (jcs-advice-add 'right-click-context-menu :override
    ;; Open Right Click Context menu.
    (let ((popup-menu-keymap (copy-sequence popup-menu-keymap)))
      (define-key popup-menu-keymap [mouse-3] #'right-click-context--click-menu-popup)
      (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree) nil))))
        (when (and (jcs-popup-clicked-on-menu-p) value)
          (if (symbolp value) (call-interactively value t) (eval value)))))))

(leaf show-eol
  :defer-config
  (jcs-advice-add 'show-eol-enable :before
    (face-remap-add-relative 'whitespace-newline :inverse-video t))
  (jcs-advice-add 'show-eol-disable :before
    (face-remap-add-relative 'whitespace-newline :inverse-video nil)))

(leaf sideline
  :hook ((flycheck-mode-hook . sideline-mode)
         (flymake-mode-hook  . sideline-mode))
  :init
  (setq sideline-delay 0.2
        sideline-backends-left '((sideline-color . up))
        sideline-backends-right '((sideline-lsp      . up)
                                  (sideline-flycheck . down)
                                  (sideline-flymake  . down))
        sideline-display-backend-name t
        sideline-display-backend-type 'inner))

(leaf sideline-flycheck :hook (flycheck-mode-hook . sideline-flycheck-setup))

(leaf turbo-log
  :init
  (setq turbo-log-allow-insert-without-tree-sitter-p t))

(leaf use-ttf
  :init
  (setq use-ttf-default-ttf-fonts
        (mapcar (lambda (file) (concat user-emacs-directory file))
                '("fonts/clacon.ttf"
                  "fonts/DejaVuSans.ttf"
                  "fonts/DejaVuSansMono.ttf"
                  "fonts/NotoSans-Regular.ttf"
                  "fonts/NotoSansSymbols-Regular.ttf"
                  "fonts/Quivira.otf"
                  "fonts/Symbola.otf"  ; for unicode
                  "fonts/UbuntuMono-R.ttf"))
        use-ttf-default-ttf-font-name "Ubuntu Mono"))

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

(leaf whitespace-cleanup-mode
  :init
  (setq whitespace-cleanup-mode-preserve-point t
        whitespace-cleanup-mode-only-if-initially-clean nil
        whitespace-cleanup-mode-ignore-modes
        '( special-mode comint-mode cider-repl-mode haskell-interactive-mode
           text-mode markdown-mode org-mode
           conf-javaprop-mode ini-mode
           view-mode diff-mode
           snippet-mode)))

(leaf winum
  :init
  (setq winum-scope 'frame-local))

(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
