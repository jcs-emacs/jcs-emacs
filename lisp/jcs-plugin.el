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

(leaf buffer-menu-filter
  :init
  (setq buffer-menu-filter-delay 0.2))

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

(leaf centaur-tabs
  :init
  (setq centaur-tabs-set-icons nil
        centaur-tabs-style "wave"
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"))

(leaf company
  :hook (company-mode-hook . (lambda () (require 'jcs-company)))
  :init
  (setq company-frontends '(company-pseudo-tooltip-frontend)
        company-require-match nil
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-eclim-auto-save nil
        company-minimum-prefix-length 0
        company-idle-delay 0.07
        company-selection-wrap-around 'on
        company-format-margin-function #'company-detect-icons-margin)
  (setq company-backends
        '( company-capf company-semantic
           company-keywords
           company-abbrev company-dabbrev company-dabbrev-code
           company-files
           company-etags company-gtags
           company-yasnippet))
  :defer-config
  (unless elenv-graphic-p
    (push 'company-echo-metadata-frontend company-frontends)))

(leaf company-box
  :hook (company-mode-hook . company-box-mode)
  :init
  (setq company-box-backends-colors nil
        company-box-frame-behavior 'point
        company-box-scrollbar 'right
        company-box-doc-delay 0.3
        company-box-doc-text-scale-level -2))

(leaf company-emojify
  :init
  (setq company-emojify-annotation (if elenv-graphic-p 'image 'unicode)
        company-emojify-emoji-styles '(github)))

(leaf company-fuzzy
  :hook (company-mode-hook . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

(leaf dashboard
  :init
  (setq dashboard-banner-logo-title "[J C S • E M A C S]"
        dashboard-footer-icon ""
        dashboard-footer-messages
        `(,(format "Copyright (c) %s %s" (format-time-string "%Y") "Shen, Jen-Chieh"))
        dashboard-items '((ls-directories . 5)
                          (ls-files       . 5)
                          (recents        . 5)
                          (projects       . 5)
                          ;;(bookmarks      . 5)
                          ;;(agenda         . 5)
                          ;;(registers      . 5)
                          )
        dashboard-item-shortcuts '((recents        . "r")
                                   (bookmarks      . "m")
                                   (projects       . "p")
                                   (agenda         . "a")
                                   (registers      . "e")
                                   (ls-directories . "d")
                                   (ls-files       . "f"))
        dashboard-center-content t
        dashboard-set-navigator nil
        dashboard-projects-backend 'project-el
        ;; Truncate style
        dashboard-path-style 'truncate-middle
        dashboard-recentf-show-base 'align
        dashboard-projects-show-base 'align
        dashboard-bookmarks-show-base 'align
        dashboard-bookmarks-item-format "%s  %s"
        dashboard-shorten-by-window-width t
        dashboard-shorten-path-offset 15)
  :defer-config
  (jcs-require '(project jcs-dashboard dashboard-ls))

  (jcs-add-hook 'jcs-after-load-theme-hook
    (setq dashboard-startup-banner (jcs-dashboard--get-banner-path))
    (when (bound-and-true-p jcs-emacs-startup-directory)
      (jcs-dashboard-refresh-buffer)))

  (dashboard-setup-startup-hook))

(leaf define-it
  :init
  (setq define-it-output-choice (if elenv-graphic-p 'frame 'view)
        define-it-text-scale-level -2))

(leaf diff-hl
  :hook (find-file    . diff-hl-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :init
  (setq diff-hl-side 'right
        diff-hl-draw-borders nil
        diff-hl-flydiff-delay 0.5
        ;; UX: get realtime feedback in diffs after staging/unstaging hunks
        diff-hl-show-staged-changes nil))

(leaf diminish-buffer
  :init
  (setq diminish-buffer-list
        '( "[*]jcs"  ; config wise
           "[*]helm" "[*]esup-" "[*]quelpa-"
           "[*]compilation" "[*]output" "[*]execrun"
           "[*]quickrun"
           "[*]Apropos[*]" "[*]Backtrace[*]" "[*]Compile-Log[*]"
           "[*]Help[*]" "[*]Bug Help[*]"
           "[*]Warnings[*]"
           "[*]VC-history[*]"
           "[*]CPU-Profiler-Report" "[*]Memory-Profiler-Report"
           "[*]Process List[*]"
           "[*]Checkdoc " "[*]Elint[*]" "[*]Package-Lint[*]" "[*]relint[*]"
           "[*]Finder[*]"
           "[*]Async Shell Command[*]" "[*]shell" "[*]eshell" "bshell<"
           "[*]eww" "[*]ESS[*]"
           "[*]emacs[*]"  ; From `async'
           ;; `lsp-mode'
           "[*]lsp-" "[*]LSP[ ]+"
           "[*][a-zA-Z0-9]+[-]*ls" "[*][a-zA-Z0-9]+::stderr[*]"
           "[*]csharp[*]"
           "[*]rust-analyzer[*:]"
           "[*]tcp-server-sonarlint"
           "[*]pyright[*]"
           "[*]tree-sitter" "tree-sitter-tree:"
           "[*]company"
           "[*]editorconfig"
           "[*]prettier"
           "[*]Local Variables[*]"
           "[*]Kill Ring[*]"  ; From `browse-kill-ring'
           "[*]SPEEDBAR"
           "[*]Flycheck" "[*]Flymake log[*]"
           "[*]httpd[*]"
           "[*]helpful" "[*]suggest[*]"
           "[*]ert[*]" "[*]indent-lint"
           "[*]elfeed-"
           "magit[-]*[[:ascii:]]*[:]"  ; From `magit'
           "[*]Most used words[*]"
           "[*]Test SHA[*]"
           "[*]RE-Builder"
           "[*]define-it: tooltip[*]" "[*]preview-it" "[*]gh-md"
           "[*]wclock[*]"
           "[*]Clippy[*]"
           "[*]CMake Temporary[*]"
           "[*]org-src-fontification"
           "[*]ASCII[*]"
           "[*]npm:" "[*]hexo"
           "[*]Flutter")
        diminish-buffer-mode-list '( "dired-mode"
                                     "shell-mode" "eshell-mode")))

(leaf diredfl
  :hook (dired-mode-hook . diredfl-mode))

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

(leaf editorconfig
  :init
  (setq editorconfig-trim-whitespaces-mode 'whitespace-cleanup-mode))

(leaf elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(leaf electric-cursor
  :init
  (setq electric-cursor-alist '((overwrite-mode . hbar)
                                (t              . box))))

(leaf electric-indent-sexp
  :hook (electric-indent-mode-hook . electric-indent-sexp-mode)
  :init
  (setq electric-indent-sexp-auto-chars t))

(leaf elfeed
  :hook (elfeed-search-mode-hook . buffer-wrap-mode)
  :init
  (setq elfeed-db-directory (concat user-emacs-directory ".elfeed")
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'delete-window
        elfeed-feeds '(("https://planet.emacslife.com/atom.xml" planet emacslife)
                       ("http://www.masteringemacs.org/feed/" mastering)
                       ("https://oremacs.com/atom.xml" oremacs)
                       ("https://pinecast.com/feed/emacscast" emacscast)
                       ("https://emacstil.com/feed.xml" Emacs TIL))))

(leaf emojify
  :init
  (setq emojify-emoji-styles '(github)
        emojify-company-tooltips-p t))

(leaf eshell-syntax-highlighting
  :hook (eshell-mode-hook . eshell-syntax-highlighting-global-mode))

(leaf exec-path-from-shell
  :defer-config
  (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

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

(leaf flycheck-eask           :hook (flycheck-mode-hook . flycheck-eask-setup))
(leaf flycheck-elsa           :hook (flycheck-mode-hook . flycheck-elsa-setup))
(leaf flycheck-grammarly      :hook (flycheck-mode-hook . flycheck-grammarly-setup))
(leaf flycheck-languagetool   :hook (flycheck-mode-hook . flycheck-languagetool-setup))
(leaf flycheck-ocaml          :hook (flycheck-mode-hook . flycheck-ocaml-setup))
(leaf flycheck-package        :hook (flycheck-mode-hook . flycheck-package-setup))
(leaf flycheck-relint         :hook (flycheck-mode-hook . flycheck-relint-setup))
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

(leaf highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-suppress-auto-error t))

(leaf highlight-numbers
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(leaf hl-todo
  :init
  (setq hl-todo-highlight-punctuation "")
  :defer-config
  (require 'asoc)
  (asoc-put! hl-todo-keyword-faces "TODO" "red" t)
  (asoc-put! hl-todo-keyword-faces "NOTE" "dark green" t)
  (asoc-put! hl-todo-keyword-faces "TEMP" "turquoise" t)
  (asoc-put! hl-todo-keyword-faces "FIXME" "red" t)
  (nconc hl-todo-keyword-faces
         '(("ATTENTION"   . "red")
           ("STUDY"       . "yellow")
           ("IMPORTANT"   . "yellow")
           ("CAUTION"     . "yellow")
           ("OPTIMIZE"    . "yellow")
           ("DESCRIPTION" . "dark green")
           ("TAG"         . "dark green")
           ("OPTION"      . "dark green")
           ("DEBUG"       . "turquoise")
           ("DEBUGGING"   . "turquoise")
           ("TEMPORARY"   . "turquoise")
           ("SOURCE"      . "PaleTurquoise2")
           ("URL"         . "PaleTurquoise2")
           ("IDEA"        . "green yellow")
           ("OBSOLETE"    . "DarkOrange3")
           ("DEPRECATED"  . "DarkOrange3")
           ("TOPIC"       . "slate blue")
           ("SEE"         . "slate blue")))
  (advice-add #'hl-todo--inside-comment-or-string-p :override #'jcs-inside-comment-or-string-p)
  (advice-add #'hl-todo-previous :after #'jcs--recenter--advice-after)
  (advice-add #'hl-todo-next :after #'jcs--recenter--advice-after))

(leaf impatient-showdown
  :init
  (setq impatient-showdown-flavor 'github))

(leaf isearch
  :hook
  ((isearch-mode-hook     . jcs-scroll-conservatively-disable)
   (isearch-mode-end-hook . jcs-scroll-conservatively-enable))
  :init
  (setq isearch-lazy-count t
        lazy-count-prefix-format "[%s:%s] "))

(leaf isearch-project
  :init
  (setq isearch-project-ignore-paths '(".vs/"
                                       ".vscode/"
                                       "bin/"
                                       "build/"
                                       "build.min/"
                                       "node_modules/"
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

(leaf keypression
  :defer-config
  (nconc keypression-ignore-mouse-events
         '(switch-frame menu-bar tool-bar tab-bar)))

(leaf line-reminder
  :hook (display-line-numbers-mode-hook
         . (lambda () (line-reminder-mode (if display-line-numbers-mode 1 -1))))
  :init
  (setq line-reminder-show-option (if elenv-graphic-p 'indicators 'linum)
        line-reminder-thumbnail t)
  (unless elenv-graphic-p
    (setq line-reminder-saved-sign " |"
          line-reminder-modified-sign " |")))

(leaf lsp-mode
  :hook ((lsp-managed-mode-hook
          . (lambda (&rest _)
              (if (and lsp-mode lsp-managed-mode) (jcs-lsp--enable)
                (jcs-lsp--disable))))
         (lsp-mode-hook
          . (lambda (&rest _)
              (if lsp-mode (jcs-lsp--enable)
                (jcs-lsp--disable)))))
  :init
  (setq lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-keep-workspace-alive nil                      ; Auto-kill LSP server
        lsp-prefer-flymake nil                            ; Use lsp-ui and flycheck
        flymake-fringe-indicator-position 'right-fringe)

  (defun jcs--lsp-connected-p ()
    "Return non-nil if LSP connected."
    (bound-and-true-p lsp-managed-mode))

  (defun jcs--safe-lsp-active ()
    "Safe way to active LSP."
    (when (and (jcs-project-under-p) (not (jcs--lsp-connected-p)))
      (if (memq this-command '(jcs-reopen-this-buffer))
          (lsp) (lsp-deferred))))
  :defer-config
  (defun jcs-lsp--enable ()
    "Do stuff when lsp is enabled."
    (jcs-re-enable-mode 'company-fuzzy-mode)
    ;; enable semantic meaning
    (setq-local company-fuzzy-passthrough-backends '(company-capf)))

  (defun jcs-lsp--disable ()
    "Do stuff when lsp is disabled."
    (setq-local company-fuzzy-passthrough-backends nil)))

(leaf lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-emmet-completions t))

(leaf lsp-ui
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-text-scale-level -1
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.6
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-cursor nil
        lsp-eldoc-enable-hover nil
        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t)
  :defer-config
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (lsp-ui-sideline-set-default-icon))

(leaf marginalia
  :init
  (setq marginalia-align 'right))

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

(leaf minimap
  :init
  (setq minimap-width-fraction 0.1
        minimap-minimum-width 10
        minimap-window-location 'right))

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

(leaf multi-shell
  :init
  (setq multi-shell-prefer-shell-type 'shell))  ; Accept `shell' or `eshll'.

(leaf multiple-cursors
  :init
  (defconst jcs-mc/cancel-commands
    '( block-travel-down block-travel-up
       jcs-isearch-backward-symbol-at-point
       isearch-forward-symbol-at-point
       jcs-isearch-repeat-backward
       jcs-isearch-repeat-forward
       jcs-isearch-project-backward-symbol-at-point
       isearch-project-forward-symbol-at-point
       jcs-isearch-project-repeat-backward
       jcs-isearch-project-repeat-forward)
    "List of commands that will quite `multiple-cursors' after execution.")

  (defun jcs-mc/cancel-multiple-cursors (&rest _)
    "Cancel the `multiple-cursors' behaviour."
    (when (and (functionp 'mc/num-cursors) (> (mc/num-cursors) 1))
      (mc/keyboard-quit)))

  (dolist (cmd jcs-mc/cancel-commands)
    (advice-add cmd :after #'jcs-mc/cancel-multiple-cursors))
  :defer-config
  (defun jcs--mc/mark-lines (num-lines direction)
    "Override `mc/mark-lines' function."
    (let ((cur-column (current-column)))
      (dotimes (i (if (= num-lines 0) 1 num-lines))
        (mc/save-excursion
         (let ((furthest-cursor (cl-ecase direction
                                  (forwards  (mc/furthest-cursor-after-point))
                                  (backwards (mc/furthest-cursor-before-point)))))
           (when (overlayp furthest-cursor)
             (goto-char (overlay-get furthest-cursor 'point))
             (when (= num-lines 0)
               (mc/remove-fake-cursor furthest-cursor))))
         (cl-ecase direction
           (forwards (next-logical-line 1 nil))
           (backwards (previous-logical-line 1 nil)))
         (move-to-column cur-column)
         (mc/create-fake-cursor-at-point)))))
  (advice-add 'mc/mark-lines :override #'jcs--mc/mark-lines))

(leaf page-break-lines
  :init
  (setq page-break-lines-modes '( browse-kill-ring-mode
                                  emacs-lisp-mode lisp-mode
                                  scheme-mode
                                  outline-mode
                                  help-mode)))

(leaf popup
  :defer-config
  (defvar jcs-popup-mouse-events-flag-p nil
    "Check if `popup-menu-item-of-mouse-event' is called.")

  (defvar jcs-popup-selected-item-flag-p nil
    "Check if `popup-selected-item' is called.")

  (defun jcs-popup-clicked-on-menu-p ()
    "Check if the user actually clicked on the `popup' object."
    (and jcs-popup-mouse-events-flag-p
         (not jcs-popup-selected-item-flag-p)))

  (jcs-advice-add 'popup-menu-item-of-mouse-event :after
    (setq jcs-popup-mouse-events-flag-p t
          jcs-popup-selected-item-flag-p nil))

  (jcs-advice-add 'popup-selected-item :after
    (setq jcs-popup-selected-item-flag-p (jcs-last-input-event-p "mouse-1")))

  (jcs-advice-add 'popup-draw :around
    (if (and (jcs-last-input-event-p "mouse-1")
             (not (jcs-popup-clicked-on-menu-p)))
        (keyboard-quit)
      (apply arg0 args))))

(leaf pos-tip
  :init
  (setq pos-tip-internal-border-width 5))

(leaf prettier
  :config
  ;; XXX: Stop displaying the error when `prettier' is not installed!
  (unless (executable-find "prettier")
    (setq prettier-prettify-on-save-flag nil)))

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
  (setq recentf-excl-commands '(jcs-goto-definition
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

(leaf shell-pop
  :init
  (setq shell-pop-window-size 60
        shell-pop-last-shell-buffer-index 0
        shell-pop-shell-type '("shell" "*shell: <>*" (lambda () (multi-shell))))
  :defer-config
  (require 'multi-shell))

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

(leaf tree-sitter-langs
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :defer-config
  (defun jcs--tree-sitter-grab-queries ()
    "Download all custom queries to the `tree-sitter-langs' queries folder."
    (require 'find-func)
    (let* ((default-directory (file-name-directory (find-library-name "tree-sitter-langs")))
           (repo "https://github.com/jcs-emacs/tree-sitter-queries")
           (repo-url (shell-quote-argument repo))
           (dirname (file-name-base repo))
           (clone-dir (expand-file-name dirname))
           (clone-queries (expand-file-name "queries" clone-dir))
           (dest-queries (expand-file-name "queries" default-directory))
           lang-dirs)
      (ignore-errors (delete-directory (expand-file-name dirname default-directory) t))
      (when (= 0 (shell-command (format "git clone %s" repo-url)))
        (setq lang-dirs (directory-files clone-queries))
        (pop lang-dirs) (pop lang-dirs)  ; remove . and ..
        (message "Installing custom tree-sitter query...")
        (dolist (lang-dir lang-dirs)
          (message "  - %s" lang-dir)
          (ignore-errors
            (delete-directory (expand-file-name lang-dir clone-queries)))
          (ignore-errors
            (copy-directory (expand-file-name lang-dir clone-queries)
                            (expand-file-name lang-dir dest-queries)
                            nil nil t)))
        (delete-directory clone-dir t)
        (message "Done install custom tree-sitter queries"))))

  (defun jcs--tree-sitter-hl-mode-hook ()
    "Hook for `tree-sitter-hl-mode'."
    (remove-hook 'tree-sitter-hl-mode-hook #'jcs--tree-sitter-hl-mode-hook)
    (jcs--tree-sitter-grab-queries)
    (tree-sitter-hl-mode 1))  ; re-enable it once
  (add-hook 'tree-sitter-hl-mode-hook #'jcs--tree-sitter-hl-mode-hook))

(leaf ts-docstr
  :hook (tree-sitter-after-on-hook . ts-docstr-mode)
  :init
  (setq ts-docstr-key-support t
        ts-docstr-desc-summary ""))

(leaf ts-fold
  :hook (tree-sitter-after-on-hook . ts-fold-indicators-mode)
  :init
  (setq ts-fold-indicators-fringe 'left-fringe
        ts-fold-indicators-face-function
        (lambda (pos &rest _)
          ;; Return the face of it's function.
          (line-reminder--get-face (line-number-at-pos pos t))))
  :defer-config
  (require 'line-reminder)
  (jcs-advice-add 'line-reminder-transfer-to-saved-lines :after
    ;; Refresh indicators for package `ts-fold'.
    (ts-fold-indicators-refresh)))

(leaf treemacs
  :init
  (setq treemacs-position 'right
        treemacs-missing-project-action 'remove
        treemacs-sorting 'alphabetic-asc
        treemacs-follow-after-init t
        treemacs-no-png-images t)
  :defer-config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (defvar jcs-treemacs-width-ratio 0.15
    "Ratio that respect to `frame-width' and `neo-window-width'.")

  (defun jcs-treemacs-toggle-refresh ()
    "Refresh `treemacs' by toggle twice."
    (save-selected-window (treemacs) (treemacs)))

  (jcs-add-hook 'window-size-change-functions
    (setq treemacs-width (round (* (frame-width) jcs-treemacs-width-ratio)))
    (when (treemacs-get-local-window) (jcs-treemacs-toggle-refresh)))

  (jcs-add-hook 'treemacs-mode-hook
    (setq buffer-wrap--relative-max-line 0)
    (buffer-wrap-mode 1)))

(leaf turbo-log
  :init
  (setq turbo-log-allow-insert-without-tree-sitter-p t))

(leaf undo-tree
  :init
  (setq undo-tree-auto-save-history nil)
  :defer-config
  (global-undo-tree-mode t))

(leaf undo-tree-vf :hook (undo-tree-mode-hook . undo-tree-vf-mode))

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

(leaf vertico
  :hook
  ((rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
   (vertico-mode-hook . vertico-flx-mode))
  :init
  (setq vertico-cycle t
        vertico-resize t
        vertico-scroll-margin 0)
  :defer-config
  (require 'jcs-vertico))

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

(leaf yasnippet
  :init
  (setq yas-verbosity 0))

(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
