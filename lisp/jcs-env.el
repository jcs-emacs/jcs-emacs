;;; jcs-env.el --- Environment Settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Optimizations" )
;;

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))

;;
;; (@* "Environment" )
;;

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(elenv-with-windows
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

(elenv-with-bsd
  (setq mac-command-modifier 'meta
        select-enable-clipboard t
        aquamacs-save-options-on-quit 0
        special-display-regexps nil
        special-display-buffer-names nil
        mac-command-key-is-meta t
        mac-pass-command-to-system nil))

;;
;; (@* "Settings" )
;;

;;
;;; Audo Saving
(setq auto-save-default nil
      auto-save-interval 0
      auto-save-list-file-prefix nil
      auto-save-timeout 0)

;;
;;; Comments
(use-package newcomment
  :init
  (setq comment-inline-offset 2))

;;
;;; Custom
(setq custom-safe-themes t)

;;
;;; Major Mode
(setq-default major-mode 'text-mode)

;;
;;; Deletion
(setq delete-by-moving-to-trash t)

;;
;;; Drag & Drop
(setq mouse-drag-and-drop-region t)

;;
;;; Ediff
(setq ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

;;
;;; ElDoc
(eldoc-add-command
 'mouse-set-point
 'vsc-edit-real-space 'vsc-edit-smart-space 'vsc-edit-space
 'vsc-edit-real-backspace 'vsc-edit-smart-backspace 'vsc-edit-backspace
 'previous-line 'next-line
 'vs-edit-previous-line 'vs-edit-next-line
 'jcs-py-indent-up 'jcs-py-indent-down
 'left-char 'right-char
 'vs-edit-forward-word 'vs-edit-backward-word
 'jcs-backward-word-capital 'jcs-forward-word-capital
 'beginning-of-line 'end-of-line
 'vsc-edit-beginning-of-line 'vsc-edit-end-of-line)

;;; Electric
(setq-default electric-pair-inhibit-predicate 'electric-pair-default-inhibit)

(use-package electric-cursor
  :init
  (setq electric-cursor-alist '((overwrite-mode . hbar)
                                (t              . box))))
(use-package electric-indent-sexp
  :hook (electric-indent-mode . electric-indent-sexp-mode)
  :init
  (setq electric-indent-sexp-auto-chars t))

(defun jcs-elec-pair-add (lst-pr)
  "Append a list of pair (LST-PR) to current buffer."
  (require 'elec-pair)
  (setq-local electric-pair-pairs (append electric-pair-pairs lst-pr)
              electric-pair-text-pairs electric-pair-pairs))

;;
;;; Files
(setq create-lockfiles nil
      make-backup-files nil)

;;
;;; Keybinds
(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-dont-use-unicode t))

;;
;;; Mark
(use-package eval-mark
  :init
  (setq eval-mark-commands-after '( eval-buffer eval-defun eval-region
                                    narrow-to-region)))

;;
;;; Messages
(use-package message-clean-mode
  :init
  (setq message-clean-mode-mute-commands '( push-mark set-mark-command
                                            y-or-n-p)
        message-clean-mode-echo-commands
        '( mwheel-scroll
           previous-line next-line
           vsc-edit-beginning-of-line vsc-edit-end-of-line
           mark-whole-buffer
           indent-region
           package-menu--mark-upgrades-1 pkg-dm--show-upgrades pkg-dm-autoremove
           browse-kill-ring-setup
           iedit-mode
           isearch-done
           undefined
           toggle-truncate-lines
           reb-update-overlays reb-next-match reb-prev-match
           lsp--message
           define-it)
        message-clean-mode-minor-mode 'echo))

(use-package msgu
  :init
  (setq msgu-sleep-seconds 0.4
        msgu-sit-seconds 100))

;;
;;; Minibuffer
(setq enable-recursive-minibuffers t
      completion-styles '(partial-completion)  ; easy on `company-mode'
      completion-category-defaults nil
      completion-ignored-extensions nil
      completion-ignore-case t
      suggest-key-bindings nil)

;;
;;; Process
(setq kill-buffer-query-functions nil)

;;
;;; Read-Only
(use-package auto-read-only
  :init
  (setq auto-read-only-function #'read-only-mode)
  :config
  (nconc auto-read-only-file-regexps
         '("emacs/.*/lisp/"
           "/[.]emacs[.]d/elpa/"))
  (jcs-advice-add 'auto-read-only--hook-find-file :override
    (unless (jcs-project-root) (auto-read-only))))

;;
;;; Recent Files
(setq recentf-max-menu-items 25)

(use-package recentf-excl
  :init
  (setq recentf-excl-commands '( jcs-goto-definition
                                 jcs-goto-definition-other-window
                                 jcs-peek-definition
                                 ediff-find-file)))

;;
;;; Revert
(use-package vs-revbuf
  :init
  (setq vs-revbuf-ask-unsaved-changes-only t))

;;
;;; Right Click
(use-package right-click-context
  :config
  (jcs-advice-add 'right-click-context-menu :override
    ;; Open Right Click Context menu.
    (let ((popup-menu-keymap (copy-sequence popup-menu-keymap)))
      (define-key popup-menu-keymap [mouse-3] #'right-click-context--click-menu-popup)
      (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree) nil))))
        (when (and (jcs-popup-clicked-on-menu-p) value)
          (if (symbolp value) (call-interactively value t) (eval value)))))))

;;
;;; Shift Select
(setq shift-select-mode t)

;;
;;; So Long
(use-package so-long
  :config
  (nconc so-long-minor-modes
         '(spell-fu-mode
           eldoc-mode
           highlight-numbers-mode
           highlight-indent-guides-mode
           hl-fill-column-mode
           line-reminder-mode
           page-break-lines-mode
           ts-fold-mode ts-fold-indicators-mode)))

;;
;;; Startup
(setq-default truncate-lines t)
(setq next-line-add-newlines nil
      truncate-partial-width-windows nil
      inhibit-startup-screen t)

;;
;;; Tab / Space
(setq-default indent-tabs-mode nil  ; Disable inset tabs, insert space only
              tab-width 4)

;;
;;; Tabulated List
(use-package tabulated-list
  :bind ( :map tabulated-list-mode-map
          ("C-+" . tabulated-list-widen-current-column)
          ("C-_" . tabulated-list-narrow-current-column))
  :config
  (jcs-advice-add 'tabulated-list-col-sort :around (save-excursion (apply arg0 args))))

;;
;;; Theme
(defconst jcs-theme-default 'vs-dark
  "Default theme name for this config.")

;;
;;; Trash
(setq delete-by-moving-to-trash t)

;;
;;; Warnings
(setq warning-minimum-level :emergency)

;;
;;; Web Wowser
(use-package eww
  :bind ( :map eww-mode-map
          ("M-<left>"  . eww-back-url)
          ("M-<right>" . eww-forward-url)
          ("<f5>"      . eww-reload)
          ("C-<f5>"    . eww-reload)
          ("<f12>"     . eww-view-source)
          ("C-S-a"     . eww-list-buffers)
          ("C-S-o"     . eww-list-bookmarks)
          ("C-h"       . eww-list-histories))
  :init
  (setq eww-search-prefix "https://www.google.com/search?q="))

;;; Word Wrap
(setq word-wrap-by-category t)

(provide 'jcs-env)
;;; jcs-env.el ends here
