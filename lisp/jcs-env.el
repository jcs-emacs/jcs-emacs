;;; jcs-env.el --- Environment Settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

;;; Audo Saving
(setq auto-save-default nil
      auto-save-interval 0
      auto-save-list-file-prefix nil
      auto-save-timeout 0)

;;; Backup Files
(setq make-backup-files nil)

;;; Bell
(defun nil-bell ())  ; Turn off the bell on macOS
(setq visible-bell nil
      ring-bell-function 'nil-bell)

;;; Change Log
(defconst jcs-changelog-template-dir (concat user-emacs-directory "templates/__changelog/")
  "Path point to all changelog template files.")

;;; Columns
(setq-default fill-column 80)

;;; Compilation
(leaf compile
  :init
  (setq compilation-context-lines t
        compilation-scroll-output t)
  :defer-config
  (require 'ansi-color)
  (jcs-add-hook 'compilation-filter-hook
    (let (buffer-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(leaf comint
  :init
  (setq comint-prompt-read-only t
        comint-process-echoes t
        comint-scroll-to-bottom-on-input t
        comint-move-point-for-output t))

;;; Commands
(leaf grep
  :init
  (setq grep-command (if elenv-windows "findstr -s -n -i -l " "grep -irHn ")
        grep-use-null-device (when elenv-windows t)))

;;; Comments
(leaf newcomment
  :init
  (setq comment-inline-offset 2))

;;; Custom
(setq custom-safe-themes t)

;;; Default Major Mode
(setq-default major-mode 'text-mode)

;;; Deletion
(setq delete-by-moving-to-trash t)

;;; Dialog
(setq use-file-dialog nil
      use-dialog-box nil)

;;; Dired
(setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
      dired-hide-details-hide-symlink-targets nil
      ;; don't prompt to revert, just do it
      dired-auto-revert-buffer #'dired-buffer-stale-p
      ;; Always copy/delete recursively
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      ;; Ask whether destination dirs should get created when copying/removing files.
      dired-create-destination-dirs 'ask
      ;; Screens are larger nowadays, we can afford slightly larger thumbnails
      image-dired-thumb-size 150)

;;; Display Column
(leaf display-fill-column-indicator
  :init
  (setq-default display-fill-column-indicator-column 80))

;;; Doc View
(leaf doc-view
  :defer-config
  (when elenv-windows
    (setq doc-view-ghostscript-program (executable-find "gswin64c"))))

;;; Drag & Drop
(setq mouse-drag-and-drop-region t)

;;; Ediff
(setq ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

;;; Electric Pair
(setq-default electric-pair-inhibit-predicate 'electric-pair-default-inhibit)

;;; Font Size
(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

;; Resolve performance issue moving around Unicode Text
(setq inhibit-compacting-font-caches t)

;;; Image
(setq image-animate-loop t)

;;; Line Numbers
(setq-default
 ;; Explicitly define a width to reduce the cost of on-the-fly computation
 display-line-numbers-width 3
 ;; Show absolute line numbers for narrowed regions to make it easier to tell the
 ;; buffer is narrowed, and where you are, exactly.
 display-line-numbers-widen t)

(column-number-mode 1)

;;; Minibuffer
(setq enable-recursive-minibuffers t
      completion-styles '(partial-completion)  ; easy on `company-mode'
      completion-category-defaults nil
      completion-ignored-extensions nil
      completion-ignore-case t
      suggest-key-bindings nil)

;;; Parenthesis
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Process
(setq kill-buffer-query-functions nil)

;;; Recent Files
(setq recentf-max-menu-items 25)

;;; Shift Select
(setq shift-select-mode t)

;;; Scroll
(setq mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)

(setq scroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq hscroll-margin 2
      hscroll-step 1)

;;; So Long
(leaf so-long
  :defer-config
  (nconc so-long-minor-modes
         '(highlight-indent-guides-mode
           line-reminder-mode
           page-break-lines-mode
           ts-fold-mode ts-fold-indicators-mode)))

;;; Startup
(setq-default truncate-lines t)
(setq next-line-add-newlines nil
      truncate-partial-width-windows nil
      inhibit-startup-screen t)

(push '(fullscreen . maximized) default-frame-alist)  ; full screen
(unless noninteractive (ignore-errors (split-window-horizontally)))

;;; Tab / Space
(setq-default indent-tabs-mode nil  ; Disable inset tabs, insert space only
              tab-width 4)

;;; Tabulated List
(leaf tabulated-list
  :defer-config
  (jcs-advice-add 'tabulated-list-col-sort :around (save-excursion (apply arg0 args))))

;;; Theme
(defconst jcs-theme-default 'vs-dark
  "Default theme name for this config.")

;;; Trash
(setq delete-by-moving-to-trash t)

;;; Undo
(setq undo-limit 20000000
      undo-strong-limit 40000000)

;;; Uniquify
(leaf uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-after-kill-buffer-p t  ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"  ; don't muck with special buffers
        uniquify-separator "/"))

;;; Variables
(setq enable-local-variables :safe)

;;; Version Control
(setq vc-git-diff-switches '("--histogram"))  ;  A slightly faster algorithm for diffing

;;; Warnings
(setq warning-minimum-level :emergency)

;;; Web Wowser
(leaf eww
  :init
  (setq eww-search-prefix "https://www.google.com/search?q="))

;;; Whitespace
(leaf whitespace
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.]))))

;;; Windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(leaf windmove
  :init
  (setq windmove-wrap-around t))

;;; Word Wrap
(setq word-wrap-by-category t)

(provide 'jcs-env)
;;; jcs-env.el ends here
