;;; jcs-env.el --- Environment Settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Determine the underlying operating system
(defconst jcs-is-windows (memq system-type '(cygwin windows-nt ms-dos))  "Windows")
(defconst jcs-is-mac     (eq system-type 'darwin)                        "macOS")
(defconst jcs-is-linux   (eq system-type 'gnu/linux)                     "Linux")
(defconst jcs-is-bsd     (or jcs-is-mac (eq system-type 'berkeley-unix)) "BSD")

(defconst jcs-system-type
  (cond (jcs-is-windows 'dos)
        (jcs-is-bsd     'mac)
        (jcs-is-linux   'unix)
        (t              'unknown))
  "Store current system type.")

(defvar jcs-makescript "[[:ascii:]]*build[[:ascii:]]*"
  "Name of the build/make file script.")
(defvar jcs-runscript "[[:ascii:]]*run[[:ascii:]]*"
  "Name of the execute/run file script.")

(defvar jcs-use-sh-p (or jcs-is-mac jcs-is-linux jcs-is-bsd)
  "Flag if the system use shell script.")

(cond
 (jcs-is-windows
  (setq jcs-makescript (concat jcs-makescript "[.]bat")
        jcs-runscript (concat jcs-runscript "[.]bat")))
 (jcs-is-bsd
  (setq mac-command-modifier 'meta
        select-enable-clipboard t
        aquamacs-save-options-on-quit 0
        special-display-regexps nil
        special-display-buffer-names nil
        mac-command-key-is-meta t
        mac-pass-command-to-system nil)))

(when jcs-use-sh-p
  (setq jcs-makescript (concat jcs-makescript "[.]sh")
        jcs-runscript (concat jcs-runscript "[.]sh")))


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

(defconst jcs-compilation-base-filename "output"
  "Base filename for compilation buffer.")

;;; Commands
(leaf grep
  :init
  (setq grep-command (if jcs-is-windows "findstr -s -n -i -l " "grep -irHn ")
        grep-use-null-device (when jcs-is-windows t)))

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

;;; Display Column
(leaf display-fill-column-indicator
  :init
  (setq-default display-fill-column-indicator-column 80))

;;; Doc View
(leaf doc-view
  :defer-config
  (when jcs-is-windows
    (setq doc-view-ghostscript-program (executable-find "gswin64c"))))

;;; Drag & Drop
(setq mouse-drag-and-drop-region t)

;;; Ediff
(defun jcs-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  "Set up windows for `ediff'."
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))

(setq ediff-window-setup-function 'jcs-ediff-setup-windows
      ediff-split-window-function 'split-window-horizontally)

;;; Electric Pair
(setq-default electric-pair-inhibit-predicate 'electric-pair-default-inhibit)

;;; Font Size
(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

;; Resolve performance issue moving around Unicode Text
(setq inhibit-compacting-font-caches t)

;;; Line Numbers
(column-number-mode 1)

(defconst jcs-line-numbers-ignore-buffers
  '("[*]+[[:ascii:]]+"
    "magit[-]*[[:ascii:]]*[:]"
    "tree-sitter-tree:")
  "List of buffers that you do not want to show line numbers in it.")

(defconst jcs-line-numbers-ignore-buffer-exceptions
  `(,(buffer-name (get-scratch-buffer-create)))
  "These buffers wouldn't be ignored line numbers mode.")

(defconst jcs-line-numbers-ignore-modes
  '(Custom-mode
    dired-mode
    doc-view-mode
    eww-mode
    help-mode
    image-mode
    message-mode
    outline-mode
    package-menu-mode
    treemacs-mode)
  "List of modes that you do not want to show line numbers.")

;;; Messages
(defconst jcs-message-log-max message-log-max
  "Default maximum lines for message log.")

;;; Minibuffer
(setq enable-recursive-minibuffers t
      completion-styles '(partial-completion)  ; easy on `company-mode'
      completion-category-defaults nil
      completion-ignored-extensions nil
      completion-ignore-case t
      suggest-key-bindings nil)

;;; Previous/Next keys
(defcustom jcs-prev/next-key-type 'smart
  "Key definition for previous and next line.

The variable can be set one of the following value.

  - normal : The default behaviour from Emacs.
  - smart : Move to the code line if available.

P.S. You would need to restart Emacs to take effect from this variable."
  :type '(choice (const :tag "normal" normal)
                 (const :tag "smart" smart))
  :group 'jcs)

;;; Process
(setq kill-buffer-query-functions nil)

;;; Recent Files
(setq recentf-max-menu-items 25)

(defun jcs-recentf-track-opened-file-p ()
  "Return non-nil if we should track opened file."
  (and recentf-excl-tracking-p (not jcs-package-installing-p)))

(jcs-advice-add 'recentf-track-opened-file :after
  (when (jcs-recentf-track-opened-file-p) (jcs-dashboard-refresh-buffer)))

(jcs-advice-add 'recentf-track-opened-file :around
  (when (jcs-recentf-track-opened-file-p) (apply arg0 args)))

;;; Save Files
(defcustom jcs-on-save-remove-control-M t
  "Remove ^M character on save."
  :type 'boolean
  :group 'jcs)

;;; Shift Select
(setq shift-select-mode t)

;;; Scroll
(setq mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)

(setq scroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq hscroll-margin 2
      hscroll-step 1)

(defun jcs-toggle-scroll-conservatively (act)
  "Enable variable `scroll-conservatively' base on ACT.

If ACT is non-nil; then make scroll less jumpy."
  (setq scroll-conservatively (if act 101 0)))

(defun jcs-scroll-conservatively-enable ()
  "Make scroll less jumpy."
  (jcs-toggle-scroll-conservatively t))

(defun jcs-scroll-conservatively-disable ()
  "Revert scroll to default value."
  (jcs-toggle-scroll-conservatively nil))

(jcs-scroll-conservatively-enable)

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
