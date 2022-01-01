;;; jcs-env.el --- Environment Settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Determine the underlying operating system
(defconst jcs-is-windows (memq system-type '(cygwin windows-nt ms-dos))
  "Non-nil when Microsoft Windows.")
(defconst jcs-is-mac (eq system-type 'darwin)
  "Non-nil when macOS.")
(defconst jcs-is-linux (eq system-type 'gnu/linux)
  "Non-nil when Linux.")
(defconst jcs-is-bsd (or jcs-is-mac (eq system-type 'berkeley-unix))
  "Non-nil when BSD.")

(defconst jcs-system-type
  (cond (jcs-is-windows 'dos)
        (jcs-is-bsd     'mac)
        (jcs-is-linux   'unix)
        (t              'unknown))
  "Return current OS type.")

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
  (cua-mode 0)
  ;;(osx-key-mode 0)
  (setq mac-command-modifier 'meta
        select-enable-clipboard t
        aquamacs-save-options-on-quit 0
        special-display-regexps nil
        special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t
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

;;; Bury Bufferss
(defconst jcs-bury-buffer-list '()
  "List of buffer that you don't want to show when after exit.")

;;; Change Log
(defconst jcs-changelog-template-dir "~/.emacs.jcs/template/__changelog/"
  "Path point to all changelog template files.")

;;; Compilation (Output)
(leaf compile
  :init
  (setq compilation-context-lines t
        compilation-error-regexp-alist
        (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
              compilation-error-regexp-alist)
        compilation-scroll-output t)
  :defer-config
  (require 'ansi-color)
  (defun jcs--colorize-compilation-buffer ()
    "Support for ANSI-escape coloring."
    (let (buffer-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'jcs--colorize-compilation-buffer))

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
  :defer-config
  (set-variable 'grep-command "grep -irHn ")
  (when jcs-is-windows
    (setq grep-use-null-device t)
    (set-variable 'grep-command "findstr -s -n -i -l ")))

;;; Creator
(defun jcs-creator-name () "Name of the creator." user-full-name)
(defun jcs-copyright-info () "Copyright information." "Shen, Jen-Chieh")

;;; Default Major Mode
(setq-default major-mode 'text-mode)

;;; Deletion
(setq delete-by-moving-to-trash t)

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

;;; ElDoc
(global-eldoc-mode 1)

;;; Electric Indent
(electric-indent-mode 1)

;;; Electric Pair
(defun jcs--electric-pair-inhibit-predicate (c)
  "Electric pair inhibit predicate with pair character C."
  (cond ((or (jcs-current-char-equal-p '("\"" "'"))
             (not (jcs-inside-comment-or-string-p)))
         (electric-pair-default-inhibit c))
        (t t)))
(setq-default electric-pair-inhibit-predicate 'jcs--electric-pair-inhibit-predicate)

(defconst jcs-smart-closing-parens '("}" ")" "]")
  "List of closing parenthesis.")

;;; Web Wowser
(leaf eww
  :init
  (setq eww-search-prefix "https://www.google.com/search?q="))

;;; Find File
(defvar jcs-current-created-parent-dir-path nil
  "Globally record the virutally created parent dir path.")

(defvar-local jcs-created-parent-dir-path nil
  "Record down the created parent directory path.")

(defun jcs--find-starting-not-exists-dir-path (path &optional d-f)
  "Return the not exists directory path by PATH; D-F is optional default directory."
  (require 'f) (require 's)
  (unless d-f (setq d-f default-directory))
  (let* ((virtual-path (s-replace d-f "" path))
         (split-paths (f-split virtual-path)) (split-path-item "")
         (prev-path (f-slash d-f)) (test-path prev-path)
         (index 0) break-it result-path)
    (while (and (< index (length split-paths)) (not break-it))
      (setq split-path-item (nth index split-paths)
            test-path (f-slash (f-join test-path split-path-item)))
      (unless (file-directory-p test-path)
        (setq result-path prev-path break-it t))
      (setq prev-path test-path
            index (1+ index)))
    (unless result-path (setq result-path prev-path))
    (f-slash result-path)))

(defun jcs-create-non-existent-directory ()
  "Create the parent directory if not exists."
  (let* ((current-d-f default-directory)
         (parent-directory (file-name-directory buffer-file-name))
         (non-virtual-path (jcs--find-starting-not-exists-dir-path parent-directory))
         (created-path (s-replace non-virtual-path "" parent-directory)))
    (when (and (not (jcs-directory-p parent-directory))
               (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t)
      (setq jcs-current-created-parent-dir-path created-path))))

(push #'jcs-create-non-existent-directory find-file-not-found-functions)

;;; Font Size
(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

;;; Key List
(defconst jcs-key-list
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "`"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" "\\"
    "~" "{" "}" "[" "]" ";" ":" "'" "\"" "," "." "<" ">"
    "/" "?" "|" " ")
  "List of key to bind.")

;; Resolve performance issue moving around Unicode Text.
(setq inhibit-compacting-font-caches t)

;;; Line Numbers
(defconst jcs-line-numbers-ignore-buffers
  '("[*]+[[:ascii:]]+"
    "magit[-]*[[:ascii:]]*[:]"
    "tree-sitter-tree:")
  "List of buffers that you do not want to show line numbers in it.")

(defconst jcs-line-numbers-ignore-buffer-exceptions '("*scratch*")
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
(defconst jcs-message-log-max (* 1000 10)
  "Default maximum lines for message log.")

(setq message-log-max jcs-message-log-max)

(defconst jcs-sleep-for-seconds 0.4
  "Default sleep for seconds.")

(defconst jcs-sit-for-seconds 100
  "Default sit for seconds.")

;;; Multiple Cursors
(defvar jcs-mc/string-distance-level 20
  "The standard similarity, the lower require more precision.")

;;; Mute
(defvar jcs-mute-commands
  (append '(previous-line next-line)
          '(beginning-of-buffer end-of-buffer)
          '(set-mark-command)
          '(jcs-beginning-of-line jcs-end-of-line)
          '(jcs-mark-whole-buffer))
  "List of commands to mute it's action warnings message.")

(defun jcs--mute-command--advice-around (fnc &rest args)
  "Mute any commands."
  (jcs-mute-apply (apply fnc args)))

(dolist (command jcs-mute-commands)
  (advice-add command :around #'jcs--mute-command--advice-around))

(defun jcs--command-error-function (data context caller)
  "Ignore signals for certain commands; pass the rest to the default handler."
  (unless (memq (car data) jcs-mute-commands)
    (command-error-default-function data context caller)))

(setq command-error-function #'jcs--command-error-function)

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

(defvar jcs-recentf-tracking-p t
  "If non-nil, track the opened file.")

(defun jcs-recentf-track-opened-file-p ()
  "Return non-nil if we should track opened file."
  (and jcs-recentf-tracking-p (not jcs-package-installing-p)))

(defun jcs--recentf-track-opened-file--advice-after ()
  "Advice execute after function `recentf-track-opened-file'."
  (when (jcs-recentf-track-opened-file-p) (jcs-dashboard-safe-refresh-buffer)))
(advice-add 'recentf-track-opened-file :after #'jcs--recentf-track-opened-file--advice-after)

(defun jcs--recentf-track-opened-file--advice-around (fnc &rest args)
  "Advice execute around function `recentf-track-opened-file'."
  (when (jcs-recentf-track-opened-file-p) (apply fnc args)))
(advice-add 'recentf-track-opened-file :around #'jcs--recentf-track-opened-file--advice-around)

;;; Save Files
(defvar jcs-on-save-end-trailing-lines-cleanup-p t
  "Remove trailing lines at the end of buffer on save.")

(defvar jcs-on-save-whitespace-cleanup-p t
  "Clean up whitespaces on save.")

(defvar jcs-on-save-remove-control-M-p t
  "Remove ^M character on save.")

(defvar jcs-on-save-tabify-type 'untabify  ; This takes `nil', `tabify', `untabify'.
  "Default untabify or tabify to the buffer.")

;;; Separator
(defconst jcs-env-separator-char "─"
  "Separator character.")

(defun jcs-env-separator ()
  "Return environment separator."
  (propertize
   (if (display-graphic-p) "\f"
     (jcs-fill-n-char-seq jcs-env-separator-char (- (window-total-width) 2)))
   'face font-lock-comment-face))

;;; Shift Select
(setq shift-select-mode t)

;;; Scroll
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(5 ((shift) . 2))
        mouse-wheel-progressive-speed nil))

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

;;; Shell
(jcs-with-eval-after-load-multiple '(shell eshell) (require 'jcs-shell))

;;; So Long
(leaf so-long
  :defer-config
  (defconst jcs-so-long-minor-modes
    '(line-reminder-mode)
    "List of disabled minor modes for `so-long' buffer.")
  (nconc so-long-minor-modes jcs-so-long-minor-modes))

;;; Startup
(push '(fullscreen . maximized) default-frame-alist)
(setq-default truncate-lines t)
(setq next-line-add-newlines nil
      truncate-partial-width-windows nil)
(unless (boundp 'jcs-build-test) (ignore-errors (split-window-horizontally)))

;;; Tab / Space
(setq-default indent-tabs-mode nil  ; Disable inset tabs, insert space only
              tab-width 4)

;;; Tabulated List
(leaf tabulated-list
  :defer-config
  (defun jcs--tabulated-list-col-sort--advice-around (fnc &rest args)
    "Advice execute around `tabulated-list-col-sort' function."
    (save-excursion (apply fnc args)))
  (advice-add 'tabulated-list-col-sort :around #'jcs--tabulated-list-col-sort--advice-around))

;;; Theme
(defconst jcs-theme-default 'vs-dark
  "Default theme name for this config.")

;;; Undo
(setq undo-limit 20000000
      undo-strong-limit 40000000)

;;; Uniquify
(leaf uniquify
  :init
  ;; NOTE: meaningful names for buffers with the same name from prelude.
  ;;
  ;; SOURCE: http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
  ;; URL: https://github.com/bbatsov/prelude
  (setq uniquify-buffer-name-style 'forward
        uniquify-after-kill-buffer-p t  ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"  ; don't muck with special buffers
        uniquify-separator "/"))

;;; Variables
(setq enable-local-variables :safe)

;;; Warnings
(setq warning-minimum-level :emergency)

;;; Whitespace
(leaf whitespace
  :defer-config
  (autoload 'whitespace-mode "whitespace-mode" "Toggle whitespace visualization." t)
  (autoload 'whitespace-toggle-options "whitespace-mode" "Toggle local `whitespace-mode' options." t)
  ;; All the face can be find here.
  ;; URL: https://www.emacswiki.org/emacs/BlankMode
  (set-face-attribute 'whitespace-indentation nil
                      :background "grey20" :foreground "aquamarine3")
  (set-face-attribute 'whitespace-trailing nil
                      :background "grey20" :foreground "red"))

;;; Windows
(defconst jcs-windows--enlarge-shrink-times 6
  "Times to shrink inside the window.")

(provide 'jcs-env)
;;; jcs-env.el ends here
