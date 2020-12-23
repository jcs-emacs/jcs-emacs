;;; jcs-env.el --- Environment Settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000
      undo-strong-limit 40000000)

;; Determine the underlying operating system
(defconst jcs-is-windows (memq system-type '(cygwin windows-nt ms-dos))
  "This value is non-nil; if current operating system on Microsoft Windows.")
(defconst jcs-is-mac (eq system-type 'darwin)
  "This value is non-nil; if current operating system on macOS.")
(defconst jcs-is-linux (eq system-type 'gnu/linux)
  "This value is non-nil; if current operating system on Linux.")
(defconst jcs-is-bsd (or jcs-is-mac (eq system-type 'berkeley-unix))
  "This value is non-nil; if current operating system on BSD.")

(defconst jcs-daily-todo-file "~/TODO_JenChieh/code/todo.txt" "Open the daily todo file.")
(defconst jcs-log-file "~/TODO_JenChieh/code/log.txt" "Log file path, file location.")

(defconst jcs-project-todo-file "TODO[.]*[[:ascii:]]*" "Project TODO file.")
(defconst jcs-project-update-log-file "CHANGELOG[.]*[[:ascii:]]*" "Project Update Log file.")

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
 (jcs-is-mac
  (cua-mode 0)
  ;;(osx-key-mode 0)
  (setq mac-command-modifier 'meta
        select-enable-clipboard t
        aquamacs-save-options-on-quit 0
        special-display-regexps nil
        special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t
        mac-pass-command-to-system nil))
 (jcs-is-linux
  ;; None..
  )
 (jcs-is-bsd
  ;; None..
  ))

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
(defconst jcs-bury-buffer-list '("[*]ffmpeg-player[*]: ")
  "List of buffer that you don't want to show when after exit.")

;;; Change Log
(defconst jcs-changelog-template-path "~/.emacs.jcs/template/__changelog/"
  "Path point to all changelog template files.")

;;; Compilation (Output)
(with-eval-after-load 'compile
  (setq compilation-context-lines t)
  (setq compilation-error-regexp-alist
        (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
              compilation-error-regexp-alist)))

(defconst jcs-compilation-base-filename "output"
  "Base filename for compilation buffer.")

;;; Commands
(with-eval-after-load 'grep
  (set-variable 'grep-command "grep -irHn ")
  (when jcs-is-windows
    (setq grep-use-null-device t)
    (set-variable 'grep-command "findstr -s -n -i -l ")))

;;; Default Major Mode
(setq-default major-mode 'text-mode)

;;; Deletion
(setq delete-by-moving-to-trash t)

;;; Doc View
(when jcs-is-windows
  (setq doc-view-ghostscript-program (executable-find "gswin64c")))

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
(electric-pair-mode 1)

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
         (index 0) (break-it nil)
         (result-path nil))
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
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t)
      (setq jcs-current-created-parent-dir-path created-path))))

(add-to-list 'find-file-not-found-functions #'jcs-create-non-existent-directory)

;;; Font Size
(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

;;; Frame
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;;; Highlight Select Region
(transient-mark-mode t)

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

;;; Language Environment
(prefer-coding-system 'utf-8)
(defconst jcs-language-environment "UTF-8" "Default language environment.")
(set-default-coding-systems 'utf-8)
(unless (jcs-reload-emacs-reloading-p) (set-terminal-coding-system 'utf-8))
(set-keyboard-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)

;; Resolve performance issue moving around Unicode Text.
(setq inhibit-compacting-font-caches t)

;;; License
(defconst jcs-license-template-path "~/.emacs.jcs/template/__license/"
  "Path point to all license template files.")

;;; Line Numbers
(defconst jcs-line-numbers-ignore-buffers '("[*]+[[:ascii:]]+"
                                            "magit: ")
  "List of buffers that you do not want to show line numbers in it.")

(defconst jcs-line-numbers-ignore-buffer-exceptions '("*scratch*")
  "These buffers wouldn't be ignored line numbers mode.")

(defconst jcs-line-numbers-ignore-modes '("Custom-mode"
                                          "dired-mode"
                                          "doc-view-mode"
                                          "eww-mode"
                                          "help-mode"
                                          "image-mode"
                                          "message-mode"
                                          "outline-mode"
                                          "package-menu-mode")
  "List of modes that you do not want to show line numbers in it.")

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

;;; Package
(use-package use-package
  :defer t
  :init
  (setq use-package-always-defer t
        use-package-expand-minimally t))

;;; Previous/Next keys
(defcustom jcs-prev/next-key-type 'smart
  "Key definition for previous and next line.

The variable can be set one of the following value.

  - normal : The default behaviour from Emacs.
  - indent : Indent every movement.
  - smart : Move to the code line if available.

P.S. You would need to restart Emacs to take effect from this variable."
  :type '(choice (const :tag "normal" normal)
                 (const :tag "indent" indent)
                 (const :tag "smart" smart))
  :group 'jcs)

;;; Process
(setq kill-buffer-query-functions nil)

;;; Recent Files
(setq recentf-max-menu-items 25)

(defun jcs--recentf-track-opened-file--advice-after ()
  "Advice execute after function `recentf-track-opened-file'."
  (unless jcs-package-installing-p (jcs-dashboard-safe-refresh-buffer)))
(advice-add 'recentf-track-opened-file :after #'jcs--recentf-track-opened-file--advice-after)

(defun jcs--recentf-track-opened-file--advice-around (fnc &rest args)
  "Advice execute around function `recentf-track-opened-file'."
  (unless jcs-package-installing-p (apply fnc args)))
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
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed t))

(setq scroll-step 1
      scroll-margin 0)

(defun jcs-toggle-scroll-conservatively (act)
  "Enable variable `scroll-conservatively' base on ACT.

If ACT is non-nil; then make scroll less jumpy."
  (setq scroll-conservatively (if act 100000 0)))

(defun jcs-scroll-conservatively-enable ()
  "Make scroll less jumpy."
  (jcs-toggle-scroll-conservatively t))

(defun jcs-scroll-conservatively-disable ()
  "Revert scroll to default value."
  (jcs-toggle-scroll-conservatively nil))

(jcs-scroll-conservatively-enable)

;;; So Long
;; NOTE: (for very long file, like `jquery.min.js', etc)
(when (version<= "27.1" emacs-version) (global-so-long-mode 1))

;;; Startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default truncate-lines t)
(setq next-line-add-newlines nil
      truncate-partial-width-windows nil)
(unless (boundp 'jcs-build-test) (ignore-errors (split-window-horizontally)))

;;; Tab / Space
(setq-default indent-tabs-mode nil  ; Disable inset tabs, insert space only
              tab-width 4)

;;; Tabulated List
(use-package tabulated-list
  :config
  (defun jcs--tabulated-list-col-sort--advice-around (fnc &rest args)
    "Advice execute around `tabulated-list-col-sort' function."
    (save-excursion (apply fnc args)))
  (advice-add 'tabulated-list-col-sort :around #'jcs--tabulated-list-col-sort--advice-around))

;;; Uniquify
(use-package uniquify
  :init
  ;; NOTE: meaningful names for buffers with the same name from prelude.
  ;;
  ;; SOURCE: http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
  ;; URL: https://github.com/bbatsov/prelude
  (setq uniquify-buffer-name-style 'forward
        uniquify-after-kill-buffer-p t  ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"  ; don't muck with special buffers
        uniquify-separator "/"))

;;; Windows
(defconst jcs-windows--enlarge-shrink-times 6
  "Times to shrink inside the window.")

(provide 'jcs-env)
;;; jcs-env.el ends here
