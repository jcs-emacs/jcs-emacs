;;; jcs-env.el --- Environment Settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)


;; Determine the underlying operating system
(defconst jcs-is-windows (memq system-type '(cygwin windows-nt ms-dos))
  "Is Microsoft Windows.")
(defconst jcs-is-mac (eq system-type 'darwin)
  "Is Mac OS X.")
(defconst jcs-is-linux (eq system-type 'gnu/linux)
  "Is Linux.")
(defconst jcs-is-bsd (or jcs-is-mac (eq system-type 'berkeley-unix))
  "Is BSD.")


(defconst jcs-daily-todo-file "~/TODO_JenChieh/code/todo.txt" "Open the daily todo file.")
(defconst jcs-log-file "~/TODO_JenChieh/code/log.txt" "Log file path, file location.")

(defconst jcs-project-todo-file "TODO[.]*[[:ascii:]]*" "Project TODO file.")
(defconst jcs-project-update-log-file "CHANGELOG[.]*[[:ascii:]]*" "Project Update Log file.")

(defvar jcs-makescript "[[:ascii:]]*build[[:ascii:]]*"
  "Build/Make script's file name.")
(defvar jcs-runscript "[[:ascii:]]*run[[:ascii:]]*"
  "Execute/Run script's file name.")

(defvar jcs-use-sh-p (or jcs-is-mac jcs-is-linux jcs-is-bsd)
  "Flag if the system use shell script.")

(cond
 (jcs-is-windows
  (setq jcs-makescript (concat jcs-makescript "[.]bat"))
  (setq jcs-runscript (concat jcs-runscript "[.]bat")))
 (jcs-is-mac
  (cua-mode 0)
  ;;(osx-key-mode 0)
  (setq mac-command-modifier 'meta)
  (setq select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (setq mac-pass-command-to-system nil))
 (jcs-is-linux
  ;; None..
  )
 (jcs-is-bsd
  ;; None..
  ))

(when jcs-use-sh-p
  (setq jcs-makescript (concat jcs-makescript "[.]sh"))
  (setq jcs-runscript (concat jcs-runscript "[.]sh")))


;;; Backup Files
(setq make-backup-files nil)

;;; Bell
(defun nil-bell ())  ; Turn off the bell on Mac OS X
(setq ring-bell-function 'nil-bell)

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

;;; Doc View
(when jcs-is-windows
  (setq doc-view-ghostscript-program (executable-find "gswin64c")))

;;; Ediff
(defun jcs-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))
(setq ediff-window-setup-function 'jcs-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

;;; ElDoc
;; It shows you the argument list of the function call you are currently
;; writing in the echo area.
(global-eldoc-mode 1)

;;; Electric Indent
(electric-indent-mode 1)

;;; Electric Pair
(setq-default
 electric-pair-inhibit-predicate
 (lambda (c)
   (if (jcs-current-char-equal-p '("\"" "'"))
       (electric-pair-default-inhibit c)
     (if (not (jcs-inside-comment-or-string-p))
         (electric-pair-default-inhibit c)
       t))))
(electric-pair-mode 1)

;;; Font Size
(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

;; Frame
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
(defconst jcs-prompt-message-sleep-delay-time 0.4
  "Delay for a time for prompting out the message in seconds.
Hence the user can see the error/operation message.")

;;; Multiple Cursors
(defvar jcs-mc/string-distance-level 20
  "The standard similarity, the lower require more precision.")

;; Navigation
(defvar jcs--no-advice-other-window nil
  "Flag to disable not advising in other window.
See `jcs-hook.el' file that has apply `advice' on command `other-window'.")

;;; Previous/Next keys
(defcustom jcs-prev/next-key-type 'indent
  "Previous or Next key definition."
  :type '(choice (const :tag "normal" normal)
                 (const :tag "indent" indent)
                 (const :tag "smart" smart)))

;;; Process
(setq kill-buffer-query-functions nil)

;;; Recent Files
(setq recentf-max-menu-items 25)


(defun jcs--recentf-track-opened-file--advice-after ()
  "Advice execute after `recentf-track-opened-file' function."
  (unless jcs-package-installing
    (jcs-dashboard-refresh-buffer)))
(advice-add 'recentf-track-opened-file :after #'jcs--recentf-track-opened-file--advice-after)

;;; Read Only
(defconst jcs-find-file-read-only-paths '("/.emacs.d/elisp/"
                                          "/.emacs.d/elpa/"
                                          "/lisp/")
  "Find file with these paths, esure read only mode enabled.")

;;; Shift Select
;; NOTE: This act weird, does not make it works like other editor.
(setq shift-select-mode nil)

;;; Smooth scroll
(setq scroll-step 2)

;;; So Long  (for very long file, like `jquery.min.js', etc)
(when (version<= "27.1" emacs-version)
  (global-so-long-mode 1))

;;; Startup windowing
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(unless (boundp 'jcs-build-test)
  (split-window-horizontally))

;;; Tab / Space
(setq-default indent-tabs-mode nil)          ; Disable inset tabs, insert space only
(setq-default tab-width 4)

(defvar jcs-tab-with-records
  '((actionscript-mode     . 4)
    (c-mode                . 4)
    (c++-mode              . 4)
    (csharp-mode           . 4)
    (css-mode              . 2)
    (dockerfile-mode       . 2)
    (elisp-mode            . 2)
    (emacs-lisp-mode       . 2)
    (go-mode               . 4)
    (java-mode             . 4)
    (jayces-mode           . 4)
    (js-mode               . 2)
    (js2-mode              . 2)
    (json-mode             . 2)
    (kotlin-mode           . 4)
    (less-css-mode         . 2)
    (lisp-mode             . 2)
    (lisp-interaction-mode . 2)
    (lua-mode              . 4)
    (nasm-mode             . 4)
    (nxml-mode             . 2)
    (objc-mode             . 4)
    (python-mode           . 4)
    (rjsx-mode             . 2)
    (ruby-mode             . 4)
    (rust-mode             . 4)
    (scss-mode             . 2)
    (shader-mode           . 4)
    (ssass-mode            . 2)
    (sql-mode              . 1)
    (typescript-mode       . 4)
    (web-mode              . 2)
    (yaml-mode             . 2))
  "Tab with recrods for all major mode.")

;;; Uniquify
;; NOTE: meaningful names for buffers with the same name from prelude.
;; SOURCE: http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; Windows
(defconst jcs-windows--enlarge-shrink-times 6
  "Times to shrink inside the window.")


(provide 'jcs-env)
;;; jcs-env.el ends here
