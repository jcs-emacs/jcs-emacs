;;; jcs-env.el --- Environment Settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)


;; Determine the underlying operating system
(defvar jcs-win32 nil "Is Microsoft Windows?")
(defvar jcs-aquamacs nil "Is Mac OS X?")
(defvar jcs-linux nil "Is Linux?")

(cond
 ((string-equal system-type "windows-nt")  ; Microsoft Windows
  (setq jcs-win32 t))
 ((string-equal system-type "darwin")  ; Mac OS X
  (setq jcs-aquamacs t))
 ((string-equal system-type "gnu/linux")  ; Linux
  (setq jcs-linux t)))


(defconst jcs-daily-todo-file "" "Open the daily todo file.")
(defconst jcs-log-file "" "Log file path, file location.")

(defconst jcs-project-todo-file "TODO" "Project TODO file.")
(defconst jcs-project-update-log-file "Update_Log" "Project Update Log file.")

(defconst jcs-makescript "" "Make script file name depends on the current OS.")
(defconst jcs-runscript "" "Run script file name depends on the current OS.")

(cond (jcs-win32
       (setq jcs-daily-todo-file "C:/TODO_JenChieh/code/todo.txt")
       (setq jcs-log-file "C:/TODO_JenChieh/code/log.txt")
       (setq jcs-makescript "build.bat")
       (setq jcs-runscript "run.bat"))
      (jcs-aquamacs
       (setq jcs-daily-todo-file "/home/TODO_JenChieh/code/todo.txt")
       (setq jcs-log-file "/home/TODO_JenChieh/code/log.txt")
       (setq jcs-makescript "./build.macosx")
       (setq jcs-runscript "./run.macosx")
       (cua-mode 0)
       ;;(osx-key-mode 0)
       ;;(tabar-mode 0)
       (setq mac-command-modifier 'meta)
       (setq select-enable-clipboard t)
       (setq aquamacs-save-options-on-quit 0)
       (setq special-display-regexps nil)
       (setq special-display-buffer-names nil)
       (define-key function-key-map [return] [13])
       (setq mac-command-key-is-meta t)
       (scroll-bar-mode nil)
       (setq mac-pass-command-to-system nil))
      (jcs-linux
       (setq jcs-daily-todo-file "/home/TODO_JenChieh/code/todo.txt")
       (setq jcs-log-file "/home/TODO_JenChieh/code/log.txt")
       (setq jcs-makescript "./build.linux")
       (setq jcs-runscript "./run.linux")
       (display-battery-mode 1)))


;;; Backup Files
(setq make-backup-files nil)

;;; Bell
(defun nil-bell ())  ;; Turn off the bell on Mac OS X
(setq ring-bell-function 'nil-bell)

;;; Compilation
(with-eval-after-load 'compile
  (setq compilation-context-lines t)
  (setq compilation-error-regexp-alist
        (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
              compilation-error-regexp-alist)))

(defun jcs-compilation-mode-hook ()
  "Compilation mode hook."
  (jcs-disable-truncate-lines)

  ;; NOTE: Set smaller font.
  (setq buffer-face-mode-face '(:height 120))
  (buffer-face-mode)

  ;; Normal
  (define-key compilation-mode-map (kbd "C-c C-c") #'kill-ring-save)
  )
(add-hook 'compilation-mode-hook 'jcs-compilation-mode-hook)

;;; Commands
(with-eval-after-load 'grep
  (set-variable 'grep-command "grep -irHn ")
  (when jcs-win32
    (setq grep-use-null-device t)
    (set-variable 'grep-command "findstr -s -n -i -l ")))

;;; Default Major Mode
(setq-default major-mode 'text-mode)

;;; Doc View
(when jcs-win32
  (setq doc-view-ghostscript-program (executable-find "gswin64c")))

;;; Ediff
(defun jcs-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))
(setq ediff-window-setup-function 'jcs-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

;;; Electric Pair
(electric-pair-mode 1)

;;; Error Handling
(advice-add 'keyboard-quit :before #'jcs-reload-active-mode)
(advice-add 'top-level :before #'jcs-reload-active-mode)

;;; Font Size
(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

;; Frame
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;;; Highlight Select Region
(transient-mark-mode t)

(electric-indent-mode 1)

;;; Language Environment
(prefer-coding-system 'utf-8)
(defconst jcs-language-environment "UTF-8" "Default language environment.")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)

;; Resolve performance issue moving around Unicode Text.
(setq inhibit-compacting-font-caches t)

;;; Line Numbers
(defconst jcs-line-numbers-ignore-buffers '("*ag"
                                            "*Backtrace*"
                                            "*Buffer List*"
                                            "*Checkdoc Help*"
                                            "*Checkdoc Status*"
                                            "*Compile-Log*"
                                            "*compilation*"
                                            "*dashboard*"
                                            "*esup"
                                            "*Flycheck errors*"
                                            "*GNU Emacs*"
                                            "*helm"
                                            "*magit"
                                            "magit: "
                                            "*Messages*"
                                            "*Music*"
                                            "*Package-Lint*"
                                            "*shell*"
                                            "*SPEEDBAR*"
                                            "*undo-tree*"
                                            "*Warnings*")
  "List of buffers that you do not want to show line numbers in it.")

(defconst jcs-line-numbers-ignore-modes '("Custom-mode"
                                          "dired-mode"
                                          "doc-view-mode"
                                          "help-mode"
                                          "image-mode"
                                          "outline-mode"
                                          "package-menu-mode")
  "List of modes that you do not want to show line numbers in it.")

;;; Menu Bar
(menu-bar-mode -1)

;;; Messages
(defconst jcs-prompt-message-sleep-delay-time 0.4  ;; in seconds
  "Delay for a time for prompting out the message, so the user
can see the error/operation message.")

;;; Recent Files
(setq recentf-max-menu-items 25)

(advice-add 'recentf-track-opened-file
            :after #'jcs-dashboard-refresh-buffer)

;;; Read Only
(defconst jcs-find-file-read-only-paths '("/.emacs.d/elisp/"
                                          "/.emacs.d/elpa/"
                                          "/lisp/")
  "When `find-file' under these paths, enable `read-only-mode' as default when opens it.")

;;; Scroll bar
(scroll-bar-mode -1)

;;; Scrolling
;;(setq scroll-preserve-screen-position 'always)

;;; Shift Select
;; NOTE: This act weird, does not make it works like other editor.
(setq shift-select-mode nil)

;;; Smooth scroll
(setq scroll-step 2)

;;; Startup windowing
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

;;; Tab / Space
(setq-default indent-tabs-mode nil)          ;; Disable inset tabs, insert space only
(setq-default tab-width 4)

;;; Toolbar
(tool-bar-mode -1)

;;; Uniquify
;; NOTE: meaningful names for buffers with the same name from prelude.
;; SOURCE: http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


(provide 'jcs-env)
;;; jcs-env.el ends here
