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
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (setq jcs-win32 t)))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq jcs-aquamacs t)))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (setq jcs-linux t))))


(defvar jcs-daily-todo-file "" "Open the daily todo file.")
(defvar jcs-log-file "" "Log file path, file location.")

(defvar jcs-project-todo-file "TODO" "Project TODO file.")
(defvar jcs-project-update-log-file "Update_Log" "Project Update Log file.")

(defvar jcs-makescript "" "Make script file name depends on the current OS.")
(defvar jcs-runscript "" "Run script file name depends on the current OS.")

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
       (setq x-select-enable-clipboard t)
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


;;; Auto Revert Files
(global-auto-revert-mode t)

;;; Backup Files
(setq make-backup-files nil)

;;; Bell
(defun nil-bell ())  ;; Turn off the bell on Mac OS X
(setq ring-bell-function 'nil-bell)

;;; Clock
(display-time)

;;; Compilation
(setq compilation-context-lines t)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

(defun jcs-big-fun-compilation-hook ()
  ;; make it look like the terminal,
  ;; so it won't jump to the next line
  ;; automatically.
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)

  ;; NOTE(JenChieh): Make specific font-size for this mode.
  ;; Source: http://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode
  (setq buffer-face-mode-face '(:height 120))  ;; default [:family "" :height 120]
  (buffer-face-mode)
  )
(add-hook 'compilation-mode-hook 'jcs-big-fun-compilation-hook)

;;; Commands
(set-variable 'grep-command "grep -irHn ")
(when jcs-win32
  (setq grep-use-null-device t)
  (set-variable 'grep-command "findstr -s -n -i -l "))

;; Default Major Mode
(setq-default major-mode 'org-mode)

;;; Ediff
(defun jcs-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))
(setq ediff-window-setup-function 'jcs-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

;;; Electric Pair
(electric-pair-mode 1)

;;; Font
;; -- Font Size: The value is in 1/10pt, so 100 will give you 10pt, etc.
(set-face-attribute 'default nil :height 160)

;;; Frame Title
;; SOURCE(jenchieh): https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;; Goto Address
(goto-address-mode t)

;;; Highlight Line
(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")

;;; Highlight Select Region
(transient-mark-mode t)

(delete-selection-mode 1)  ;; replace the hightlighted text!
(electric-indent-mode 1)

;;; Language Environment
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Resolve performance issue moving around Unicode Text.
(setq inhibit-compacting-font-caches t)

;; Line Numbers
(defvar jcs-line-numbers-ignore-buffers '("*dashboard*"
                                          "*GNU Emacs*"
                                          "*helm"
                                          "*Packages*")
  "List of buffers that you do not want to show line numbers in it.")

;;; Menu Bar
(menu-bar-mode -1)

;;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;; Screen
;;(setq scroll-preserve-screen-position 'always)

;; Scroll bar
(scroll-bar-mode -1)

;;; Shift Select
;; NOTE(jenchieh): This act weird, does not make it works
;; like other editor.
(setq shift-select-mode nil)

;;; Show Paren
;; NOTE(jenchieh): turn on highlight matching brackets
;; when cursor is on one
(show-paren-mode t)

;;; Smooth scroll
(setq scroll-step 2)

;;; Splash Screen
;;(setq fancy-splash-image t)
;;(setq fancy-splash-image-file "~/.emacs.d/splash_screen.png")

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
(tool-bar-mode 0)

;;; Uniquify
;; NOTE: meaningful names for buffers with the same name from
;; prelude.
;; SOURCE(jenchieh): http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


(use-package isearch
  :defer t
  :config
  (defun jcs-isearch-mode-hook ()
    "Paste the current symbol when `isearch' enabled."
    (when (use-region-p)
      (deactivate-mark)
      (ignore-errors
        (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))))
  (add-hook 'isearch-mode-hook #'jcs-isearch-mode-hook))


(use-package windmove
  :defer t
  :init
  (defvar jcs-windmove-max-move-count 25
    "Possible maximum windows count.")
  :config
  (setq windmove-wrap-around t))


(provide 'jcs-env)
;;; jcs-env.el ends here
