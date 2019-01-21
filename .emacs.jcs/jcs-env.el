;; ========================================================================
;; $File: jcs-env.el $
;; $Date: 2017-05-29 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;================================================
;; JayCeS Environment Settings.
;;================================================

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)


;; Determine the underlying operating system
(defvar jcs-win32 nil
  "Is Microsoft Windows?")
(defvar jcs-aquamacs nil
  "Is Mac OS X?")
(defvar jcs-linux nil
  "Is Linux?")

;; Get OS type
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


(defvar jcs-daily-todo-file "C:/TODO_JenChieh/code/todo.txt"
  "Open the daily todo file.")
(defvar jcs-log-file "C:/TODO_JenChieh/code/log.txt"
  "Log file???")

(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")


(scroll-bar-mode -1)
(setq enable-local-variables nil)

;; TODO file.
(defvar jcs-project-todo-file "TODO"
  "Project TODO file.")
;; Log file.
(defvar jcs-project-update-log-file "Update_Log"
  "Project Update Log file.")

(defvar jcs-makescript ""
  "Make script file name depends on the current OS.")
(defvar jcs-runscript ""
  "Run script file name depends on the current OS.")

(when jcs-win32
  (setq jcs-makescript "build.bat")
  (setq jcs-runscript "run.bat"))

(when jcs-aquamacs
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
  (setq mac-pass-command-to-system nil)
  (setq jcs-makescript "./build.macosx")
  (setq jcs-runscript "./run.macosx"))

(when jcs-linux
  (setq jcs-makescript "./build.linux")
  (setq jcs-runscript "./run.linux")
  (display-battery-mode 1))

;; Turn off the toolbar
(tool-bar-mode 0)

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
;; NOTE(jenchieh): After helm version 20181125.17147, must disable this.
;; INFO(jenchieh): https://github.com/emacs-helm/helm/issues/2116#issuecomment-441649358
;;   => Ido is incompatible with Helm-mode.
;;(ido-mode t)

(defun casey-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))
(setq ediff-window-setup-function 'casey-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

;; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Setup my compilation mode
(defun jcs-big-fun-compilation-hook ()
  (interactive)

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


;; Maximize my Emacs frame on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ALT-alternatives
;; (defadvice set-mark-command (after no-bloody-t-m-m activate)
;;   "Prevent consecutive marks activating bloody `transient-mark-mode'."
;;   (if transient-mark-mode (setq transient-mark-mode nil)))

;; (defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
;;   "Prevent mouse commands activating bloody `transient-mark-mode'."
;;   (if transient-mark-mode (setq transient-mark-mode nil)))


;; Compilation
(setq compilation-context-lines t)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

;; Commands
(set-variable 'grep-command "grep -irHn ")
(when jcs-win32
  (setq grep-use-null-device t)
  (set-variable 'grep-command "findstr -s -n -i -l "))

;; Smooth scroll
(setq scroll-step 2)

;; Clock
(display-time)

;; Absolutely no backup file.
(setq make-backup-files nil)

;; Auto reload buffer if the file changes.
(global-auto-revert-mode t)

;; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

;;(add-to-list 'default-frame-alist '(font . "Liberation Mono-14.0"))         ;; default [11.5]
;;(set-face-attribute 'default t :font "Liberation Mono-11.5")                ;; default [11.5]
;;(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "#38EFCA")
;;(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#D2D2D2")  ;; function name
(set-face-attribute 'font-lock-keyword-face nil :foreground "#17A0FB")        ;; data type
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "#38EFCA")           ;; class name
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#D2D2D2")  ;; declare name

(defun post-load-stuff ()
  "Post load stuff."
  (interactive)
  (menu-bar-mode -1)
  (set-foreground-color "#D2D2D2")           ;; text color
  (set-background-color "#161616")              ;; background color
  (set-cursor-color "#40FF40")                  ;; cursor color
  )
(add-hook 'window-setup-hook 'post-load-stuff t)

;; Set default Theme Color.
(add-to-list 'default-frame-alist '(foreground-color . "#D2D2D2"))
(add-to-list 'default-frame-alist '(background-color . "#161616"))
(add-to-list 'default-frame-alist '(cursor-color . "#40FF40"))



;; Unicode Environment
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; jcs editor setting
(global-linum-mode t)                        ;; Global Line Number
(setq-default indent-tabs-mode nil)          ;; Disable inset tabs, insert space only
(setq-default tab-width 4)
(setq tab-width 4)
(electric-pair-mode 1)                       ;; auto close bracket insertion. New in emacs 24
(setq shift-select-mode t)
(delete-selection-mode 1)                    ;; replace the hightlighted text!
(electric-indent-mode 1)

;; highlight the select region
(transient-mark-mode t)                    ;; default as nil

;; -- Font Size: The value is in 1/10pt, so 100 will give you 10pt, etc.
(set-face-attribute 'default nil :height 160)

;; SOURCE(jenchieh): https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;; Splash Screen
;;(setq fancy-splash-image t)
;;(setq fancy-splash-image-file "~/.emacs.d/splash_screen.png")

;;; Resolve performance issue moving around Unicode Text.
(setq inhibit-compacting-font-caches t)

;; Enable address mode.
(goto-address-mode t)

;; Set default major mode to `org-mode'.
(setq-default major-mode 'org-mode)

;;; Show paren mode.
;; NOTE(jenchieh): turn on highlight matching brackets
;; when cursor is on one
(show-paren-mode t)

;;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
