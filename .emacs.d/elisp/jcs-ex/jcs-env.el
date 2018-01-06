;; This is the start of jcs-env.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-env.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2017-05-29 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2017 Jen-Chieh Shen

;; jcs-env is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-env is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;================================================
;; JayCeS Environment Settings.
;;================================================

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)


;; Determine the underlying operating system
(setq casey-aquamacs (featurep 'aquamacs))
(setq casey-linux (featurep 'x))
(setq casey-win32 (not (or casey-aquamacs casey-linux)))

(setq casey-todo-file "C:/TODO_JenChieh/code/todo.txt")
(setq casey-log-file "C:/TODO_JenChieh/code/log.txt")

(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")


(setq compilation-directory-locked nil)
(scroll-bar-mode -1)
(setq enable-local-variables nil)
(setq casey-font "outline-DejaVu Sans Mono")

;; TODO file.
(setq jcs-todo-file "TODO(jenchieh)")
;; Log file.
(setq jcs-update-log-file "Update_Log")

(when casey-win32
  (setq casey-makescript "build.bat")
  (setq jcs-runscript "run.bat")
  (setq casey-font "outline-Liberation Mono"))

(when casey-aquamacs
  (cua-mode 0)
  (osx-key-mode 0)
  (tabar-mode 0)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (scroll-bar-mode nil)
  (setq mac-pass-command-to-system nil)
  (setq casey-makescript "./build.macosx")
  (setq jcs-runscript "./run.macosx"))

(when casey-linux
  (setq casey-makescript "./build.linux")
  (setq jcs-runscript "./run.linux")
  (display-battery-mode 1))

;; Turn off the toolbar
(tool-bar-mode 0)

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

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

(defun load-todo ()
  (interactive)
  (find-file casey-todo-file))

(defun insert-timeofday ()
  (interactive "*")
  (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p")))

(defun load-log ()
  (interactive)
  (find-file casey-log-file)
  (if (boundp 'longlines-mode) ()
    (longlines-mode 1)
    (longlines-show-hard-newlines))
  (if (equal longlines-mode t) ()
    (longlines-mode 1)
    (longlines-show-hard-newlines))
  (end-of-buffer)
  (newline-and-indent)
  (insert-timeofday)
  (newline-and-indent)
  (newline-and-indent)
  (end-of-buffer))


;; Bright-red TODOs
(defvar jcs-fixme-modes '(actionscript-mode
                          bat-mode
                          cc-mode
                          c-mode
                          c++-mode
                          cobol-mode
                          cmake-mode
                          csharp-mode
                          css-mode
                          emacs-lisp-mode
                          emmet-mode
                          go-mode
                          jdee-mode
                          js2-mode
                          lua-mode
                          nasm-mode
                          org-mode
                          php-mode
                          python-mode
                          sh-mode
                          web-mode
                          ))

;; List of color: https://alexschroeder.ch/geocities/kensanata/colors.html
(defface jcs-font-lock-fixme-face
  '((t (:foreground "red" :underline t :weight bold)))
  "Highlight word 'TODO'.")
(defvar jcs-font-lock-fixme-face 'jcs-font-lock-fixme-face)

(defface jcs-font-lock-attention-face
  '((t (:foreground "red" :underline t :weight bold)))
  "Highlight word 'ATTENTION'.")
(defvar jcs-font-lock-attention-face 'jcs-font-lock-attention-face)

(defface jcs-font-lock-study-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'STUDY'.")
(defvar jcs-font-lock-study-face 'jcs-font-lock-study-face)

(defface jcs-font-lock-important-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'IMPORTANT'.")
(defvar jcs-font-lock-important-face 'jcs-font-lock-important-face)

(defface jcs-font-lock-optimize-face
  '((t (:foreground "yellow" :underline t :weight bold)))
  "Highlight word 'OPTIMIZE'.")
(defvar jcs-font-lock-optimize-face 'jcs-font-lock-optimize-face)

(defface jcs-font-lock-note-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'NOTE'.")
(defvar jcs-font-lock-note-face 'jcs-font-lock-note-face)

(defface jcs-font-lock-description-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'DESCRIPTION'.")
(defvar jcs-font-lock-description-face 'jcs-font-lock-description-face)

(defface jcs-font-lock-tag-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'TAG'.")
(defvar jcs-font-lock-tag-face 'jcs-font-lock-tag-face)

(defface jcs-font-lock-debugging-face
  '((t (:foreground "turquoise" :underline t :weight bold)))
  "Highlight word 'DEBUGGING'.")
(defvar jcs-font-lock-debugging-face 'jcs-font-lock-debugging-face)

(defface jcs-font-lock-temporary-face
  '((t (:foreground "turquoise" :underline t :weight bold)))
  "Highlight word 'TEMPORARY'.")
(defvar jcs-font-lock-temporary-face 'jcs-font-lock-temporary-face)

(defface jcs-font-lock-source-face
  '((t (:foreground "PaleTurquoise2" :underline t :weight bold)))
  "Highlight word 'SOURCE'.")
(defvar jcs-font-lock-source-face 'jcs-font-lock-source-face)

(defface jcs-font-lock-url-face
  '((t (:foreground "PaleTurquoise2" :underline t :weight bold)))
  "Highlight word 'URL'.")
(defvar jcs-font-lock-url-face 'jcs-font-lock-url-face)

(defface jcs-font-lock-idea-face
  '((t (:foreground "green yellow" :underline t :weight bold)))
  "Highlight word 'IDEA'.")
(defvar jcs-font-lock-idea-face 'jcs-font-lock-idea-face)

(defface jcs-font-lock-obsolete-face
  '((t (:foreground "DarkOrange3" :underline t :weight bold)))
  "Highlight word 'OBSOLETE'.")
(defvar jcs-font-lock-obsolete-face 'jcs-font-lock-obsolete-face)

(defface jcs-font-lock-deprecated-face
  '((t (:foreground "DarkOrange3" :underline t :weight bold)))
  "Highlight word 'DEPRECATED'.")
(defvar jcs-font-lock-deprecated-face 'jcs-font-lock-deprecated-face)

(defface jcs-font-lock-topic-face
  '((t (:foreground "slate blue" :underline t :weight bold)))
  "Highlight word 'TOPIC'.")
(defvar jcs-font-lock-topic-face 'jcs-font-lock-topic-face)

(defface jcs-font-lock-see-face
  '((t (:foreground "slate blue" :underline t :weight bold)))
  "Highlight word 'SEE'.")
(defvar jcs-font-lock-see-face 'jcs-font-lock-see-face)

(defface jcs-font-lock-option-face
  '((t (:foreground "dark green" :underline t :weight bold)))
  "Highlight word 'OPTION'.")
(defvar jcs-font-lock-option-face 'jcs-font-lock-option-face)

(defface jcs-font-lock-or-face
  '((t (:foreground "green yellow" :underline t :weight bold)))
  "Highlight word 'OR'.")
(defvar jcs-font-lock-or-face 'jcs-font-lock-or-face)

(defface jcs-font-lock-key-highlight-face
  '((t (:foreground "#38EFCA")))
  "Highlight word between ` and ' word.")
(defvar jcs-font-lock-key-highlight-face 'jcs-font-lock-key-highlight-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)\\>" 1 'jcs-font-lock-fixme-face t)
           ("\\<\\(ATTENTION\\)\\>" 1 'jcs-font-lock-attention-face t)
           ("\\<\\(STUDY\\)\\>" 1 'jcs-font-lock-study-face t)
           ("\\<\\(IMPORTANT\\)\\>" 1 'jcs-font-lock-important-face t)
           ("\\<\\(OPTIMIZE\\)\\>" 1 'jcs-font-lock-optimize-face t)
           ("\\<\\(NOTE\\)\\>" 1 'jcs-font-lock-note-face t)
           ("\\<\\(DESCRIPTION\\)\\>" 1 'jcs-font-lock-description-face t)
           ("\\<\\(TAG\\)\\>" 1 'jcs-font-lock-tag-face t)
           ("\\<\\(DEBUGGING\\)\\>" 1 'jcs-font-lock-debugging-face t)
           ("\\<\\(TEMPORARY\\)\\>" 1 'jcs-font-lock-temporary-face t)
           ("\\<\\(SOURCE\\)\\>" 1 'jcs-font-lock-source-face t)
           ("\\<\\(URL\\)\\>" 1 'jcs-font-lock-url-face t)
           ("\\<\\(IDEA\\)\\>" 1 'jcs-font-lock-idea-face t)
           ("\\<\\(OBSOLETE\\)\\>" 1 'jcs-font-lock-obsolete-face t)
           ("\\<\\(DEPRECATED\\)\\>" 1 'jcs-font-lock-deprecated-face t)
           ("\\<\\(TOPIC\\)\\>" 1 'jcs-font-lock-topic-face t)
           ("\\<\\(SEE\\)\\>" 1 'jcs-font-lock-see-face t)

           ("\\<\\(OPTION\\)\\>" 1 'jcs-font-lock-option-face t)
           ("\\<\\(OR\\)\\>" 1 'jcs-font-lock-or-face t)

           ("`\\([a-zA-Z0-9_ $]*\\)'" 'jcs-font-lock-key-highlight-face)
           )'end))
      jcs-fixme-modes)


(defun casey-replace-string (FromString ToString)
  "Replace a string without moving point.
FROMSTRING : String will be replaced.
TOSTRING : String will replaced."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)))

(defun casey-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

;; TXT mode handling
(defun casey-big-fun-text-hook ()
  "4-space tabs."
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Newline indents, semi-colon doesn't
  (define-key text-mode-map "\C-m" 'newline-and-indent)

  ;; Prevent overriding of alt-s
  (define-key text-mode-map "\es" 'casey-save-buffer)
  )
(add-hook 'text-mode-hook 'casey-big-fun-text-hook)

;; Window Commands
(defun w32-restore-frame ()
  "Restore a minimized frame."
  (interactive)
  (w32-send-sys-command 61728))

(defun maximize-frame ()
  "Maximize the current frame."
  (interactive)
  (when casey-aquamacs (aquamacs-toggle-full-frame))
  (when casey-win32 (w32-send-sys-command 61488)))

;; ALT-alternatives
;; (defadvice set-mark-command (after no-bloody-t-m-m activate)
;;   "Prevent consecutive marks activating bloody `transient-mark-mode'."
;;   (if transient-mark-mode (setq transient-mark-mode nil)))

;; (defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
;;   "Prevent mouse commands activating bloody `transient-mark-mode'."
;;   (if transient-mark-mode (setq transient-mark-mode nil)))

(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill)
  (copy-region-as-kill (mark) (point)))

(defun casey-replace-in-region (old-word new-word)
  "Perform a `replace-string' in the current region.
OLD-WORD : old word will be replaced.
NEW-WORD : new word ready to be insert."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
                    (narrow-to-region (mark) (point))
                    (goto-char (point-min))
                    (replace-string old-word new-word)
                    )))

;; Compilation
(setq compilation-context-lines t)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

(defun find-project-directory-recursive-run ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p casey-makescript) t
    (cd "../")
    (find-project-directory-recursive-run)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile."
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile."
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive-run)
    (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile casey-makescript))
  (other-window 1))

;; Commands
(set-variable 'grep-command "grep -irHn ")
(when casey-win32
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

(defun casey-never-split-a-window
    "Never, ever split a window.  Why would anyone EVER want you to do that??"
  nil)
(setq split-window-preferred-function 'casey-never-split-a-window)

;;(add-to-list 'default-frame-alist '(font . "Liberation Mono-14.0"))                     ;; default [11.5]
;;(set-face-attribute 'default t :font "Liberation Mono-11.5")                            ;; default [11.5]
;;(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "#38EFCA")
;;(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#D2D2D2")            ;; function name
(set-face-attribute 'font-lock-keyword-face nil :foreground "#17A0FB")                  ;; data type
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "#38EFCA")                     ;; class name
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#D2D2D2")            ;; declare name

(defun post-load-stuff ()
  "Post load stuff."
  (interactive)
  (menu-bar-mode -1)
  (maximize-frame)
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

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-env.el file
