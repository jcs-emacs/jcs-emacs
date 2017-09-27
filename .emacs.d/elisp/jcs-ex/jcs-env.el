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

(when casey-win32
  (setq casey-makescript "build.bat")
  (setq jenchieh-runscript "run.bat")
  (setq casey-font "outline-Liberation Mono")
  )

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
  (setq jenchieh-runscript "./run.macosx")
  )

(when casey-linux
  (setq casey-makescript "./build.linux")
  (setq jenchieh-runscript "./run.linux")
  (display-battery-mode 1)
  )

;; Turn off the toolbar
(tool-bar-mode 0)

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; Setup my key binding
(global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
(global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

(defun casey-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
  )
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
  (find-file casey-todo-file)
  )
(define-key global-map "\et" 'load-todo)

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
  (end-of-buffer)
  )
(define-key global-map "\eT" 'load-log)

;; no screwing with my middle mouse button
(global-unset-key [mouse-2])

;; Bright-red TODOs
(setq fixme-modes '(ac-php
                    actionscript-mode
                    batch-mode
                    c++-mode
                    c-mode
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

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-attention-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-debugging-face)
(make-face 'font-lock-source-face)
(make-face 'font-lock-temporary-face)
(make-face 'font-lock-optimize-face)
(make-face 'font-lock-description-face)
(make-face 'font-lock-idea-face)
(make-face 'font-lock-obsolete-face)
(make-face 'font-lock-deprecated-face)
(make-face 'font-lock-tag-face)
(make-face 'font-lock-topic-face)
(make-face 'font-lock-url-face)
(make-face 'font-lock-see-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(ATTENTION\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(OPTIMIZE\\)" 1 'font-lock-optimize-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
           ("\\<\\(DESCRIPTION\\)" 1 'font-lock-description-face t)
           ("\\<\\(TAG\\)" 1 'font-lock-tag-face t)
           ("\\<\\(DEBUGGING\\)" 1 'font-lock-debugging-face t)
           ("\\<\\(TEMPORARY\\)" 1 'font-lock-temporary-face t)
           ("\\<\\(SOURCE\\)" 1 'font-lock-source-face t)
           ("\\<\\(URL\\)" 1 'font-lock-url-face t)
           ("\\<\\(IDEA\\)" 1 'font-lock-idea-face t)
           ("\\<\\(OBSOLETE\\)" 1 'font-lock-obsolete-face t)
           ("\\<\\(DEPRECATED\\)" 1 'font-lock-deprecated-face t)
           ("\\<\\(TOPIC\\)" 1 'font-lock-topic-face t)
           ("\\<\\(SEE\\)" 1 'font-lock-see-face t)
           )))
      fixme-modes)

;; List of color: https://alexschroeder.ch/geocities/kensanata/colors.html
(modify-face 'font-lock-fixme-face "red" nil nil t nil t nil nil)
(modify-face 'font-lock-attention-face "red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-optimize-face "yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "dark green" nil nil t nil t nil nil)
(modify-face 'font-lock-description-face "dark green" nil nil t nil t nil nil)
(modify-face 'font-lock-tag-face "dark green" nil nil t nil t nil nil)
(modify-face 'font-lock-debugging-face "turquoise" nil nil t nil t nil nil)
(modify-face 'font-lock-temporary-face "turquoise" nil nil t nil t nil nil)
(modify-face 'font-lock-source-face "PaleTurquoise2" nil nil t nil t nil nil)
(modify-face 'font-lock-url-face "PaleTurquoise2" nil nil t nil t nil nil)
(modify-face 'font-lock-idea-face "green yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-obsolete-face "DarkOrange3" nil nil t nil t nil nil)
(modify-face 'font-lock-deprecated-face "DarkOrange3" nil nil t nil t nil nil)
(modify-face 'font-lock-topic-face "slate blue" nil nil t nil t nil nil)
(modify-face 'font-lock-see-face "slate blue" nil nil t nil t nil nil)


(defun casey-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))
(define-key global-map [f8] 'casey-replace-string)

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
  ;; 4-space tabs
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
  "Restore a minimized frame"
  (interactive)
  (w32-send-sys-command 61728))

(defun maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (when casey-aquamacs (aquamacs-toggle-full-frame))
  (when casey-win32 (w32-send-sys-command 61488)))

(define-key global-map "\ep" 'quick-calc)

;; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
  )

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
  )

(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
;;(define-key global-map [home] 'beginning-of-line)
;;(define-key global-map [home] 'back-to-indentation)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)

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
  (copy-region-as-kill (mark) (point))
  )
(define-key global-map "\e " 'set-mark-command)
(define-key global-map "\eq" 'jcs-other-window-prev)
(define-key global-map "\ea" 'yank)
(define-key global-map "\ez" 'kill-region)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)

(define-key global-map "\e:" 'View-back-to-mark)
(define-key global-map "\e;" 'exchange-point-and-mark)

(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)   ;; 'ALT-SHIFT-n' = '\eN'

(define-key global-map "\eg" 'goto-line)
(define-key global-map "\ej" 'imenu)

;; Editting
(define-key global-map "" 'copy-region-as-kill)
(define-key global-map "" 'yank)
(define-key global-map "" 'nil)
(define-key global-map "" 'rotate-yank-pointer)
(define-key global-map "\eu" 'undo)
(define-key global-map "\e6" 'upcase-word)
(define-key global-map "\e^" 'captilize-word)
(define-key global-map "\e." 'fill-paragraph)

(defun casey-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
                    (narrow-to-region (mark) (point))
                    (beginning-of-buffer)
                    (replace-string old-word new-word)
                    ))
  )
(define-key global-map "\el" 'casey-replace-in-region)

(define-key global-map "\eo" 'query-replace)
(define-key global-map "\eO" 'casey-replace-string)

;; \377 is alt-backspace
(define-key global-map "\377" 'backward-kill-word)
(define-key global-map [M-delete] 'kill-word)

(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)

;; Buffers
(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)

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
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
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
(global-auto-revert-mode)

;; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(flymake-google-cpplint-command "C:/jcs_ide_packages/jcs_win7_packages/cpplint/cpplint.exe")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(jdee-jdk-registry
   (quote
    (("1.8.0_111" . "C:/Program Files/Java/jdk1.8.0_111"))))
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(package-selected-packages
   (quote
    (preproc-font-lock sublimity tree-mode rainbow-mode py-autopep8 multi-web-mode jdee java-imports impatient-mode iedit helm-gtags google-c-style gitlab gitignore-mode github-notifier gitconfig-mode flymake-google-cpplint flymake-cursor elpy ein cpputils-cmake cmake-project cmake-ide cmake-font-lock blank-mode better-defaults batch-mode auto-package-update auto-install auto-complete-c-headers actionscript-mode ace-window ac-php ac-js2 ac-html ac-emmet)))
 '(send-mail-function (quote mailclient-send-it))
 '(version-control nil))

(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-for-tab-command)
(define-key global-map "\C-y" 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
;;(define-key global-map "      " 'indent-region)

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
(global-linum-mode)                          ;; Global Line Number
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


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-env.el file
