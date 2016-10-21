;; This is the start of .emacs file
;;------------------------------------------------------------------------------------------------------

;; This is my super-poopy .emacs file.
;; I barely know how to program LISP, and I know
;; even less about ELISP.  So take everything in
;; this file with a grain of salt!
;;
;; - Casey
;; - JenChieh (Modefied)

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

;; Setup my find-files
(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

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
(setq fixme-modes '(c++-mode
                    c-mode
                    csharp-mode
                    emacs-lisp-mode
                    jdee
                    js2-mode
                    lua-mode
                    nasm-mode
                    python-mode
                    web-mode
                    ))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-debugging-face)
(make-face 'font-lock-source-face)
(make-face 'font-lock-temporary-face)
(make-face 'font-lock-optimize-face)
(make-face 'font-lock-description-face)
(make-face 'font-lock-idea-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(OPTIMIZE\\)" 1 'font-lock-optimize-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
           ("\\<\\(DESCRIPTION\\)" 1 'font-lock-description-face t)
           ("\\<\\(DEBUGGING\\)" 1 'font-lock-debugging-face t)
           ("\\<\\(TEMPORARY\\)" 1 'font-lock-temporary-face t)
           ("\\<\\(SOURCE\\)" 1 'font-lock-source-face t)
           ("\\<\\(IDEA\\)" 1 'font-lock-idea-face t)
           )))
      fixme-modes)

;; List of color: https://alexschroeder.ch/geocities/kensanata/colors.html
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-optimize-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-description-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-debugging-face "Turquoise" nil nil t nil t nil nil)
(modify-face 'font-lock-temporary-face "Turquoise" nil nil t nil t nil nil)
(modify-face 'font-lock-source-face "PaleTurquoise2" nil nil t nil t nil nil)
(modify-face 'font-lock-idea-face "green yellow" nil nil t nil t nil nil)

;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.h$"    .c++-mode)
         ("\\.hpp$"    .c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ) auto-mode-alist))

;; C++ indentation style
(defconst jcs-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "Casey's Big Fun C++ Style")


;; CC++ mode handling
(defun jcs-big-fun-c-hook ()
  ;; Set my style for the current buffer
  (c-add-style "BigFun" jcs-big-fun-c-style t)

  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun jcs-header-format ()
    "Format the given file as a header file."

    (interactive)

    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    (insert "#ifndef __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__\n")
    (jcs-global-file-info)
    (insert "#define __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__\n\n\n\n")
    (insert "#endif // __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__\n\n")
    )

  (defun jcs-source-format ()
    "Format the given file as a source file."
    (interactive)

    (jcs-global-file-info)

    ;; macro
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (jcs-source-format))
        ((string-match "[.]cin" buffer-file-name) (jcs-source-format))
        ((string-match "[.]h" buffer-file-name) (jcs-header-format))
        ((string-match "[.]hpp" buffer-file-name) (jcs-header-format))
        ((string-match "[.]c" buffer-file-name) (jcs-source-format))
        ((string-match "[.]cpp" buffer-file-name) (jcs-source-format)))

  (defun casey-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
        (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file")))

  (defun casey-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (casey-find-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map [f12] 'casey-find-corresponding-file)
  (define-key c++-mode-map [M-f12] 'casey-find-corresponding-file-other-window)

  ;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'casey-save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "  " 'indent-region)
  ;;(define-key c++-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c++-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\e " 'set-mark-command)
  (define-key c++-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)

  ;; jcs-added
  (define-key c++-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key c++-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'casey-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(casey-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))

  ;; Turn on line numbers
  ;;(linum-mode)
  ;; or Turn on globale line numbers
  (global-linum-mode)
  )

(defun casey-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))
(define-key global-map [f8] 'casey-replace-string)

(add-hook 'c-mode-common-hook 'jcs-big-fun-c-hook)

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
(setq compilation-context-lines 0)
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
(define-key global-map (kbd "C-S-B") 'make-without-asking) ;; default "\em"

;; Commands
(set-variable 'grep-command "grep -irHn ")
(when casey-win32
  (setq grep-use-null-device t)
  (set-variable 'grep-command "findstr -s -n -i -l "))

;; Smooth scroll
(setq scroll-step 2)

;; Clock
(display-time)

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
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(jdee-jdk-registry
   (quote
    (("1.8.0_77" . "C:\\Program Files\\Java\\jdk1.8.0_11"))))
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
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


;;------------------------------------------------------------------------------------------------------
;; jcs initialize emacs lisp

;; ==================
;; [IMPORTANT] This should be ontop of all require packages!!!

;; start package.el with emacs
(require 'package)

;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; initialize package.el
(package-initialize)

;; ==================

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


;;------------------------------------------------------------------------------------------------------
;;;
;; Auto install list of packages i want at the startup of emacs.
;;;

;; Ensure all the package installed
;; Source -> http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; How to use?
(ensure-package-installed 'actionscript-mode  ;; for text related mode
                          'ac-php             ;; auto complete php
                          'ac-html            ;; auto complete html
                          'ac-js2
                          'ac-emmet
                          'ace-window
                          'auto-complete
                          'auto-complete-c-headers
                          'auto-install
                          'auto-package-update
                          'batch-mode
                          'better-defaults
                          'blank-mode
                          'cmake-font-lock
                          'cmake-ide
                          'cmake-mode
                          'cmake-project
                          'cpputils-cmake
                          'csharp-mode
                          'dash
                          'ein
                          'elpy
                          'emmet-mode
                          'flymake-cursor
                          'flymake-easy
                          'flymake-google-cpplint
                          'gitconfig-mode
                          'github-notifier
                          'gitignore-mode
                          'gitlab
                          'google-c-style
                          'rainbow-mode
                          'lua-mode
                          'multiple-cursors
                          'nasm-mode
                          'php-auto-yasnippets
                          'py-autopep8
                          'python-mode
                          'jdee
                          'js2-mode
                          'js2-refactor
                          'json-mode
                          'java-imports
                          'neotree
                          'impatient-mode
                          'web-mode
                          'yasnippet)


;; activate installed packages
(package-initialize)

;;========================================
;;      JENCHIEH FILE LOADING
;;----------------------------------
(load-file "~/.emacs.d/elisp/jcs-mode.el")
(load-file "~/.emacs.d/elisp/jcs-file-info-format.el")
(load-file "~/.emacs.d/elisp/jcs-function.el")


;;========================================
;;      JENCHIEH KEY GLOBAL KEY
;;----------------------------------

;; unbind the key
(global-unset-key "\C-k")

;; bind the key
(global-set-key "\C-cd" 'duplicate-line)
(global-set-key (kbd "C-d") 'kill-whole-line)   ;; Emacs default version
;;(global-set-key (kbd "C-d") 'my-delete-line)    ;; delete the line without copying!!
(define-key global-map "\C-x\C-x" 'kill-region)
(define-key global-map "\C-c\C-c" 'kill-ring-save)
(global-set-key "\C-v" 'yank)
(global-set-key "\C-s" 'jcs-save-buffer)
(global-set-key "\C-f" 'isearch-forward)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key "\C-z" 'undo)
(global-set-key (kbd "C-/") 'comment-region)
(global-set-key (kbd "C-?") 'uncomment-region)
(global-set-key "\C-k\C-c" 'comment-region)
(global-set-key "\C-k\C-u" 'uncomment-region)
(global-set-key "\C-p" 'package-list-packages)
(global-set-key "\e=" 'text-scale-increase)
(global-set-key "\e-" 'text-scale-decrease)
(global-set-key (kbd "C-<backspace>") 'my-backward-delete-word)
(global-set-key "\C-t" 'transient-mark-mode)
(define-key global-map [home] 'back-to-indentation-or-beginning)

(global-set-key "\em" 'describe-mode)
(global-set-key "\C-cd" 'jcs-toggle-shell-window) ; shell command
(global-set-key "\C-k\C-f" 'indent-region)
(global-set-key "\C-k\C-d" 'jcs-format-document)
(global-set-key "\C-xn" 'jcs-new-window)
(global-set-key "\C-xd" 'delete-frame)  ; delete the external window

(global-set-key "\e`" 'jcs-mode-toggle)

(global-set-key (kbd "M-<f12>") 'jcs-find-file-other-window)

(define-key global-map (kbd "S-<home>") 'jcs-smart-select-home)
(define-key global-map (kbd "S-<end>") 'jcs-smart-select-end)
(global-set-key (kbd "<up>") 'jcs-smart-indent-up)
(global-set-key (kbd "<down>") 'jcs-smart-indent-down)

(define-key global-map "\e]" 'run-without-asking)        ;; ALT-]
(global-set-key (kbd "C-<f5>") 'run-without-asking)

(global-set-key "\er" 'revert-buffer-no-confirm)

(define-key global-map "\ew" 'jcs-other-window-next)
(define-key global-map "\eq" 'jcs-other-window-prev)

;; switch line-ending key
(global-set-key "\C-x\C-e" 'set-buffer-file-coding-system)

;; Upper/Down case key binding.
(define-key global-map "\eu" 'upcase-word)
(define-key global-map "\ed" 'downcase-word)

;; ace window
(require 'ace-window)
(define-key global-map "\ee" 'ace-window)


;;------------------------------
;; ENABLE / DISABLE THE MODE
;;------------------------------
;;=============
;; Compile lanauge!
;;=============
;;(require 'c-mode)                     ;; c/c++
;;(global-set-key "\C-cc" 'c-mode)
(require 'jdee)                         ;; Java
(global-set-key "\C-cj" 'jdee-mode)

;;=============
;; Scripting/Interpreter
;;=============
(require 'php-mode)                     ;; PHP
(global-set-key "\C-xp" 'php-mode)
(require 'web-mode)                     ;; html, css, js
(global-set-key "\C-xw" 'web-mode)
(require 'js2-mode)                     ;; js
(global-set-key "\C-xj" 'js2-mode)

;;=============
;; Cross Language support
;;=============
(require 'rainbow-mode)
(global-set-key "\C-xr" 'rainbow-mode)
(require 'blank-mode)
(global-set-key "\C-xb" 'blank-mode)

;;------------------------------------------------------------------------------------------------------
;;; View Mode
;;------------------------------------------------------------------------------------------------------

(defun jcs-view-mode-hook()
  "In view mode, read only file."
  (interactive)

  ;; unset all the key
  (define-key view-mode-map "a" nil)
  (define-key view-mode-map "b" nil)
  (define-key view-mode-map "c" nil)
  (define-key view-mode-map "d" nil)
  (define-key view-mode-map "e" nil)
  (define-key view-mode-map "f" nil)
  (define-key view-mode-map "g" nil)
  (define-key view-mode-map "h" nil)
  (define-key view-mode-map "i" nil)
  (define-key view-mode-map "j" nil)
  (define-key view-mode-map "k" nil)
  (define-key view-mode-map "l" nil)
  (define-key view-mode-map "m" nil)
  (define-key view-mode-map "n" nil)
  (define-key view-mode-map "o" nil)
  (define-key view-mode-map "p" nil)
  (define-key view-mode-map "q" nil)
  (define-key view-mode-map "r" nil)
  (define-key view-mode-map "s" nil)
  (define-key view-mode-map "t" nil)
  (define-key view-mode-map "u" nil)
  (define-key view-mode-map "v" nil)
  (define-key view-mode-map "w" nil)
  (define-key view-mode-map "x" nil)
  (define-key view-mode-map "y" nil)
  (define-key view-mode-map "z" nil)
  (define-key view-mode-map "," nil)
  (define-key view-mode-map "\\" nil)
  (define-key view-mode-map "." nil)
  (define-key view-mode-map "," nil)
  (define-key view-mode-map "/" nil)
  (define-key view-mode-map "'" nil)
  (define-key view-mode-map " " nil)
  (define-key view-mode-map [tab] nil)
  (define-key view-mode-map (kbd "RET") nil)
  (define-key view-mode-map [space] nil)

  ;; just save buffer, don't care about the tab or spaces.
  (define-key view-mode-map "\C-s" 'save-buffer)
  )
(add-hook 'view-mode-hook 'jcs-view-mode-hook)


;;------------------------------------------------------------------------------------------------------
;; Text Related mode
;;------------------------------------------------------------------------------------------------------

;;===============
;; Blank Mode
;;-------------
(require 'blank-mode)
(autoload 'blank-mode           "blank-mode" "Toggle blank visualization."        t)
(autoload 'blank-toggle-options "blank-mode" "Toggle local `blank-mode' options." t)

;;===============
;; Neo Tree
;;-------------
(require 'neotree)
(setq neo-window-position)              ;; set window to the right
(setq neo-smart-open t)


(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-M-l") 'neotree-toggle)

;;==========================
;;   Folding Settings
;;------------------------
(outline-minor-mode t)      ; turn on the folding
(global-set-key (kbd "C-M-o") 'hide-other)
(global-set-key (kbd "C-M-p") 'show-all)

;;====================================
;; jcs java mode handling
;;---------------------------
(require 'gitignore-mode)
(defun jcs-gitignore-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; jcs gitignore key binding
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  (define-key gitignore-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key gitignore-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key gitignore-mode-map (kbd "<up>") 'previous-line)
  (define-key gitignore-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'gitignore-mode-hook 'jcs-gitignore-mode-hook)

;; git file types
(add-to-list 'auto-mode-alist '("\\.gitignore?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.gitattributes?\\'" . gitignore-mode))

;; temporary mode for all Text related!
(add-to-list 'auto-mode-alist '("\\.txt?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.bin?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.md?\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.jcs?\\'" . gitignore-mode))

;; temporary ALGOL
(add-to-list 'auto-mode-alist '("\\.alg?\\'" . gitignore-mode))

;;------------------------------------------------------------------------------------------------------
;; Makefile Mode.

(require 'cmake-mode)
(defun jcs-cmake-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; jcs makefile key binding
  (define-key cmake-mode-map (kbd "<up>") 'previous-line)
  (define-key cmake-mode-map (kbd "<down>") 'next-line)
  (define-key cmake-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key cmake-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key cmake-mode-map (kbd "<up>") 'previous-line)
  (define-key cmake-mode-map (kbd "<down>") 'next-line)

  ;; tabify save key
  (define-key cmake-mode-map "\C-s" 'jcs-tabify-save-buffer)
  )
(add-hook 'cmake-mode-hook 'jcs-cmake-mode-hook)

;; temporary makefile
(add-to-list 'auto-mode-alist '("\\.mak?\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)[Mm]akefile" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)makefile" . cmake-mode))

;;------------------------------------------------------------------------------------------------------
;;; Assembly mode

(require 'nasm-mode)
(defun jcs-nasm-mode-hook()
  ;;
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun jcs-asm-format ()
    "Format the given file as a asm code. - JenChieh Assembly Language"
    (interactive)

    (jcs-asm-file-format)

    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]asm" buffer-file-name) (jcs-asm-format))
        )

  ;; jcs key binding
  (define-key nasm-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key nasm-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key nasm-mode-map (kbd "<up>") 'previous-line)
  (define-key nasm-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'nasm-mode-hook 'jcs-nasm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.asm?\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.inc?\\'" . nasm-mode))

;;------------------------------------------------------------------------------------------------------
;;; Batch mode

(require 'batch-mode)
(defun jcs-batch-mode-hook ()
  ;;
  (electric-pair-mode nil)

  ;; jcs key binding
  (define-key batch-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key batch-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'batch-mode-hook 'jcs-batch-mode-hook)

(add-to-list 'auto-mode-alist '("\\.bat?\\'" . batch-mode))

;;------------------------------------------------------------------------------------------------------
;; Shell Script mode

(require 'sh-script)
(defun jcs-sh-script-hook()

  ;; jcs key binding
  (define-key sh-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key sh-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'sh-mode-hook 'jcs-sh-script-hook)

;;------------------------------------------------------------------------------------------------------
;;; C/C++ JCS_Mode

;; start auto-complete with emacs
(require 'auto-complete)

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; define a function which initializes auto-complete-c-headers and gets called for c/c++ hooks
(defun jcs-ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; here we adjust the c library we want to use,
  ;; current i am using MinGW because is cross os.
  (add-to-list 'achead:include-directories '"C:/MinGW/include")
  )
;; now lets' call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'jcs-ac-c-header-init)
(add-hook 'c-mode-hook 'jcs-ac-c-header-init)

;; Fix "iedit" bug for OSX
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; start flymake-google-cpplint-load
;; let's define a function for flymake initilization
(defun jcs-flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "C:/jcs_ide_packages/jcs_win7_packages/cpplint/cpplint.exe"))
  (flymake-google-cpplint-load)
  )
(add-hook 'c-mode-hook 'jcs-flymake-google-init)
(add-hook 'c++-mode-hook 'jcs-flymake-google-init)

;; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;;------------------------------------------------------------------------------------------------------
;; JCS Java Environment Settings

;;====================================
;;      jdee mode
;;---------------------------

(require 'jdee)
(defun jcs-big-fun-java-hook ()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun jcs-java-class-format ()

    "Format the given file as a class. - JenChieh Java class"

    (jcs-global-file-info)

    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

    (insert "\n\n")
    (insert "public class ")
    (push-mark)
    (insert BaseFileName)
    (pop-mark)
    (insert " {\n\n}\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]java" buffer-file-name) (jcs-java-class-format))
        )

  ;; jcs java key binding
  (define-key java-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key java-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; jcs Build and Run
  (define-key java-mode-map (kbd "C-S-B") 'make-without-asking)
  (define-key global-map [f5] 'run-without-asking)

  )

(add-hook 'jdee-mode-hook 'jcs-big-fun-java-hook)

(add-to-list 'auto-mode-alist '("\\.java?\\'" . jdee-mode))

;; (autoload 'jde-mode "~/.enacs.d/elpha/jdee-20160304.536/jdee.el" "JDE mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))

;;====================================
;;      Java Imports
;;---------------------------
(require 'java-imports)
;; whatever you want to bind it to
(define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim)

;; See customization below for where to put java imports
(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)

(add-hook 'java-mode-hook 'java-imports-scan-file)


;;------------------------------------------------------------------------------------------------------
;; JCS ActionScript 3.0 Environment setting
(require 'actionscript-mode)
(defun jcs-action-script-mode-hook ()
  ;;

  ;; jcs java key binding
  (define-key actionscript-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key actionscript-mode-map "\C-c\C-c" 'kill-ring-save)
  )

(add-hook 'actionscript-mode-hook 'jcs-action-script-mode-hook)


;;------------------------------------------------------------------------------------------------------
;; JCS Python environment setting

;;====================================
;;      Python-Mode
;;---------------------------

(require 'python-mode)
(defun jcs-python-mode-hook ()
  ;; enable the stuff you want for Python here
  (electric-pair-mode nil)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun jcs-python-class-format ()

    "Format the given file as a class. - JenChieh Java class"

    (jcs-manage-file-info)

    ;; define macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))
    (insert "\n")

    (insert "class ")
    (insert BaseFileName)
    (insert ":\n\n")

    (insert "    \"\"\"TODO(jenchieh): Class Description here...\"\"\"")
    (insert "\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # Public Variables\n")
    (insert "    # --------------------------------------------\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # Private Variables\n")
    (insert "    # --------------------------------------------\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # Protected Variables\n")
    (insert "    # --------------------------------------------\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # Constructor\n")
    (insert "    # --------------------------------------------\n")
    (insert "    def __init__(self):\n\n")
    (insert "    # --------------------------------------------\n")
    (insert "    # Public Methods\n")
    (insert "    # --------------------------------------------\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # Protected Methods\n")
    (insert "    # --------------------------------------------\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # Private Methods\n")
    (insert "    # --------------------------------------------\n\n")

    (insert "    # --------------------------------------------\n")
    (insert "    # setter / getter\n")
    (insert "    # --------------------------------------------\n\n")

    ;; Move to beginning of the buffer.
    (beginning-of-buffer)
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]py" buffer-file-name) (jcs-python-class-format))
        )

  ;; jcs java key binding
  (define-key python-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key python-mode-map "\C-c\C-c" 'kill-ring-save)
  (define-key python-mode-map [C-backspace] 'backward-kill-word)

  (define-key global-map [M-up] 'previous-blank-line)
  (define-key global-map [M-down] 'next-blank-line)

  (define-key python-mode-map (kbd "<up>") 'previous-line)
  (define-key python-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'python-mode-hook 'jcs-python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.py?\\'" . python-mode))

(require 'elpy)
;;(elpy-enable)

(require 'ein)
;;(elpy-use-ipython)

;; enable autopep8 formatting on save
(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;------------------------------------------------------------------------------------------------------
;; JCS_WebDevelopment Mode

;; Note for "Impatient Mode" (real time editing)
;; Step 1: M-x httpd-start        (Open the port default: 8080)
;; Step 2: M-x impatient-mode     (Enabled Impatient Mode)

;; ======================
;; web-mode.el
;; homepage - http://web-mode.org/

;; Load path

;; list of  extensions that will take effect of this mode
(require 'web-mode)
(defun jcs-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)

  ;; Abbrevation expansion
  (abbrev-mode 1)



  (defun jcs-html-format ()
    "Format the give file. - JenChieh HTML file"
    (interactive)

    ;; macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

    ;; insert tag header
    (jcs-tag-file-info)

    ;; insert HTML common format
    (insert "<!Doctype html>\n")
    (insert "<html>\n")
    (insert "<head>\n")
    (insert "<meta charset=\"UTF-8\">\n\n")
    (insert "<title>")
    (push-mark)
    (insert BaseFileName)
    (pop-mark)
    (insert "</title>\n")
    (insert "</head>\n")
    (insert "<body>\n\n\n")
    (insert "</body>\n")
    (insert "</html>\n")
    )

  (defun jcs-php-format ()
    "Format the give file. - JenChieh PHP file"
    (interactive)

    ;; insert tag header
    (jcs-tag-file-info)

    ;; insert PHP common format
    (insert "<?php\n\n")
    (insert "?>\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]html" buffer-file-name) (jcs-html-format))
        ((string-match "[.]php" buffer-file-name) (jcs-php-format))
        )

  ;; jcs web key binding
  (define-key web-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key web-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'web-mode-hook  'jcs-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))       ;; Add .php to the list

;; Associate an engine
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

;; Associate a content type
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("json" . "/some/path/.*\\.api\\'")
        ("xml"  . "/other/path/.*\\.api\\'")
        ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))


;;=======================

;; Indentation
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

;; Left padding
(setq web-mode-style-padding 1)
(setq web-mode-script-padding 1)
(setq web-mode-block-padding 0)

;; Comments
;;(setq web-mode-comment-style 2)

;; Syntax Highlighting
(set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")

;; Shortcuts
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)

;; Snippets
(setq web-mode-extra-snippets
      '(("erb" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
        ("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
                  ("debug" . ("<?php error_log(__LINE__); ?>"))))
        ))

;; Auto-pairs
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
        ))

;; Enable / disable features
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)

;; Keywords / Constants
;;(setq web-mode-extra-constants '(("php" . ("CONS1" "CONS2")))

;; Current eletemt / column highlight
(setq web-mode-enable-current-element-highlight t)

;; Context-aware auto-completion
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;;=====================
;; ac-html
(defun setup-ac-for-haml ()
  ;; Require ac-haml since we are setup haml auto completion
  (require 'ac-haml)
  ;; Require default data provider if you want to use
  (require 'ac-html-default-data-provider)
  ;; Enable data providers,
  ;; currently only default data provider available
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  ;; Let ac-haml do some setup
  (ac-haml-setup)
  ;; Set your ac-source
  (setq ac-sources '(ac-source-haml-tag
                     ac-source-haml-attr
                     ac-source-haml-attrv))
  ;; Enable auto complete mode
  (auto-complete-mode))
(add-hook 'haml-mode-hook 'setup-ac-for-haml)

;; ac-php
(add-hook 'php-mode-hook '(lambda ()
                            (auto-complete-mode t)
                            (require 'ac-php)
                            (setq ac-sources  '(ac-source-php ) )
                            (yas-global-mode 1)

                            (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                            (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back

                            ;; jcs java key binding
                            (define-key php-mode-map (kbd "C-d") 'kill-whole-line)
                            (define-key php-mode-map "\C-c\C-c" 'kill-ring-save)
                            ))


;;============================
;; CSS editing

(require 'rainbow-mode)


;; Source: -> CSS Mode: https://www.emacswiki.org/emacs/css-mode.el
;;         -> Xah CSS Mode: http://ergoemacs.org/emacs/xah-css-mode.html
(load-file "~/.emacs.d/elisp/css-mode.el")
(require 'css-mode)
(defun jcs-css-mode-hook ()
  (defun jcs-css-format()
    "Format the give file. - JenChieh CSS file"
    (interactive)

    (jcs-global-file-info)
    (insert "\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]css" buffer-file-name) (jcs-css-format))
        )
  )
(add-hook 'css-mode-hook  'jcs-css-mode-hook)

(add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode))


;;============================
;; JavaScript editing

(require 'js2-mode)
;; self define javascript mode here!
(defun jcs-javascript-mode-hook ()

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  ;; enable the stuff you want for JavaScript here
  (electric-pair-mode 1)

  (defun jcs-javascript-format()
    (interactive)

    ;; define macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    ;; start designing the header.

    ;; insert the file info header.
    (jcs-global-file-info)

    ;; do JavaScript specific thing here...
    (insert "\n\n") ;; currently nothing.

    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]js" buffer-file-name) (jcs-javascript-format))
        ((string-match "[.]json" buffer-file-name) (jcs-javascript-format))
        )

  ;; jcs java key binding
  (define-key js2-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key js2-mode-map "\C-c\C-c" 'kill-ring-save)
  )
(add-hook 'js2-mode-hook 'jcs-javascript-mode-hook)

(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json?\\'" . js2-mode))

(require 'ac-js2)
(setq ac-js2-evaluate-calls t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;------------------------------------------------------------------------------------------------------
;; JCS Lua environment setting

(require 'lua-mode)
(defun jcs-lua-mode-hook ()
  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)

  (defun jcs-lua-script-format ()

    "Format the given file as a class. - JenChieh Lua Script"

    (interactive)

    ;; macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    (insert "-- ========================================================================\n")
    (insert "-- $File: ")
    (insert BaseFileNameWithExtension)
    (insert " $\n")
    (insert "-- $Date: ")
    (jcs-timestamp)
    (insert " $\n")
    (insert "-- $Revision: $\n")
    (insert "-- $Creator: Jen-Chieh Shen $\n")
    (insert "-- $Notice: See LICENSE.txt for modification and distribution information $ \n")
    (insert "--                   Copyright (c) 2016 by Shen, Jen-Chieh $\n")
    (insert "-- ========================================================================\n")
    (insert "\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]lua" buffer-file-name) (jcs-lua-script-format))
        ((string-match "[.]luac" buffer-file-name) (jcs-lua-script-format))
        )

  ;; jcs Lua key binding
  (define-key lua-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key lua-mode-map "\C-c\C-c" 'kill-ring-save)

  )
(add-hook 'lua-mode-hook 'jcs-lua-mode-hook)

(add-to-list 'auto-mode-alist '("\\.lua?\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.luac?\\'" . lua-mode))


;;------------------------------------------------------------------------------------------------------
;; JCS C#(CSharp) environment setting


;;====================================
;;      CSharp-Mode
;;---------------------------

(require 'csharp-mode)
(defun jcs-csharp-mode-hook ()

  ;; Abbrevation expansion
  (abbrev-mode 1)

  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)

  (defun jcs-csharp-class-format ()

    "Format the given file as a class. - JenChieh C# class"

    (jcs-global-file-info)

    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

    (insert "\n\n")
    (insert "public class ")
    (push-mark)
    (insert BaseFileName)
    (pop-mark)
    (insert " {\n\n}\n\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]cs" buffer-file-name) (jcs-csharp-class-format))
        )

  ;; jcs C# key binding
  (define-key csharp-mode-map (kbd "C-d") 'kill-whole-line)
  (define-key csharp-mode-map "\C-c\C-c" 'kill-ring-save)

  )
(add-hook 'csharp-mode-hook 'jcs-csharp-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cs?\\'" . csharp-mode))

;; NOTE(JenChieh): .cs add-to-list action above will
;;                  override the .c file c++-mode add-to-list
;;                  action, so i have to override it agian
;;                  so it won't cover the setting i want...
(add-to-list 'auto-mode-alist '("\\.c?\\'" . c++-mode))


;;------------------------------------------------------------------------------------------------------
;; This is the end of .emacs file
