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
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-attention-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-optimize-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-description-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-tag-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-debugging-face "Turquoise" nil nil t nil t nil nil)
(modify-face 'font-lock-temporary-face "Turquoise" nil nil t nil t nil nil)
(modify-face 'font-lock-source-face "PaleTurquoise2" nil nil t nil t nil nil)
(modify-face 'font-lock-url-face "PaleTurquoise2" nil nil t nil t nil nil)
(modify-face 'font-lock-idea-face "green yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-obsolete-face "DarkOrange3" nil nil t nil t nil nil)
(modify-face 'font-lock-deprecated-face "DarkOrange3" nil nil t nil t nil nil)
(modify-face 'font-lock-topic-face "Slate Blue" nil nil t nil t nil nil)
(modify-face 'font-lock-see-face "Slate Blue" nil nil t nil t nil nil)



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


;; C/C++ mode handling
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
    (insert "#endif /* __")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H__ */\n")
    )

  (defun jcs-source-format ()
    "Format the given file as a source file."
    (interactive)

    (jcs-global-file-info)

    ;; macro
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))

    (insert "\n")
    (insert "#include \"")
    (insert BaseFileName)
    (insert ".h\"\n\n")
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

  (define-key c++-mode-map [f8] 'casey-find-corresponding-file)
  (define-key c++-mode-map [M-f8] 'casey-find-corresponding-file-other-window)

  (define-key c-mode-map [f8] 'casey-find-corresponding-file)
  (define-key c-mode-map [M-f8] 'casey-find-corresponding-file-other-window)

  ;; If just want to open the same file, not the
  ;; corresponding file.
  (define-key c++-mode-map [f7] 'jcs-find-file-other-window)
  (define-key c-mode-map [f7] 'jcs-find-file-other-window)

  ;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c-mode-map "\ec" 'casey-find-corresponding-file)
  (define-key c-mode-map "\eC" 'casey-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'casey-save-buffer)
  (define-key c-mode-map "\es" 'casey-save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "  " 'indent-region)
  ;;(define-key c++-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c-mode-map "\t" 'dabbrev-expand)
  (define-key c-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c-mode-map [C-tab] 'indent-region)
  (define-key c-mode-map "  " 'indent-region)
  ;;(define-key c-mode-map [tab] '(lambda () (interactive) (insert "    ")))

  (define-key c++-mode-map "\ej" 'imenu)
  (define-key c-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)
  (define-key c-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)
  (define-key c-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)

  (define-key c-mode-map "\eq" 'jcs-other-window-prev)
  (define-key c-mode-map "\ea" 'yank)
  (define-key c-mode-map "\ez" 'kill-region)

  ;; jcs-added
  (define-key c++-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key c++-mode-map "\C-c\C-c" 'kill-ring-save)

  (define-key c-mode-map (kbd "C-d") 'jcs-kill-whole-line)
  (define-key c-mode-map "\C-c\C-c" 'kill-ring-save)

  ;; Comment Block.
  (define-key c-mode-map (kbd "RET") 'jcs-smart-context-line-break)
  (define-key c++-mode-map (kbd "RET") 'jcs-smart-context-line-break)

  (define-key c-mode-map (kbd "*") 'jcs-c-comment-pair)
  (define-key c++-mode-map (kbd "*") 'jcs-c-comment-pair)

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
    (sublimity zencoding-mode web-mode tree-mode rainbow-mode python-mode py-autopep8 php-auto-yasnippets omnisharp neotree nasm-mode multi-web-mode lua-mode json-mode js2-refactor jdee java-imports impatient-mode iedit helm-gtags google-c-style gitlab gitignore-mode github-notifier gitconfig-mode flymake-google-cpplint flymake-cursor elpy ein cpputils-cmake cmake-project cmake-ide cmake-font-lock blank-mode better-defaults batch-mode auto-package-update auto-install auto-complete-c-headers actionscript-mode ace-window ac-php ac-js2 ac-html ac-emmet)))
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

;; SOURCE(jenchieh): https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


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
                          'helm
                          'helm-gtags
                          'jdee
                          'js2-mode
                          'js2-refactor
                          'json-mode
                          'java-imports
                          'lua-mode
                          'multiple-cursors
                          'nasm-mode
                          'neotree
                          'php-auto-yasnippets
                          'py-autopep8
                          'python-mode
                          'rainbow-mode
                          'sublimity
                          'impatient-mode
                          'web-mode
                          'yasnippet
                          'zencoding-mode)


;; activate installed packages
(package-initialize)

;;================================================
;; Find file in project plugin
;; SOURCE(jenchieh): https://github.com/technomancy/find-file-in-project
;;================================================
(load-file "~/.emacs.d/elisp/find-file-in-project.el")

(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
(autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
(autoload 'ffip-ivy-resume "find-file-in-project" nil t)

;; You prefer ido-mode?
;;(setq ffip-prefer-ido-mode t)


;;===============
;; Sublimity
;;-------------
;; URL(jenchieh): https://github.com/zk-phi/sublimity
(require 'sublimity)
(require 'sublimity-scroll)

;; default on or off?
;; NOTE(jenchieh): This also trigger the animate scrolling too.
(sublimity-mode 1)

;; Scroll Speed.
(setq sublimity-scroll-weight 2  ;; [Default : 2]
      sublimity-scroll-drift-length 2)  ;; [Default : 2]

(require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)

(setq sublimity-map-size 0)  ;; [Default : 10]
(setq sublimity-map-fraction 0.3)  ;; [Default : 0.3]
(setq sublimity-map-text-scale -7)  ;; [Default: -7]

;; NOTE(jenchieh): When a positive integer is set, buffer
;; width is truncated to this value and drawn centered. To
;; cancel this feature, set this value nil.
(setq sublimity-attractive-centering-width nil)  ;; [Default : 110]

;; NOTE(jenchieh): With the setting above, minimap is displayed
;; after 5 seconds of idle time. When sublimity-map-set-delay
;; is called with nil, then minimap is shown with no delay. This
;; defers from setting delay to 0, especially when used with
;; sublimity-scroll, in the sense that minimap looks not deleted
;; at all but gets worse performance.

;; ATTENTION(jenchieh): Set it to very hight so it will never
;; reach the timer error.
(sublimity-map-set-delay 40000000)

;; NOTE(jenchieh): sublimity-map-setup-hook will run when
;; minimap is created.
(add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))

;; NOTE(jenchieh): Following functions are available to hide
;; some UI parts.
;;(sublimity-attractive-hide-bars)
;;(sublimity-attractive-hide-vertical-border)
;;(sublimity-attractive-hide-fringes)
;;(sublimity-attractive-hide-modelines)

;;===============
;; Uniquify
;;-------------
;; meaningful names for buffers with the same name
;; from prelude
;; SOURCE(jenchieh): http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;;========================================
;;      JENCHIEH FILE LOADING
;;----------------------------------
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-file-info-format.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-function.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-global-key.el")
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-helm.el")

;;; jcs-all-mode
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-elisp-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-txt-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-cmake-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-nasm-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-batch-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-sh-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-cc-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-jayces-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-java-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-actionscript-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-python-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-web-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-js-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-json-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-lua-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-cs-mode.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-mode/jcs-message-mode.el")

;; NOTE(JenChieh): .cs add-to-list action above will
;;                  override the .c file c-mode add-to-list
;;                  action, so i have to override it agian
;;                  so it won't cover the setting i want...
(add-to-list 'auto-mode-alist '("\\.c?\\'" . c-mode))

;;; Override all the mode's key bindings.
(load-file "~/.emacs.d/elisp/jcs-ex/jcs-global-key.el")

;;------------------------------------------------------------------------------------------------------
;; This is the end of .emacs file
