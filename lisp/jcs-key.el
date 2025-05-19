;;; jcs-key.el --- Global Key Definition  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(jcs-key global-map
  `(
;;; Unset
    ((kbd "C-0"))
    ((kbd "C-e"))
    ((kbd "C-k"))
    ((kbd "C-p"))
    ((kbd "C-q"))
    ((kbd "C-r"))
    ((kbd "C-w"))
    ((kbd "C-x w"))
    ((kbd "M-SPC"))

;;; *Messages*
    ((kbd "M-m") . jcs-messages)
    ((kbd "M-M") . jcs-messages-other-window)

;;; Admin
    ((kbd "M-<f4>") . save-buffers-kill-terminal)
    ((kbd "M-<f6>") . restart-emacs)

;;; Balanced Expression
    ((kbd "C-:")  . jcs-backward-sexp)
    ((kbd "C-\"") . jcs-forward-sexp)
    ((kbd "C-;")  . backward-sexp)
    ((kbd "C-'")  . forward-sexp)

;;; Browser
    ((kbd "M-h") . eww)
    ((kbd "M-H") . (lambda () (interactive)
                     (save-window-excursion (call-interactively #'eww))
                     (jcs-switch-to-buffer-other-window "*eww*")))

;;; Buffer Menu
    ((kbd "M-b")     . jcs-buffer-menu)
    ((kbd "M-B")     . jcs-buffer-menu-other-window)
    ((kbd "C-M-b")   . jcs-buffer-menu-project)
    ((kbd "C-S-M-b") . jcs-buffer-menu-project-other-window)

;;; Buffers
    ((kbd "C-a") . mark-whole-buffer)
    ((kbd "M-r") . revert-buffer)

;;; Calculator
    ((kbd "C-k =") . literate-calc-eval-line)
    ((kbd "C-k +") . literate-calc-eval-buffer)

;;; Canceling Action
    ((kbd "<escape>")   . top-level)
    ((kbd "S-<escape>") . keyboard-escape-quit)

;;; Cleaning
    ((kbd "M-(") . jcs-clear-M-x-history)
    ((kbd "M-)") . clean-buffers-kill-useless-buffers)

;;; Comment / Uncomment
    ((kbd "C-/")     . smart-comment)
    ((kbd "C-k C-c") . comment-region)
    ((kbd "C-k C-u") . (lambda () (interactive)
                         ;; Not sure why this can't bind directly
                         (call-interactively #'uncomment-region)))

    ((kbd "C-k -") . banner-comment)

;;; Debugging
    ((kbd "M-1") . turbo-log-print)

    ([f9]     . jcs-debug-toggle-breakpoint)
    ([f5]     . jcs-debug-start)
    ([S-f5]   . jcs-debug-stop)
    ([C-S-f5] . jcs-debug-restart)
    ([f10]    . jcs-debug-step-over)
    ([f11]    . jcs-debug-step-in)
    ([S-f11]  . jcs-debug-step-out)

;;; Declaration / Definition
    ([f12]   . jcs-goto-definition)
    ([S-f12] . jcs-goto-definition-other-window)
    ([M-f12] . jcs-peek-definition)

;;; Editing
    ([C-right]          . vs-edit-forward-word)
    ([C-left]           . vs-edit-backward-word)
    ((kbd "<prior>")    . better-scroll-down)
    ((kbd "<next>")     . better-scroll-up)
    ((kbd "S-<prior>")  . better-scroll-down-other-window)
    ((kbd "S-<next>")   . better-scroll-up-other-window)
    ((kbd "C-S-d")      . duplicate-line)
    ((kbd "C-v")        . yank)
    ((kbd "C-s")        . jcs-save-buffer)
    ((kbd "C-S-s")      . jcs-save-all-buffers)
    ((kbd "<up>")       . previous-line)
    ((kbd "<down>")     . next-line)
    ((kbd "C-M-<up>")   . scroll-down-line)
    ((kbd "C-M-<down>") . scroll-up-line)

    ((kbd "C-M-?") . cycle-at-point)
    ((kbd "C-^")   . cycle-case-style)
    ((kbd "M-?")   . cycle-quotes)
    ((kbd "C-?")   . cycle-slash)

;;; Eval
    ((kbd "C-e b") . ueval-buffer)
    ((kbd "C-e d") . ueval-defun)
    ((kbd "C-e e") . ueval-expression)
    ((kbd "C-e r") . ueval-region)

;;; Expand Region
    ((kbd "C-M-=") . er/expand-region)
    ((kbd "C-M--") . er/contract-region)

;;; File Explorer
    ((kbd "C-M-l") . treemacs)  ; `Visual Studio'
    ((kbd "C-b")   . treemacs)  ; `VS Code'

;;; File editing
    ((kbd "M-k")   . jcs-maybe-kill-this-buffer)
    ((kbd "M-K")   . jcs-reopen-this-buffer)
    ((kbd "C-M-k") . kill-current-buffer)

;;; File Files
    ((kbd "M-f")     . ffap)
    ((kbd "M-F")     . ffap-other-window)
    ((kbd "C-k M-f") . project-find-file)
    ((kbd "C-k M-F") . jcs-project-find-file-other-window)

;;; Folding Settings
    ((kbd "C-k C-0") . vs-edit-fold-close-all)
    ((kbd "C-k C-j") . vs-edit-fold-open-all)
    ((kbd "C-{")     . vs-edit-fold-close)
    ((kbd "C-}")     . vs-edit-fold-open)

;;; Font
    ((kbd "C-k f") . menu-set-font)

;;; Format file
    ((kbd "C-k C-f") . vs-edit-indent-region)
    ((kbd "C-k C-d") . vs-edit-format-document)
    ((kbd "C-k a")   . jcs-align-region-or-document)

;;; Goto Thing
    ((kbd "M-g c") . goto-char-preview)
    ((kbd "M-g l") . goto-line-preview)
    ((kbd "M-g p") . goto-last-change)

;;; Impatient Mode
    ((kbd "C-w o") . jcs-impatient-start)
    ((kbd "C-w p") . jcs-impatient-stop)

;;; Languages
    ((kbd "C-k 0") . (lambda () (interactive) (require 'powerthesaurus)
                       (powerthesaurus-transient)))

;;; Macro
    ((kbd "C-k x") . macrostep-expand)

;;; Mark
    ((kbd "M-z") . toggle-truncate-lines)
    ("\e:"       . View-back-to-mark)
    ("\e;"       . exchange-point-and-mark)

;;; Mode Toggle
    ((kbd "C-k `") . (lambda () (interactive)
                       (zoom-window-zoom) (jcs-reload-active-mode)))
    ((kbd "C-~")   . shell-pop)
    ((kbd "C-`")   . shell-pop)
    ((kbd "C-k r") . colorful-mode)

;;; Mouse
    ([mouse-2] . mouse-set-point)

;;; Move Current Line Up or Down
    ([M-up]   . move-text-up)
    ([M-down] . move-text-down)

;;; Music
    ((kbd "M-e") . emp)
    ((kbd "M-E") . emp-other-window)

;;; Mutliple Cursors
    ((kbd "C-M-S-<up>")    . vsc-multiple-cursors-mark-previous-like-this-line)
    ((kbd "C-M-S-<down>")  . vsc-multiple-cursors-mark-next-like-this-line)
    ((kbd "C-M-_")         . vsc-multiple-cursors-mark-previous-similar-this-line)
    ((kbd "C-M-+")         . vsc-multiple-cursors-mark-next-similar-this-line)
    ((kbd "S-M-<mouse-1>") . mc/add-cursor-on-click)

;;; Navigation
    ((kbd "C-<home>") . beginning-of-buffer)
    ((kbd "C-<end>")  . end-of-buffer)
    ([home]           . vsc-edit-beginning-of-line)
    ([end]            . vsc-edit-end-of-line)

    ((kbd "M-<left>")  . jcs-backward-word-capital)
    ((kbd "M-<right>") . jcs-forward-word-capital)

;;; Find file other window
    ((kbd "<f7>")   . jcs-same-file-other-window)
    ((kbd "<f8>")   . (lambda () (interactive)
                        (if (jcs-debugging-p) (next-error)
                          (fof))))
    ((kbd "S-<f8>") . (lambda () (interactive)
                        (if (jcs-debugging-p) (previous-error)
                          (fof-other-window))))

;;; Organize Imports
    ((kbd "C-S-o")  . jcs-organize-imports)

;;; Overwrite
    ([insert] . overwrite-mode)

;;; Packages
    ((kbd "C-k C-p") . package-list-packages)
    ((kbd "C-S-x")   . package-list-packages)

;;; Process
    ((kbd "M-p") . list-processes)

;;; Profiler
    ((kbd "M-7") . toggle-profiler)

;;; Rename file
    ((kbd "M-<f2>") . jcs-rename-current-buffer-file)

;;; Return
    ((kbd "RET")        . newline-and-indent)
    ((kbd "C-<return>") . jcs-ctrl-return-key)

;;; Reveal In Folder
    ((kbd "M-R") . reveal-in-folder)

;;; Revert Buffer
    ("\er" . vs-revbuf-no-confirm)

;;; Screensaveer
    ((kbd "M-0")  . jcs-screensaver)

;;; Script Executing (Output)
    ((kbd "C-S-u")  . execrun-popup)
    ((kbd "C-S-b")  . execrun-build)  ; Build
    ((kbd "C-<f5>") . quickrun-select)
    ((kbd "C-<f7>") . quickrun-compile-only-select)

;;; Search Word
    ((kbd "C-f")     . isearch-forward)
    ((kbd "C-S-f")   . isearch-project-forward)
    ((kbd "C-r C-f") . isearch-backward-regexp)
    ((kbd "C-,")     . jcs-isearch-backward-thing-at-point)
    ((kbd "C-.")     . isearch-forward-thing-at-point)
    ((kbd "C-<")     . jcs-isearch-project-backward-thing-at-point)
    ((kbd "C->")     . isearch-project-forward-thing-at-point)

;;; Show Hover
    ((kbd "C-k C-i") . jcs-poptip)

    ((kbd "M-Q") . jcs-poptip-toggle-focus)

;;; Show Symbol
    ((kbd "M-i") . show-eol-mode)
    ((kbd "M-I") . set-buffer-file-coding-system)

    ((kbd "M-o") . show-eof-mode)

    ((kbd "C-k b")   . whitespace-mode)
    ((kbd "C-r C-w") . whitespace-mode)

;;; Startup Screen (Dashboard)
    ((kbd "M-d") . jcs-dashboard)

;;; Syntax Check
    ((kbd "<f6>") . flycheck-mode)

;;; Tab Bar
    ((kbd "C-t")         . centaur-tabs-mode)
    ((kbd "C-<prior>")   . centaur-tabs-backward)
    ((kbd "C-<next>")    . centaur-tabs-forward)
    ((kbd "C-S-<prior>") . centaur-tabs-backward-group)
    ((kbd "C-S-<next>")  . centaur-tabs-forward-group)

    ((kbd "C-<insert>")   . centaur-tabs-toggle-groups)
    ((kbd "C-S-<insert>") . centaur-tabs-switch-group)

;;; Tab Width
    ((kbd "C-k >") . indent-control-inc)
    ((kbd "C-k <") . indent-control-dec)

;;; Terminal
    ((kbd "C-M-t") . terminal-here)

;;; Theme
    ((kbd "C-k C-t") . load-theme)
    ((kbd "M-D")     . jcs-toggle-theme-light-dark)

;;; Transwin
    ("\e`" . transwin-toggle)
    ("\e=" . transwin-inc)
    ("\e-" . transwin-dec)

;;; Version Control
    ((kbd "C-k m") . jcs-magit)
    ((kbd "C-0 g") . jcs-magit)  ; Visual Studio
    ((kbd "C-S-g") . jcs-magit)  ; VSCode
    ((kbd "C-k c") . magit-branch-or-checkout)
    ((kbd "C-k d") . magit-branch-delete)

;;; Window
    ([M-f11]        . toggle-frame-fullscreen)
    ((kbd "C-S-n")  . jcs-make-frame)
    ((kbd "C-S-w")  . delete-frame)  ; delete the external frame .
    ((kbd "C-<f4>") . jcs-delete-window)
    ((kbd "C-h h")  . transpose-frame)
    ((kbd "C-w e")  . (lambda () (interactive) (require 'toggle-window)
                        (toggle-window-hide-show-window)))
    ((kbd "C-\\")   . split-window-horizontally)
    ((kbd "C-|")    . split-window-vertically)
    ((kbd "C-M-\\") . (lambda () (interactive) (split-window-sensibly)))

;;; Window Navigation
    ((kbd "C-1") . winum-select-window-1)
    ((kbd "C-2") . winum-select-window-2)
    ((kbd "C-3") . winum-select-window-3)
    ((kbd "C-4") . winum-select-window-4)
    ((kbd "C-5") . winum-select-window-5)
    ((kbd "C-6") . winum-select-window-6)
    ((kbd "C-7") . winum-select-window-7)
    ((kbd "C-8") . winum-select-window-8)
    ((kbd "C-9") . winum-select-window-9)

;;; Window Size
    ((kbd "C-M-S-j") . enlarge-window-horizontally)
    ((kbd "C-M-S-l") . shrink-window-horizontally)
    ((kbd "C-M-S-i") . enlarge-window)
    ((kbd "C-M-S-k") . shrink-window)

;;; Word Case
    ((kbd "C-M-u") . upcase-dwim)
    ((kbd "C-M-d") . downcase-dwim)
    ((kbd "C-M-c") . capitalize-dwim)

;;; Zoom
    ((kbd "C-=")      . text-scale-increase)
    ((kbd "C--")      . text-scale-decrease)

    ((kbd "C-+")      . default-text-scale-increase)
    ((kbd "C-_")      . default-text-scale-decrease)

    ((kbd "C-<kp-0>") . (lambda () (interactive)
                          (text-scale-set 0)
                          (default-text-scale-reset)))

;;; Windows Menu
    ((kbd "M-SPC n") . suspend-frame)
    ((kbd "M-SPC x") . toggle-frame-maximized)
    ((kbd "M-SPC c") . save-buffers-kill-terminal)))

(jcs-key prog-mode-map
  `(((kbd "<up>")   . vs-edit-previous-line)
    ((kbd "<down>") . vs-edit-next-line)))

(jcs-bind-key*
  `(("\eq"               . (lambda () (interactive) (other-window -1)))
    ("\ew"               . other-window)
    ([C-up]              . block-travel-up)
    ([C-down]            . block-travel-down)
    ((kbd "C-d")         . jcs-kill-whole-line)
    ((kbd "C-x")         . kill-region)
    ((kbd "C-c")         . kill-ring-save)
    ((kbd "C-M-<left>")  . buf-move-left)
    ((kbd "C-M-<right>") . buf-move-right)
    ((kbd "C-o")         . diminish-buffer-mode)  ; Diminish Buffer

    ((kbd "C-k C-s") . describe-bindings)

    ((kbd "C-r C-r") . iedit-mode)          ; Iedit
    ((kbd "C-r b")   . re-builder)          ; RE-Builder
    ((kbd "C-r o")   . read-only-mode)      ; Read-Only
    ((kbd "C-r f")   . recentf-open-files)  ; Recent Files

    ([C-S-tab] . centaur-tabs-backward)
    ([C-tab]   . centaur-tabs-forward)

;;; *scratch*
    ((kbd "M-s") . scratch-buffer)
    ((kbd "M-S") . jcs-scratch-other-window)

;;; Kill Word
    ((kbd "C-<backspace>") . vs-edit-backward-delete-word)
    ((kbd "C-<delete>")    . vs-edit-forward-delete-word)
    ((kbd "M-<backspace>") . jcs-backward-kill-word-capital)
    ((kbd "M-<delete>")    . jcs-forward-kill-word-capital)

;;; Undo / Redo
    ((kbd "C-z") . undo-tree-vf-undo)
    ((kbd "C-y") . undo-tree-vf-redo)))

;; ---

(jcs-key messages-buffer-mode-map
  `(("\ek" . jcs-messages-maybe-kill-this-buffer)
    ("\eK" . jcs-messages-erase-buffer)))

(jcs-key minibuffer-local-map
  `(((kbd "S-<return>") . newline)))

;;; Goto Address
(use-package goto-addr
  :bind ( :map goto-address-highlight-keymap
          ("C-c")))

(jcs-key package-menu-mode-map
  `(((kbd "M-K")     . package-list-packages)
    ((kbd "U")       . pkg-dm-upgrade-all)
    ((kbd "C-k r m") . pkg-dm-autoremove)))

(jcs-key special-mode-map
  `(((kbd "<up>")   . previous-line)
    ((kbd "<down>") . next-line)))

(provide 'jcs-key)
;;; jcs-key.el ends here
