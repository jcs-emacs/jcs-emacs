;;; jcs-key.el --- Global Key Definition  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(jcs-key global-map
  `(
;;; Unset
    ((kbd "C-e"))
    ((kbd "C-k"))
    ((kbd "C-p"))
    ((kbd "C-r"))
    ((kbd "C-w"))
    ((kbd "C-x w"))

;;; *Messages*
    ((kbd "M-m") . jcs-messages)
    ((kbd "M-M") . jcs-messages-other-window)

;;; *scratch*
    ((kbd "M-s") . scratch-buffer)
    ((kbd "M-S") . jcs-scratch-other-window)

;;; Admin
    ((kbd "M-<f4>") . save-buffers-kill-terminal)
    ((kbd "M-<f6>") . restart-emacs)

;;; Balanced Expression
    ((kbd "C-?")  . jcs-toggle-backward-forward-sexp)
    ((kbd "C-:")  . jcs-backward-sexp)
    ((kbd "C-\"") . jcs-forward-sexp)
    ((kbd "C-;")  . backward-sexp)
    ((kbd "C-'")  . forward-sexp)

;;; Buffer Menu
    ((kbd "M-b")     . buffer-menu)
    ((kbd "M-B")     . buffer-menu-other-window)
    ((kbd "C-M-b")   . buffer-menu-project)
    ((kbd "C-S-M-b") . buffer-menu-project-other-window)

;;; Buffers
    ((kbd "C-a") . mark-whole-buffer)
    ((kbd "M-r") . revert-buffer)

;;; Calculator
    ((kbd "C-k =") . jcs-calc-eval-region)

;;; Canceling Action
    ((kbd "<escape>")   . top-level)
    ((kbd "S-<escape>") . keyboard-escape-quit)

;;; Comment/Uncomment
    ((kbd "C-/")     . smart-comment)
    ((kbd "C-k C-c") . jcs-comment-region-or-line)
    ((kbd "C-k C-u") . jcs-uncomment-region-or-line)

;;; Debug
    ((kbd "C-S-d") . dap-mode)
    ((kbd "M-1")   . turbo-log-print)

;;; Declaration / Definition
    ([f12]   . jcs-goto-definition)
    ([S-f12] . jcs-goto-definition-other-window)
    ([M-f12] . jcs-peek-definition)

;;; Docstring
    ((kbd "M-0") . ts-docstr-ask)

;;; Editting
    ([C-right]             . jcs-smart-forward-word)
    ([C-left]              . jcs-smart-backward-word)
    ((kbd "<prior>")       . better-scroll-down)
    ((kbd "<next>")        . better-scroll-up)
    ((kbd "S-<prior>")     . better-scroll-down-other-window)
    ((kbd "S-<next>")      . better-scroll-up-other-window)
    ((kbd "C-S-d")         . jcs-duplicate-line)
    ((kbd "C-v")           . yank)
    ((kbd "C-s")           . jcs-save-buffer)
    ((kbd "C-S-s")         . jcs-save-all-buffers)
    ((kbd "<up>")          . previous-line)
    ((kbd "<down>")        . next-line)
    ((kbd "C-M-<up>")      . scroll-down-line)
    ((kbd "C-M-<down>")    . scroll-up-line)

;;; Error
    ([f9]  . first-error)
    ([f10] . previous-error)
    ([f11] . next-error)

;;; Eval
    ((kbd "C-e b") . eval-buffer)
    ((kbd "C-e d") . eval-defun)
    ((kbd "C-e e") . eval-expression)
    ((kbd "C-e r") . eval-region)

;;; Expand Region
    ((kbd "C-+") . er/expand-region)
    ((kbd "C-_") . jcs-er/contract-region)

;;; File Explorer
    ((kbd "C-M-l") . treemacs)  ; `Visual Studio'
    ((kbd "C-b")   . treemacs)  ; `VS Code'

;;; File editing
    ((kbd "M-k")   . jcs-maybe-kill-this-buffer)
    ((kbd "M-K")   . jcs-reopen-this-buffer)
    ((kbd "C-M-k") . kill-this-buffer)

;;; File Files
    ((kbd "M-f")     . ffap)
    ((kbd "M-F")     . ffap-other-window)
    ((kbd "C-k M-f") . project-find-file)
    ((kbd "C-k M-F") . jcs-project-find-file-other-window)

;;; Folding Settings
    ((kbd "C-k C-0") . jcs-close-all-nodes)
    ((kbd "C-k C-j") . jcs-open-all-nodes)
    ((kbd "C-{")     . jcs-close-node)
    ((kbd "C-}")     . jcs-open-node)

;;; Font
    ((kbd "C-k f") . menu-set-font)

;;; Format file
    ((kbd "C-k C-f") . indent-region)
    ((kbd "C-k C-d") . jcs-format-document)
    ((kbd "C-k a")   . jcs-align-region-or-document)

;;; Goto Thing
    ((kbd "M-g c") . goto-char-preview)
    ((kbd "M-g l") . goto-line-preview)

;;; Impatient Mode
    ((kbd "C-w o") . jcs-impatient-start)
    ((kbd "C-w p") . jcs-impatient-stop)

;;; Kill Word
    ((kbd "C-<backspace>") . jcs-smart-backward-delete-word)
    ((kbd "C-<delete>")    . jcs-smart-forward-delete-word)
    ((kbd "M-<backspace>") . jcs-backward-kill-word-capital)
    ((kbd "M-<delete>")    . jcs-forward-kill-word-capital)

;;; Line Endings
    ((kbd "M-i") . show-eol-mode)
    ((kbd "M-I") . set-buffer-file-coding-system)

;;; Mark
    ("\e "       . set-mark-command)
    ((kbd "M-z") . toggle-truncate-lines)
    ("\e:"       . View-back-to-mark)
    ("\e;"       . exchange-point-and-mark)

;;; Minimap
    ((kbd "C-k m") . minimap-mode)

;;; Mode Toggle
    ((kbd "C-k `") . (lambda () (interactive)
                       (zoom-window-zoom) (jcs-reload-active-mode)))
    ((kbd "C-~")   . shell-pop)
    ((kbd "C-`")   . shell-pop)
    ((kbd "C-k r") . rainbow-mode)

;;; Mouse
    ([mouse-2] . mouse-set-point)

;;; Move Current Line Up or Down
    ([M-up]   . move-text-up)
    ([M-down] . move-text-down)

;;; Mutliple Cursors
    ((kbd "C-M-S-<up>")     . vsc-multiple-cursors-mark-previous-like-this-line)
    ((kbd "C-M-S-<down>")   . vsc-multiple-cursors-mark-next-like-this-line)
    ((kbd "C-M-_")          . vsc-multiple-cursors-mark-previous-similar-this-line)
    ((kbd "C-M-+")          . vsc-multiple-cursors-mark-next-similar-this-line)
    ((kbd "C-M-=")          . vsc-multiple-cursors-inc-string-distance-level)
    ((kbd "C-M--")          . vsc-multiple-cursors-dec-string-distance-level)
    ((kbd "S-M-<mouse-1>")  . mc/add-cursor-on-click)

;;; Navigation
    ((kbd "C-<home>") . beginning-of-buffer)
    ((kbd "C-<end>")  . end-of-buffer)
    ([home]           . jcs-beginning-of-line)
    ([end]            . jcs-end-of-line)

;;; Find file other window
    ((kbd "<f7>")   . jcs-same-file-other-window)
    ((kbd "<f8>")   . fof)
    ((kbd "S-<f8>") . fof-other-window)

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

;;; Right Click Context
    ([S-f10] . right-click-context-menu)

;;; Script Executing (Output)
    ((kbd "C-S-u")  . execrun-popup)
    ((kbd "M-o")    . execrun-switch-to-buffer)
    ((kbd "<f5>")   . execrun-run)    ; Run
    ((kbd "C-S-b")  . execrun-build)  ; Build
    ((kbd "C-<f5>") . quickrun-select)
    ((kbd "C-<f7>") . quickrun-compile-only-select)

;;; Search Word
    ((kbd "C-f")   . isearch-forward)
    ((kbd "C-S-f") . isearch-project-forward)
    ;; NOTE: Basic search is bind to `jcs-cross-mode' and `jcs-depend-mode'.
    ;; See `jcs-mode.el' file for the settings.
    ((kbd "C-r C-f") . isearch-backward-regexp)
    ((kbd "C-,")     . jcs-isearch-backward-symbol-at-point)
    ((kbd "C-.")     . isearch-forward-symbol-at-point)
    ((kbd "C-<")     . jcs-isearch-project-backward-symbol-at-point)
    ((kbd "C->")     . isearch-project-forward-symbol-at-point)

;;; Show Hover
    ((kbd "C-k C-i") . jcs-describe-thing-in-popup)

;;; Sort
    ((kbd "C-i") . sort-words)

;;; Startup Screen (Dashboard)
    ((kbd "M-d") . jcs-dashboard)
    ((kbd "M-D") . jcs-dashboard-other-window)

;;; Syntax Check
    ((kbd "<f6>") . jcs-flycheck-mode)

;;; Tab Bar
    ((kbd "C-t")       . jcs-toggle-tabbar-mode)
    ((kbd "C-<prior>") . centaur-tabs-backward)
    ((kbd "C-<next>")  . centaur-tabs-forward)

;;; Tab Width
    ((kbd "C-k >") . indent-control-inc)
    ((kbd "C-k <") . indent-control-dec)

;;; Terminal
    ((kbd "C-M-t") . terminal-here)

;;; Theme
    ((kbd "C-k C-t") . load-theme)

;;; Transwin
    ("\e`" . transwin-toggle)
    ("\e=" . transwin-inc)
    ("\e-" . transwin-dec)

;;; Window
    ([M-f11]        . toggle-frame-fullscreen)
    ((kbd "C-S-n")  . jcs-make-frame)
    ((kbd "C-S-w")  . delete-frame)  ; delete the external frame.
    ((kbd "C-<f4>") . delete-window)
    ((kbd "C-h h")  . transpose-frame)
    ((kbd "C-w e")  . (lambda () (interactive) (require 'toggle-window)
                        (toggle-window-hide-show-window)))
    ((kbd "C-\\")   . split-window-horizontally)
    ((kbd "C-|")    . split-window-vertically)

;;; Window Navigation
    ((kbd "M-e") . ace-window)
    ((kbd "C-1") . winum-select-window-1)
    ((kbd "C-2") . winum-select-window-2)
    ((kbd "C-3") . winum-select-window-3)
    ((kbd "C-4") . winum-select-window-4)
    ((kbd "C-5") . winum-select-window-5)
    ((kbd "C-6") . winum-select-window-6)
    ((kbd "C-7") . winum-select-window-7)
    ((kbd "C-8") . winum-select-window-8)
    ((kbd "C-9") . winum-select-window-9)

;;; Word Case
    ((kbd "C-M-u") . upcase-dwim)
    ((kbd "C-M-d") . downcase-dwim)
    ((kbd "C-M-c") . capitalize-dwim)

;;; Whitespace
    ((kbd "C-k b") . whitespace-mode)

;;; Zoom
    ((kbd "C-=")      . text-scale-increase)
    ((kbd "C--")      . text-scale-decrease)
    ((kbd "C-<kp-0>") . (lambda () (interactive) (text-scale-set 0)))))

(jcs-key prog-mode-map
  `(((kbd "<up>")        . ,(jcs-get-prev/next-key-type 'previous))
    ((kbd "<down>")      . ,(jcs-get-prev/next-key-type 'next))))

(jcs-leaf-key*
  `(("\eq"               . (lambda () (interactive) (other-window -1)))
    ("\ew"               . other-window)
    ([C-up]              . jcs-previous-blank-line)
    ([C-down]            . jcs-next-blank-line)
    ((kbd "C-d")         . jcs-kill-whole-line)
    ((kbd "C-x")         . kill-region)
    ((kbd "C-c")         . kill-ring-save)
    ((kbd "C-M-<left>")  . buf-move-left)
    ((kbd "C-M-<right>") . buf-move-right)
    ((kbd "M-<left>")    . jcs-backward-word-capital)
    ((kbd "M-<right>")   . jcs-forward-word-capital)
    ((kbd "C-o")         . diminish-buffer-mode)  ; Diminish Buffer

    ((kbd "M-s") . scratch-buffer)

    ((kbd "C-k C-s") . describe-bindings)

    ((kbd "C-r C-r") . jcs-iedit-mode)        ; Iedit
    ((kbd "C-r b")   . jcs-re-builder)        ; RE-Builder
    ((kbd "C-r o")   . read-only-mode)        ; Read-Only
    ((kbd "C-r f")   . recentf-open-files)    ; Recent Files

    ([C-S-tab] . centaur-tabs-backward)
    ([C-tab]   . centaur-tabs-forward)

;;; Undo/Redo
    ((kbd "C-z") . undo-tree-vf-undo)
    ((kbd "C-y") . undo-tree-vf-redo)))

;; ---

(leaf auto-highlight-symbol
  :defer-config
  (jcs-key auto-highlight-symbol-mode-map
    `(((kbd "M-S-<right>"))
      ((kbd "M-S-<left>"))
      ((kbd "M--"))
      ((kbd "M-<left>"))
      ((kbd "M-<right>")))))

(jcs-key messages-buffer-mode-map
  `(("\ek" . jcs-messages-maybe-kill-this-buffer)
    ("\eK" . jcs-messages-erase-buffer)))

;;; Auto Completion
(leaf company
  :defer-config
  (jcs-key company-active-map
    `(([tab]       . vsc-edit-tab)
      ((kbd "TAB") . vsc-edit-tab)
      ((kbd "C-s") . jcs-save-buffer))))

;;; Binary/Hex Editor
(leaf nhexl-mode
  :defer-config
  (jcs-key nhexl-mode-map
    `(((kbd "<up>")    . previous-line)
      ((kbd "<down>")  . next-line)
      ((kbd "<right>") . forward-char)
      ((kbd "<left>")  . backward-char))))

(leaf scrollable-quick-peek
  :defer-config
  (jcs-key scrollable-quick-peek-keymap
    `(((kbd "<down>"))
      ((kbd "<up>"))
      ((kbd "S-<down>") . scrollable-quick-peek-scroll-down)
      ((kbd "S-<up>")   . scrollable-quick-peek-scroll-up))))

;;; Goto Address
(leaf goto-addr
  :defer-config
  (jcs-key goto-address-highlight-keymap
    `(((kbd "C-c")))))

;;; Kill Ring
(leaf browse-kill-ring
  :defer-config
  (jcs-key browse-kill-ring-mode-map
    `(((kbd "<escape>") . kill-this-buffer))))

(leaf multiple-cursors
  :defer-config
  (jcs-key mc/keymap
    `(((kbd "<escape>") . mc/keyboard-quit)
      ((kbd "<return>"))
      ((kbd "C-v")      . vsc-edit-yank)
      ((kbd "C-:"))
      ((kbd "C-'")))))

(jcs-key package-menu-mode-map
  `(((kbd "M-K")     . package-list-packages)
    ((kbd "U")       . jcs-package-upgrade-all)
    ((kbd "C-k r m") . jcs-package-autoremove)))

(jcs-key isearch-mode-map
  `(((kbd "C-s"))
    ((kbd "C-r"))
    ((kbd "C-,") . jcs-isearch-repeat-backward)
    ((kbd "C-.") . jcs-isearch-repeat-forward)
    ((kbd "C-<") . jcs-isearch-project-repeat-backward)
    ((kbd "C->") . jcs-isearch-project-repeat-forward)
    ;; TODO: Implements isearch cursor for these two keys
    ;;((kbd "C-x") . kill-region)
    ;;((kbd "C-c") . kill-ring-save)
    ((kbd "C-v") . isearch-yank-pop)))

(jcs-key special-mode-map
  `(((kbd "<up>")   . previous-line)
    ((kbd "<down>") . next-line)))

(leaf dashboard
  :defer-config
  (jcs-key dashboard-mode-map
    `(((kbd "SPC"))
      ((kbd "<up>")    . previous-line)
      ((kbd "<down>")  . next-line)
      ((kbd "C-k C-p") . package-list-packages)
      ((kbd "M-K")     . jcs-dashboard-refresh-buffer))))

(leaf flycheck
  :defer-config
  (jcs-key flycheck-error-list-mode-map
    `(((kbd "M-k") . jcs-flycheck-mode)
      ((kbd "M-K") . flycheck-error-list-reset-filter))))

(jcs-key tabulated-list-mode-map
  `(((kbd "C-+") . tabulated-list-widen-current-column)
    ((kbd "C-_") . tabulated-list-narrow-current-column)))

(leaf hl-todo
  :defer-config
  (jcs-key hl-todo-mode-map
    `(([C-f10] . hl-todo-previous)
      ([C-f11] . hl-todo-next))))

(leaf suggest
  :defer-config
  (jcs-key suggest-mode-map
    `(((kbd "U")   . suggest-update)
      ((kbd "M-K") . suggest-update))))

(leaf undo-tree
  :defer-config
  (jcs-key undo-tree-visualizer-mode-map
    `(((kbd "RET") . undo-tree-visualizer-quit)
      ((kbd "C-s") . undo-tree-visualizer-quit)))
  (jcs-key undo-tree-map
    `(((kbd "C-/") . smart-comment)
      ((kbd "C-/"))
      ("\C-_")
      ((kbd "C-?"))
      ((kbd "M-_")))))

(leaf vertico
  :defer-config
  (jcs-key vertico-map
    `(((kbd "\177")     . vertico-directory-delete-char)
      ((kbd "<return>") . vertico-directory-enter)
      ((kbd "/")        . jcs-vertico-/))))

(leaf with-editor
  :defer-config
  (jcs-key with-editor-mode-map
    `(((kbd "C-s")      . with-editor-finish)
      ((kbd "C-g")      . with-editor-cancel)
      ((kbd "<escape>") . with-editor-cancel))))

(provide 'jcs-key)
;;; jcs-key.el ends here
