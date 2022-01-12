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
    ((kbd "M-m") . jcs-message-buffer)
    ((kbd "M-M") . jcs-message-buffer-other-window)

;;; *scratch*
    ((kbd "M-s") . jcs-scratch-buffer)
    ((kbd "M-S") . jcs-scratch-buffer-other-window)

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
    ((kbd "C-M-b")   . jcs-buffer-menu-project)
    ((kbd "C-S-M-b") . jcs-buffer-menu-project-other-window)

;;; Buffers
    ((kbd "C-a") . jcs-mark-whole-buffer)
    ((kbd "M-r") . revert-buffer)

;;; Calculator
    ((kbd "C-k =") . jcs-calc-eval-region)

;;; Canceling Action
    ;;((kbd "C-g")    . top-level)
    ((kbd "<escape>") . top-level)

;;; Comment/Uncomment
    ((kbd "C-/")     . jcs-comment-uncomment-region-or-line)
    ((kbd "C-k C-c") . jcs-comment-region-or-line)
    ((kbd "C-k C-u") . jcs-uncomment-region-or-line)

;;; Debug
    ((kbd "C-S-d") . dap-mode)
    ((kbd "M-1")   . turbo-log)

;;; Declaration / Definition
    ([f12]   . jcs-goto-definition)
    ([S-f12] . jcs-goto-definition-other-window)
    ([M-f12] . jcs-peek-definition)

;;; Describe Things
    ((kbd "C-k C-s")       . describe-bindings)

;;; Editting
    ([C-right]             . jcs-smart-forward-word)
    ([C-left]              . jcs-smart-backward-word)
    ((kbd "<prior>")       . better-scroll-down)
    ((kbd "<next>")        . better-scroll-up)
    ((kbd "S-<prior>")     . better-scroll-down-other-window)
    ((kbd "S-<next>")      . better-scroll-up-other-window)
    ((kbd "<backspace>")   . jcs-real-backspace)
    ((kbd "S-<backspace>") . jcs-real-backspace)
    ((kbd "<delete>")      . jcs-real-delete)
    ((kbd "S-<delete>")    . jcs-real-delete)
    ((kbd "SPC")           . jcs-real-space)
    ((kbd "S-SPC")         . jcs-real-space)
    ((kbd "C-S-d")         . jcs-duplicate-line)
    ((kbd "C-v")           . yank)
    ((kbd "C-s")           . jcs-save-buffer-by-mode)
    ((kbd "C-S-s")         . jcs-save-all-buffers)
    ((kbd "C-k s")         . jcs-reverse-tab-untab-save-buffer)
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
    ((kbd "M-k") . jcs-maybe-kill-this-buffer)
    ((kbd "M-K") . jcs-reopen-this-buffer)
    ([tab]       . jcs-tab-key)
    ([S-tab]     . jcs-shift-tab-key)
    ([backtab]   . jcs-shift-tab-key)

;;; File Files
    ((kbd "M-f")     . counsel-find-file)
    ((kbd "M-F")     . jcs-counsel-find-files-other-window)
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

;;; Ivy / Counsel / Swiper
    ((kbd "M-x")   . counsel-M-x)
    ((kbd "C-S-p") . counsel-M-x)  ; Compatible to VSCode
    ((kbd "<f1>")  . counsel-M-x)
    ((kbd "M-y")   . counsel-yank-pop)

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
    ((kbd "C-k m") . jcs-toggle-minimap)

;;; Mode Toggle
    ((kbd "C-k `") . jcs-depend-cross-mode-toggle)
    ((kbd "C-~")   . jcs-shell-new-shell)
    ((kbd "C-`")   . jcs-toggle-shell-window)
    ((kbd "C-k r") . rainbow-mode)

;;; Mouse
    ([mouse-2] . mouse-set-point)

;;; Move Current Line Up or Down
    ([M-up]   . move-text-up)
    ([M-down] . move-text-down)

;;; Mutliple Cursors
    ((kbd "C-M-S-<up>")   . jcs-mc/mark-previous-like-this-line)
    ((kbd "C-M-S-<down>") . jcs-mc/mark-next-like-this-line)
    ((kbd "C-M-_")        . jcs-mc/mark-previous-similar-this-line)
    ((kbd "C-M-+")        . jcs-mc/mark-next-similar-this-line)
    ((kbd "C-M-=")        . jcs-mc/inc-string-distance-level)
    ((kbd "C-M--")        . jcs-mc/dec-string-distance-level)

;;; Navigation
    ((kbd "C-<home>") . beginning-of-buffer)
    ((kbd "C-<end>")  . end-of-buffer)
    ([home]           . jcs-beginning-of-line)
    ([end]            . jcs-end-of-line)

;;; Open same file in other window
    ((kbd "<f7>") . jcs-same-file-other-window)
    ((kbd "<f8>") . jcs-same-file-other-window)  ; Replace by corresponding file if any

;;; Overwrite
    ([insert] . overwrite-mode)

;;; Packages
    ((kbd "C-k C-p") . package-list-packages)
    ((kbd "C-S-x")   . package-list-packages)

;;; Process
    ((kbd "M-p") . list-processes)

;;; Rename file
    ((kbd "M-<f2>") . jcs-rename-current-buffer-file)

;;; Return
    ((kbd "RET")        . newline-and-indent)
    ((kbd "C-<return>") . jcs-ctrl-return-key)

;;; Reveal In Folder
    ((kbd "M-R") . reveal-in-folder)

;;; Revert Buffer
    ("\er" . jcs-revert-buffer-no-confirm)

;;; Right Click Context
    ([S-f10] . right-click-context-menu)

;;; Script Executing (Output)
    ((kbd "C-S-u") . jcs-output-window)
    ((kbd "M-o")   . jcs-dev-switch-to-output-buffer)
    ((kbd "<f5>")  . jcs-run-without-asking)   ; Run
    ((kbd "C-S-b") . jcs-make-without-asking)  ; Build

;;; Search Word
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
    ((kbd "C-k >") . indent-control-inc-indent-level)
    ((kbd "C-k <") . indent-control-dec-indent-level)

;;; Transwin
    ("\e`" . transwin-toggle-transparent-frame)
    ("\e=" . transwin-increment-frame-transparent)
    ("\e-" . transwin-decrement-frame-transparent)

;;; Window
    ([M-f11]        . toggle-frame-fullscreen)
    ((kbd "C-S-n")  . jcs-make-frame)
    ((kbd "C-S-w")  . delete-frame)  ; delete the external frame.
    ((kbd "C-<f4>") . delete-window)
    ((kbd "C-h h")  . jcs-toggle-window-split-hv)
    ((kbd "C-w e")  . jcs-toggle-enlarge-window-selected)
    ((kbd "C-\\")   . split-window-horizontally)
    ((kbd "C-|")    . split-window-vertically)

;;; Window Navigation
    ("\ew"       . jcs-other-window-next)
    ("\eq"       . jcs-other-window-prev)
    ((kbd "M-e") . ace-window)
    ((kbd "C-1") . jcs-ace-window-1)
    ((kbd "C-2") . jcs-ace-window-2)
    ((kbd "C-3") . jcs-ace-window-3)
    ((kbd "C-4") . jcs-ace-window-4)
    ((kbd "C-5") . jcs-ace-window-5)
    ((kbd "C-6") . jcs-ace-window-6)
    ((kbd "C-7") . jcs-ace-window-7)
    ((kbd "C-8") . jcs-ace-window-8)
    ((kbd "C-9") . jcs-ace-window-9)

;;; Word Case
    ((kbd "C-M-u") . jcs-upcase-word-or-region)
    ((kbd "C-M-d") . jcs-downcase-word-or-region)
    ((kbd "C-M-c") . jcs-capitalize-word-or-region)

;;; Undo/Redo
    ((kbd "C-z") . jcs-undo)
    ((kbd "C-y") . jcs-redo)

;;; Whitespace
    ((kbd "C-k b") . whitespace-mode)

;;; Zoom
    ((kbd "C-=")      . jcs-text-scale-increase)
    ((kbd "C--")      . jcs-text-scale-decrease)
    ((kbd "C-<kp-0>") . jcs-reset-zoom)))

(jcs-key prog-mode-map
  `(((kbd "<backspace>") . jcs-smart-backspace)
    ((kbd "<delete>")    . jcs-smart-delete)
    ((kbd "SPC")         . jcs-smart-space)
    ((kbd "C-v")         . jcs-smart-yank)
    ((kbd "<up>")        . ,(jcs-get-prev/next-key-type 'previous))
    ((kbd "<down>")      . ,(jcs-get-prev/next-key-type 'next))))

(jcs-leaf-key*
  `(([C-up]              . jcs-previous-blank-line)
    ([C-down]            . jcs-next-blank-line)
    ((kbd "C-d")         . jcs-kill-whole-line)
    ((kbd "C-x")         . jcs-vs-cut-key)
    ((kbd "C-c")         . kill-ring-save)
    ((kbd "C-M-<left>")  . buf-move-left)
    ((kbd "C-M-<right>") . buf-move-right)
    ((kbd "M-<left>")    . jcs-backward-word-capital)
    ((kbd "M-<right>")   . jcs-forward-word-capital)
    ((kbd "C-o")         . diminish-buffer-mode)  ; Diminish Buffer
    ((kbd "C-r C-r")     . jcs-iedit-mode)        ; Iedit
    ((kbd "C-r b")       . jcs-re-builder)        ; RE-Builder
    ((kbd "C-r o")       . read-only-mode)        ; Read-Only
    ((kbd "C-r f")       . recentf-open-files)    ; Recent Files

    ([C-S-tab] . centaur-tabs-backward)
    ([C-tab]   . centaur-tabs-forward)))

;; ---

(with-eval-after-load 'auto-highlight-symbol
  (jcs-key auto-highlight-symbol-mode-map
    `(((kbd "M-S-<right>"))
      ((kbd "M-S-<left>"))
      ((kbd "M--"))
      ((kbd "M-<left>"))
      ((kbd "M-<right>")))))

(jcs-key messages-buffer-mode-map
  `(("\ek" . jcs-message-erase-buffer)
    ("\eK" . jcs-message-erase-buffer-stay)))

;;; Auto Completion
(with-eval-after-load 'company
  (jcs-key company-active-map
    `(([tab]       . jcs-tab-key)
      ((kbd "TAB") . jcs-tab-key)
      ((kbd "C-s") . jcs-save-buffer-by-mode))))

;;; Buffer Menu
(with-eval-after-load 'jcs-buffer-menu
  (jcs-key Buffer-menu-mode-map
    `(((kbd "C-k"))
      ((kbd "M-K")     . buffer-menu)
      ((kbd "C-k C-s") . describe-bindings)
      ((kbd "M-s")     . jcs-scratch-buffer)
      ;; Sort
      ((kbd "M-1")     . jcs-buffer-menu-sort-by-visit)
      ((kbd "M-2")     . jcs-buffer-menu-sort-by-buffer)
      ((kbd "M-3")     . jcs-buffer-menu-sort-by-size)
      ((kbd "M-4")     . jcs-buffer-menu-sort-by-time)
      ((kbd "M-5")     . jcs-buffer-menu-sort-by-mode)
      ((kbd "M-6")     . jcs-buffer-menu-sort-by-file)
      ;; Searching / Filtering
      ((kbd "<escape>") . (lambda () (interactive) (buffer-menu) (top-level)))
      ((kbd "<return>") . jcs-buffer-menu-return)))

  (dolist (key-str jcs-key-list)
    (define-key Buffer-menu-mode-map key-str
      (lambda () (interactive) (jcs--buffer-menu-input key-str))))

  (define-key Buffer-menu-mode-map (kbd "<backspace>")
    (lambda () (interactive) (jcs--buffer-menu-input "" -1))))

;;; Binary/Hex Editor
(with-eval-after-load 'nhexl-mode
  (jcs-key nhexl-mode-map
    `(((kbd "<up>")    . previous-line)
      ((kbd "<down>")  . next-line)
      ((kbd "<right>") . forward-char)
      ((kbd "<left>")  . backward-char))))

(with-eval-after-load 'scrollable-quick-peek
  (jcs-key scrollable-quick-peek-keymap
    `(((kbd "<down>"))
      ((kbd "<up>"))
      ((kbd "S-<down>") . scrollable-quick-peek-scroll-down)
      ((kbd "S-<up>")   . scrollable-quick-peek-scroll-up))))

;;; ESUP
(with-eval-after-load 'esup
  (jcs-key esup-mode-map
    `(((kbd "C-z") . undo-tree-undo)
      ((kbd "C-y") . undo-tree-redo))))

;;; Goto Address
(with-eval-after-load 'goto-addr
  (jcs-key goto-address-highlight-keymap
    `(((kbd "C-c")))))

;;; Help
(with-eval-after-load 'help-mode
  (jcs-key help-mode-map
    `(((kbd "C-c") . kill-ring-save))))

(with-eval-after-load 'counsel
  (jcs-key counsel-find-file-map
    `(((kbd "<backspace>") . jcs-counsel-find-files-backspace)
      ((kbd "<return>")    . jcs-counsel-find-files-enter)
      ((kbd "/")           . jcs-counsel-find-files--slash))))

;;; Kill Ring
(with-eval-after-load 'browse-kill-ring
  (jcs-key browse-kill-ring-mode-map
    `(((kbd "<escape>") . kill-this-buffer))))

(with-eval-after-load 'multiple-cursors
  (jcs-key mc/keymap
    `(((kbd "<escape>") . mc/keyboard-quit)
      ((kbd "<return>"))
      ((kbd "C-v")      . jcs-smart-yank)
      ((kbd "C-:"))
      ((kbd "C-'")))))

;;; Packages
(jcs-key package-menu-mode-map
  `(((kbd "s")       . jcs-package-menu-filter-by-status)
    ((kbd "U")       . jcs-package-upgrade-all)
    ((kbd "C-k r m") . jcs-package-autoremove)))

(jcs-key isearch-mode-map
  `(((kbd "C-s"))
    ((kbd "C-r"))
    ((kbd "C-,") . jcs-isearch-repeat-backward)
    ((kbd "C-.") . jcs-isearch-repeat-forward)
    ((kbd "C-<") . jcs-isearch-project-repeat-backward)
    ((kbd "C->") . jcs-isearch-project-repeat-forward)
    ;; TODO: Implements isearch cursor for these two keys.
    ;;((kbd "C-x") . jcs-vs-cut-key)
    ;;((kbd "C-c") . kill-ring-save)
    ((kbd "C-v") . isearch-yank-pop)))

;;; Special
(jcs-key special-mode-map
  `(((kbd "<up>")   . previous-line)
    ((kbd "<down>") . next-line)))

(with-eval-after-load 'dashboard
  (jcs-key dashboard-mode-map
    `(((kbd "SPC"))
      ((kbd "<backspace>") . jcs-dashboard-remove-current-item)
      ((kbd "<delete>")    . jcs-dashboard-remove-current-item)
      ((kbd "d")           . jcs-dashboard-remove-current-item)
      ((kbd "g")           . jcs-dashboard-refresh-buffer)
      ("1"                 . jcs-dashboard-item-section-1)
      ("2"                 . jcs-dashboard-item-section-2)
      ("3"                 . jcs-dashboard-item-section-3)
      ("4"                 . jcs-dashboard-item-section-4)
      ("5"                 . jcs-dashboard-item-section-5)
      ("6"                 . jcs-dashboard-item-section-6)
      ("7"                 . jcs-dashboard-item-section-7)
      ("8"                 . jcs-dashboard-item-section-8)
      ("9"                 . jcs-dashboard-item-section-9)
      ((kbd "<up>")        . previous-line)
      ((kbd "<down>")      . next-line)
      ((kbd "C-<up>")      . jcs-dashboard-previous-blank-line)
      ((kbd "C-<down>")    . jcs-dashboard-next-blank-line)
      ((kbd "C-k C-p")     . package-list-packages)
      ((kbd "M-K")         . jcs-dashboard-refresh-buffer))))

(with-eval-after-load 'flycheck
  (jcs-key flycheck-error-list-mode-map
    `(((kbd "M-k") . jcs-flycheck-mode)
      ((kbd "M-K") . flycheck-error-list-reset-filter))))

;;; Tabulated-List
(jcs-key tabulated-list-mode-map
  `(((kbd "C-+") . tabulated-list-widen-current-column)
    ((kbd "C-_") . tabulated-list-narrow-current-column)))

;;; Todo
(with-eval-after-load 'hl-todo
  (jcs-key hl-todo-mode-map
    `(([C-f10] . hl-todo-previous)
      ([C-f11] . hl-todo-next))))

;;; Undo Tree
(with-eval-after-load 'undo-tree
  (jcs-key undo-tree-visualizer-mode-map
    `(((kbd "RET") . undo-tree-visualizer-quit)))
  ;; STUDY: `undo-tree''s minor mode will overwrite the global key map's
  ;; key bindings. What we need to do is to remap this again...
  (jcs-key undo-tree-map
    `(((kbd "C-/") . jcs-comment-uncomment-region-or-line)
      ((kbd "C-/"))
      ("\C-_")
      ((kbd "C-?"))
      ((kbd "M-_")))))

;;; With Editor
(with-eval-after-load 'with-editor
  (jcs-key with-editor-mode-map
    `(((kbd "C-s")      . with-editor-finish)
      ((kbd "C-g")      . with-editor-cancel)
      ((kbd "<escape>") . with-editor-cancel))))

(provide 'jcs-key)
;;; jcs-key.el ends here
