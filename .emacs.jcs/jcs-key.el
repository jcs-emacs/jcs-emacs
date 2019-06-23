;;; jcs-key.el --- Global Key Definition.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;---------------------------------------------
;;; Unset key binding
;;;---------------------------------------------
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-w"))

(with-eval-after-load 'auto-highlight-symbol
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") 'nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") 'nil))

(define-key Buffer-menu-mode-map (kbd "C-k") nil)

(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-r") nil)


;;;---------------------------------------------
;;; Set key binding
;;;---------------------------------------------

;;; Admin
(define-key global-map (kbd "C-x C-v") #'reload-emacs)
(define-key global-map (kbd "C-x C-b") #'restart-emacs)

;;; ag
(with-eval-after-load 'ag
  (define-key ag-mode-map (kbd "M-K") #'jcs-ag-refresh-search))

;;; Auto Completion
(with-eval-after-load 'company
  (define-key company-active-map [tab] #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "C-s") #'jcs-untabify-save-buffer))

;;; Buffer Menu
(define-key global-map (kbd "M-b") #'buffer-menu)
(define-key global-map (kbd "M-B") #'buffer-menu-other-window)

(define-key Buffer-menu-mode-map "\eK" #'buffer-menu)
(define-key Buffer-menu-mode-map "1" #'jcs-buffer-menu-sort-by-visit)
(define-key Buffer-menu-mode-map "2" #'jcs-buffer-menu-sort-by-buffer)
(define-key Buffer-menu-mode-map "3" #'jcs-buffer-menu-sort-by-size)
(define-key Buffer-menu-mode-map "4" #'jcs-buffer-menu-sort-by-time)
(define-key Buffer-menu-mode-map "5" #'jcs-buffer-menu-sort-by-mode)
(define-key Buffer-menu-mode-map "6" #'jcs-buffer-menu-sort-by-file)
(define-key Buffer-menu-mode-map (kbd "C-k C-s") #'describe-bindings)

;;; Buffers
(define-key global-map "\C-a" #'jcs-mark-whole-buffer)
(define-key global-map "\er" #'revert-buffer)
(define-key global-map "\es" #'save-buffer)

(define-key global-map [S-tab] #'indent-for-tab-command)
(define-key global-map [backtab] #'indent-for-tab-command)
(define-key global-map (kbd "C-y") #'indent-for-tab-command)
(define-key global-map [C-tab] #'indent-region)

;;; Binary/Hex Editor
(with-eval-after-load 'nhexl-mode
  (define-key nhexl-mode-map (kbd "<up>") #'previous-line)
  (define-key nhexl-mode-map (kbd "<down>") #'next-line)
  (define-key nhexl-mode-map (kbd "<right>") #'forward-char)
  (define-key nhexl-mode-map (kbd "<left>") #'backward-char))

;;; Calculator
(define-key global-map (kbd "C-x =") #'jcs-calc-eval-region)

;;; Canceling Action.
;;(define-key global-map (kbd "C-g") #'top-level)
(define-key global-map (kbd "<escape>") #'top-level)

;;; Comment/Uncomment
(define-key global-map (kbd "C-/") #'jcs-comment-uncomment-region-or-line)
(define-key global-map (kbd "C-k C-c") #'jcs-comment-region-or-line)
(define-key global-map (kbd "C-k C-u") #'jcs-uncomment-region-or-line)

;;; Describe Thing
(define-key global-map (kbd "C-k C-s") #'describe-bindings)

;;; Editting
(define-key global-map [C-right] #'forward-word)
(define-key global-map [C-left] #'backward-word)
(define-key global-map [C-up] #'jcs-previous-blank-line)
(define-key global-map [C-down] #'jcs-next-blank-line)
(define-key global-map [pgup] #'forward-page)
(define-key global-map [pgdown] #'backward-page)
(define-key global-map [C-next] #'scroll-other-window)
(define-key global-map [C-prior] #'scroll-other-window-down)

(define-key global-map (kbd "C-c d") #'jcs-duplicate-line)
(define-key global-map (kbd "C-d") #'jcs-kill-whole-line)
(define-key global-map (kbd "C-x C-x") #'jcs-vs-cut-key)
(define-key global-map (kbd "C-c C-c") #'kill-ring-save)
(define-key global-map (kbd "C-v") #'yank)
(define-key global-map (kbd "C-s") #'jcs-untabify-save-buffer)
(define-key global-map (kbd "C-S-s") #'jcs-tabify-save-buffer)

(define-key global-map (kbd "<up>") #'jcs-smart-indent-up)
(define-key global-map (kbd "<down>") #'jcs-smart-indent-down)

(define-key global-map (kbd "C-M-<up>") #'jcs-scroll-down-one-line)
(define-key global-map (kbd "C-M-<down>") #'jcs-scroll-up-one-line)

(define-key global-map (kbd "C-M-<left>") #'buf-move-left)
(define-key global-map (kbd "C-M-<right>") #'buf-move-right)

(progn ;; Navigating General Programming Symbols
  (define-key global-map (kbd "M-)") (quote jcs-move-forward-close-paren))
  (define-key global-map (kbd "M-(") (quote jcs-move-backward-open-paren))
  (define-key global-map (kbd "M-]") (quote jcs-move-forward-close-sqrParen))
  (define-key global-map (kbd "M-[") (quote jcs-move-backward-open-sqrParen))
  (define-key global-map (kbd "M-}") (quote jcs-move-forward-close-curlyParen))
  (define-key global-map (kbd "M-{") (quote jcs-move-backward-open-curlyParen))
  (define-key global-map (kbd "M-'") (quote jcs-move-forward-single-quot))
  (define-key global-map (kbd "M-;") (quote jcs-move-backward-single-quot))
  (define-key global-map (kbd "M-\"") (quote jcs-move-forward-double-quot))
  (define-key global-map (kbd "M-:") (quote jcs-move-backward-double-quot))
  (define-key global-map (kbd "M->") (quote jcs-move-forward-greater-than-sign))
  (define-key global-map (kbd "M-<") (quote jcs-move-backward-less-than-sign))

  (define-key global-map (kbd "M-.") (quote jcs-move-forward-comma))
  (define-key global-map (kbd "M-,") (quote jcs-move-backward-comma))
  (define-key global-map (kbd "C-M-.") (quote jcs-move-forward-period))
  (define-key global-map (kbd "C-M-,") (quote jcs-move-backward-period)))


(progn ;; Changing/Deleting inside between Programming Symbols
  (define-key global-map (kbd "C-c i [") #'jcs-delete-inside-sqrParen)
  (define-key global-map (kbd "C-c i ]") #'jcs-delete-inside-sqrParen)
  (define-key global-map (kbd "C-c i (") #'jcs-delete-inside-paren)
  (define-key global-map (kbd "C-c i )") #'jcs-delete-inside-paren)
  (define-key global-map (kbd "C-c i {") #'jcs-delete-inside-curlyParen)
  (define-key global-map (kbd "C-c i }") #'jcs-delete-inside-curlyParen)
  (define-key global-map (kbd "C-c i '") #'jcs-delete-inside-single-quot)
  (define-key global-map (kbd "C-c i \"") #'jcs-delete-inside-double-quot)
  (define-key global-map (kbd "C-c i <") #'jcs-delete-inside-greater-less-sign)
  (define-key global-map (kbd "C-c i >") #'jcs-delete-inside-less-greater-sign)

  (define-key global-map (kbd "C-c i `") #'jcs-delete-inside-back-quot)
  (define-key global-map (kbd "C-c i ~") #'jcs-delete-inside-tilde)
  (define-key global-map (kbd "C-c i !") #'jcs-delete-inside-exclamation-mark)
  (define-key global-map (kbd "C-c i @") #'jcs-delete-inside-at-sign)
  (define-key global-map (kbd "C-c i #") #'jcs-delete-inside-sharp-sign)
  (define-key global-map (kbd "C-c i $") #'jcs-delete-inside-dollar-sign)
  (define-key global-map (kbd "C-c i %") #'jcs-delete-inside-percent-sign)
  (define-key global-map (kbd "C-c i ^") #'jcs-delete-inside-caret)
  (define-key global-map (kbd "C-c i &") #'jcs-delete-inside-and)
  (define-key global-map (kbd "C-c i *") #'jcs-delete-inside-asterisk)
  (define-key global-map (kbd "C-c i -") #'jcs-delete-inside-dash)
  (define-key global-map (kbd "C-c i _") #'jcs-delete-inside-underscore)
  (define-key global-map (kbd "C-c i =") #'jcs-delete-inside-equal)
  (define-key global-map (kbd "C-c i +") #'jcs-delete-inside-plus)

  (define-key global-map (kbd "C-c i \\") #'jcs-delete-inside-backslash)
  (define-key global-map (kbd "C-c i |") #'jcs-delete-inside-or)

  (define-key global-map (kbd "C-c i :") #'jcs-delete-inside-colon)
  (define-key global-map (kbd "C-c i ;") #'jcs-delete-inside-semicolon)
  (define-key global-map (kbd "C-c i ,") #'jcs-delete-inside-comma)
  (define-key global-map (kbd "C-c i .") #'jcs-delete-inside-period)
  (define-key global-map (kbd "C-c i /") #'jcs-delete-inside-slash)
  (define-key global-map (kbd "C-c i ?") #'jcs-delete-inside-question-mark))

;;; Error
(define-key global-map [f9] #'first-error)
(define-key global-map [f10] #'previous-error)
(define-key global-map [f11] #'next-error)

;;; ESUP
(with-eval-after-load 'esup
  (define-key esup-mode-map (kbd "C-z") #'undo-tree-undo)
  (define-key esup-mode-map (kbd "C-y") #'undo-tree-redo))

;;; File Explorer
(define-key global-map (kbd "C-M-l") #'jcs-sr-speedbar-toggle)  ;; Compatible to `Visual Studio'.
(define-key global-map (kbd "C-b") #'jcs-sr-speedbar-toggle)    ;; Compatible to `VS Code'.
(with-eval-after-load 'sr-speedbar
  (define-key speedbar-mode-map (kbd "<backspace>") #'speedbar-up-directory)
  (define-key speedbar-mode-map (kbd "<return>") #'jcs-speedbar-edit-line)
  (define-key speedbar-mode-map (kbd "<f2>") #'speedbar-item-rename))

;;; File editing
(define-key global-map (kbd "M-k") #'jcs-maybe-kill-this-buffer)
(define-key global-map (kbd "M-K") #'jcs-reopen-this-buffer)
;;(define-key global-map (kbd "M-k") #'jcs-kill-this-buffer)
(define-key global-map [tab] #'jcs-tab-key)

;;; File Files
;;(define-key global-map "\ef" #'ido-find-file)
;;(define-key global-map "\eF" #'ido-find-file-other-window)
(define-key global-map (kbd "M-f") #'helm-find-files)
(define-key global-map (kbd "M-F") #'jcs-helm-find-files-other-window)
(define-key global-map (kbd "C-x M-f") #'helm-projectile-find-file)
(define-key global-map (kbd "C-x M-F") #'jcs-helm-projectile-find-file-other-window)

;;; Folding Settings
(define-key global-map (kbd "C-M-o") #'jcs-close-all-nodes)
(define-key global-map (kbd "C-M-p") #'jcs-open-all-nodes)

;;; Font
(define-key global-map (kbd "C-c f") #'jcs-change-font)

;;; Format file.
(define-key global-map "\C-k\C-f" #'indent-region)
(define-key global-map "\C-k\C-d" #'jcs-format-document)
(define-key global-map (kbd "C-S-f") #'jcs-format-region-or-document)
(define-key global-map "\C-xa" #'jcs-align-region-or-document)

;;; Goto Declaration/Definition
(define-key global-map [f12] #'dumb-jump-go-prefer-external)
(define-key global-map [S-f12] #'dumb-jump-go-prefer-external-other-window)

;;; Goto Thing
(define-key global-map (kbd "M-g c") #'goto-char-preview)
(define-key global-map (kbd "M-g l") #'goto-line-preview)

;;; Helm
(progn
  (define-key global-map (kbd "M-x") #'helm-M-x)
  (define-key global-map (kbd "M-y") #'helm-show-kill-ring)
  ;; NOTE: Match to OS's file explorer's navigation system.
  (with-eval-after-load 'helm-files
    (define-key helm-find-files-map (kbd "<return>") #'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "M-<up>") #'helm-find-files-up-one-level)
    (define-key helm-find-files-map (kbd "M-<left>") #'helm-find-files-up-one-level)
    (define-key helm-find-files-map (kbd "M-<right>") #'helm-find-files-down-last-level))
  (with-eval-after-load 'helm-projectile
    (define-key helm-projectile-find-file-map (kbd "<return>") #'jcs-helm-exit-minibuffer)))

;;; Help
(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "C-c C-c") #'kill-ring-save))

;;; Impatient Mode
(define-key global-map (kbd "C-w o") #'jcs-httpd-start)
(define-key global-map (kbd "C-w p") #'jcs-httpd-stop)

;;; Kill Word
(define-key global-map (kbd "C-<backspace>") #'jcs-backward-delete-word)
(define-key global-map (kbd "C-<delete>") #'jcs-forward-delete-word)

(define-key global-map (kbd "M-<backspace>") #'jcs-backward-kill-word-capital)
(define-key global-map (kbd "M-<delete>") #'jcs-forward-kill-word-capital)

;;; Line Endings
(define-key global-map (kbd "C-x C-e") #'set-buffer-file-coding-system)

;;; Mark
(define-key global-map "\e " #'set-mark-command)
(define-key global-map (kbd "M-z") #'toggle-truncate-lines)

(define-key global-map "\e:" #'View-back-to-mark)
(define-key global-map "\e;" #'exchange-point-and-mark)

;;; *Messages*
(define-key global-map (kbd "M-m") #'jcs-message-buffer)
(define-key global-map (kbd "M-M") #'jcs-message-buffer-other-window)

(define-key messages-buffer-mode-map "\ek" #'jcs-message-erase-buffer)
(define-key messages-buffer-mode-map "\eK" #'jcs-message-erase-buffer-stay)

;;; Minimap
(define-key global-map "\C-cm" #'jcs-toggle-minimap)

;;; Mode Line
(define-key global-map (kbd "C-M-m") #'jcs-toggle-mode-line)

;;; Mode Toggle
(progn
  ;;(define-key global-map "\e`" #'jcs-insert-command-mode-toggle)
  (define-key global-map (kbd "C-~") #'jcs-depend-cross-mode-toggle)
  (define-key global-map (kbd "C-`") #'jcs-toggle-shell-window))
(progn
  (define-key global-map (kbd "C-c c") #'jcs-toggle-cc-mode)
  (define-key global-map (kbd "C-c r") #'rainbow-mode))

;;; Mouse
(global-unset-key [mouse-2])  ;; no screwing with my middle mouse button

;;; Move Current Line Up or Down
(define-key global-map [M-up] #'move-text-up)
(define-key global-map [M-down] #'move-text-down)

;;; Mutliple Cursors
(define-key global-map (kbd "C-M-S-<up>") #'jcs-mc/mark-previous-like-this)
(define-key global-map (kbd "C-M-S-<down>") #'jcs-mc/mark-next-like-this)

;;; Navigation
(define-key global-map [home] #'jcs-beginning-of-line)
(define-key global-map [end] #'jcs-end-of-line)
(define-key global-map (kbd "M-<left>") #'jcs-backward-capital-char)
(define-key global-map (kbd "M-<right>") #'jcs-forward-capital-char)

;;; Open same file in other window.
(progn
  (define-key global-map (kbd "<f7>") #'jcs-same-file-other-window)
  ;; NOTE: If there are corresponding file, then
  ;; key <f8> should be replace by find corresponding file
  ;; interactive function call.
  (define-key global-map (kbd "<f8>") #'jcs-same-file-other-window))

;;; Open TODO file.
(define-key global-map (kbd "C-x t") #'jcs-open-project-todo-file)

;;; Open Log file.
(define-key global-map (kbd "C-x u") #'jcs-open-project-update-log-file)

;;; Org
(define-key global-map "\C-xo" #'org-mode)

;;; Overwrite
(define-key global-map [insert] #'jcs-overwrite-mode)

;;; Packages
(define-key global-map (kbd "C-p") #'package-list-packages)
(define-key package-menu-mode-map (kbd "s") #'jcs-package-menu-filter-by-status)
(define-key package-menu-mode-map (kbd "u") #'jcs-package-upgrade-all)
(define-key package-menu-mode-map (kbd "C-x r m") #'package-autoremove)

;;; Process
(define-key global-map (kbd "M-p") #'list-processes)

;;; Recent Files
(define-key global-map (kbd "C-r f") #'recentf-open-files)

;; Rename file
(define-key global-map (kbd "M-<f2>") #'jcs-rename-current-buffer-file)

;;; Return
(define-key global-map (kbd "C-<return>") #'jcs-ctrl-return-key)

;;; Revert Buffer
(define-key global-map "\er" #'jcs-revert-buffer-no-confirm)

;;; Right Click Context
(define-key global-map [S-f10] #'right-click-context-menu)

;;; Script Executing
(progn
  ;; Run
  (define-key global-map (kbd "<f5>") #'jcs-run-without-asking)

  ;; Build
  (define-key global-map (kbd "C-S-b") #'jcs-make-without-asking))

;;; Search Word
(progn
  ;; NOTE: Basic search is bind to `jcs-cross-mode' and `jcs-depend-mode'.
  ;; See `jcs-mode.el' file for the settings.
  (define-key global-map (kbd "C-r C-f") #'isearch-backward-regexp)

  (define-key global-map (kbd "C-,") #'jcs-isearch-backward-symbol-at-point)
  (define-key global-map (kbd "C-.") #'isearch-forward-symbol-at-point)
  (define-key global-map (kbd "C-<") #'jcs-isearch-project-backward-symbol-at-point)
  (define-key global-map (kbd "C->") #'isearch-project-forward-symbol-at-point)

  (define-key isearch-mode-map (kbd "C-,") #'jcs-isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-.") #'jcs-isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-<") #'jcs-isearch-project-repeat-backward)
  (define-key isearch-mode-map (kbd "C->") #'jcs-isearch-project-repeat-forward)

  ;; TODO: Implements isearch cursor for these two keys.
  ;;(define-key isearch-mode-map (kbd "C-x C-x") #'jcs-vs-cut-key)
  ;;(define-key isearch-mode-map (kbd "C-c C-c") #'kill-ring-save)
  (define-key isearch-mode-map (kbd "C-v") #'isearch-yank-pop)
  )

;;; Show Hover
(define-key global-map (kbd "C-k C-i") #'jcs-describe-thing-in-popup)

;;; Smooth Scrolling
(define-key global-map "\C-ca" #'jcs-toggle-sublimity-mode)

;;; Source Control
(define-key global-map (kbd "C-x g") #'magit-status)

;;; Special
(define-key special-mode-map (kbd "<up>") #'previous-line)
(define-key special-mode-map (kbd "<down>") #'next-line)

;;; Startup Screen (Dashboard)
(define-key global-map (kbd "M-d") #'jcs-dashboard)
(define-key global-map (kbd "M-D") #'jcs-dashboard-other-window)

(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "<backspace>")  #'jcs-dashboard-remove-current-item)
  (define-key dashboard-mode-map (kbd "<delete>")  #'jcs-dashboard-remove-current-item)
  (define-key dashboard-mode-map (kbd "d")  #'jcs-dashboard-remove-current-item)
  (define-key dashboard-mode-map "1" #'jcs-dashboard-item-section-1)
  (define-key dashboard-mode-map "2" #'jcs-dashboard-item-section-2)
  (define-key dashboard-mode-map "3" #'jcs-dashboard-item-section-3)
  (define-key dashboard-mode-map "4" #'jcs-dashboard-item-section-4)
  (define-key dashboard-mode-map "5" #'jcs-dashboard-item-section-5)
  (define-key dashboard-mode-map "6" #'jcs-dashboard-item-section-6)
  (define-key dashboard-mode-map "7" #'jcs-dashboard-item-section-7)
  (define-key dashboard-mode-map "8" #'jcs-dashboard-item-section-8)
  (define-key dashboard-mode-map "9" #'jcs-dashboard-item-section-9)
  (define-key dashboard-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key dashboard-mode-map (kbd "<down>") #'jcs-next-line)
  (define-key dashboard-mode-map (kbd "C-<up>") #'jcs-dashboard-previous-blank-line)
  (define-key dashboard-mode-map (kbd "C-<down>") #'jcs-dashboard-next-blank-line)
  (define-key dashboard-mode-map (kbd "C-p") #'package-list-packages)
  (define-key dashboard-mode-map (kbd "M-k") #'jcs-dashboard-maybe-kill-this-buffer)
  (define-key dashboard-mode-map (kbd "M-K") #'jcs-dashboard-refresh-buffer))

;;; Syntax Check
(define-key global-map (kbd "<f6>") #'jcs-flycheck-mode)
(with-eval-after-load 'flycheck
  (define-key flycheck-error-list-mode-map (kbd "M-k") #'jcs-flycheck-mode)
  (define-key flycheck-error-list-mode-map (kbd "M-K") #'flycheck-error-list-reset-filter))

;;; Tab Bar
(define-key global-map (kbd "C-t") #'jcs-toggle-tabbar-mode)
(define-key global-map [C-S-tab] #'tabbar-forward)
(define-key global-map [C-tab] #'tabbar-backward)

;;; Text Scale
(define-key global-map (kbd "C-=") #'jcs-text-scale-increase)
(define-key global-map (kbd "C--") #'jcs-text-scale-decrease)

;;; Todo
(with-eval-after-load 'hl-todo
  (define-key hl-todo-mode-map [C-f10] #'jcs-hl-todo-previous)
  (define-key hl-todo-mode-map [C-f11] #'jcs-hl-todo-next))

;;; Transparent Window
(define-key global-map "\e`" #'jcs-toggle-transparent-frame)
(define-key global-map "\e=" #'jcs-increment-frame-transparent)
(define-key global-map "\e-" #'jcs-decrement-frame-transparent)

;;; Window
(progn
  (define-key global-map (kbd "C-x n") #'make-frame)
  (define-key global-map (kbd "C-x d") #'delete-frame)  ; delete the external frame.
  (define-key global-map (kbd "C-x w") #'delete-window)  ; delete current window.
  (define-key global-map (kbd "C-h h") #'jcs-toggle-window-split-hv)
  (define-key global-map (kbd "C-w e") #'jcs-toggle-enlarge-window-selected)
  (define-key global-map (kbd "C-\\") #'split-window-horizontally))

;;; Window Navigation
(progn
  (define-key global-map "\ew" #'jcs-other-window-next)
  (define-key global-map "\eq" #'jcs-other-window-prev))

(progn
  ;; NOTE: Make compatible to Vim.
  (define-key global-map (kbd "C-w <up>") #'windmove-up)
  (define-key global-map (kbd "C-w <down>") #'windmove-down)
  (define-key global-map (kbd "C-w <left>") #'windmove-left)
  (define-key global-map (kbd "C-w <right>") #'windmove-right))

(progn
  (define-key global-map (kbd "M-e") #'ace-window)

  (define-key global-map (kbd "C-1") #'jcs-ace-window-1)
  (define-key global-map (kbd "C-2") #'jcs-ace-window-2)
  (define-key global-map (kbd "C-3") #'jcs-ace-window-3)
  (define-key global-map (kbd "C-4") #'jcs-ace-window-4)
  (define-key global-map (kbd "C-5") #'jcs-ace-window-5)
  (define-key global-map (kbd "C-6") #'jcs-ace-window-6)
  (define-key global-map (kbd "C-7") #'jcs-ace-window-7)
  (define-key global-map (kbd "C-8") #'jcs-ace-window-8)
  (define-key global-map (kbd "C-9") #'jcs-ace-window-9))

;;; Word Case
(define-key global-map (kbd "C-w u") #'jcs-upcase-word-or-region)
(define-key global-map (kbd "C-w d") #'jcs-downcase-word-or-region)

(define-key global-map (kbd "C-w c") #'jcs-capitalize-word-or-region)

;;; Undo/Redo
(define-key global-map (kbd "C-z") #'jcs-undo)
(define-key global-map (kbd "C-y") #'jcs-redo)

;;; Undo Tree
(with-eval-after-load 'undo-tree
  (define-key undo-tree-visualizer-mode-map (kbd "RET") #'undo-tree-visualizer-quit)
  ;; STUDY: `undo-tree''s minor mode will overwrite
  ;; the global key map's key bindings. What we need to do
  ;; is to remap this again...
  (define-key undo-tree-map (kbd "C-/") #'jcs-comment-uncomment-region-or-line))

;;; wgrep
(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map (kbd "<up>") #'jcs-previous-line)
  (define-key wgrep-mode-map (kbd "<down>") #'jcs-next-line)
  (define-key wgrep-mode-map (kbd "C-s") #'jcs-wgrep-finish-edit)
  (define-key wgrep-mode-map (kbd "M-K") #'jcs-ag-refresh-search))

;;; Whitespace
(define-key global-map (kbd "C-x b") #'whitespace-mode)


;;;---------------------------------------------
;;; Rebinding
;;;---------------------------------------------

(defun jcs-global-key-rebind ()
  "Some key are not allow to bind, the solution here is just re-bind
the key everytime the mode changes."

  ;; re-builder
  (define-key global-map "\C-rb" #'jcs-re-builder)

  ;; Read-Only toggle.
  (define-key global-map (kbd "C-r o") #'read-only-mode)

  ;; Replace
  (define-key global-map (kbd "C-r C-r") #'jcs-iedit-mode)

  ;; Recent Files
  (define-key global-map (kbd "C-r f") #'recentf-open-files)

  ;; Kill Buffer
  (define-key global-map (kbd "C-r DEL") #'jcs-backward-delete-current-char-repeat)
  (define-key global-map (kbd "C-r S-<backspace>") #'jcs-forward-delete-current-char-repeat)
  )


(provide 'jcs-key)
;;; jcs-key.el ends here
