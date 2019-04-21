;;; jcs-key.el --- Global Key Definition.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;---------------------------------------------
;;; Unset key binding
;;;---------------------------------------------
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>") 'nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>") 'nil)

(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-r") nil)


;;;---------------------------------------------
;;; Set key binding
;;;---------------------------------------------

;;; Admin
(define-key global-map "\C-x\C-v" #'reload-emacs)
(define-key global-map "\C-x\C-b" #'restart-emacs)

;;; Auto Completion
(require 'company)
(define-key company-active-map [tab] #'company-complete-selection)
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "C-s") #'jcs-untabify-save-buffer)

;;; Buffer Menu
(define-key global-map (kbd "M-m") #'jcs-buffer-menu)
(define-key global-map (kbd "M-M") #'buffer-menu-other-window)

(define-key Buffer-menu-mode-map "\eK" #'jcs-buffer-menu)
(define-key Buffer-menu-mode-map "1" #'jcs-buffer-menu-sort-by-visit)
(define-key Buffer-menu-mode-map "2" #'jcs-buffer-menu-sort-by-buffer)
(define-key Buffer-menu-mode-map "3" #'jcs-buffer-menu-sort-by-size)
(define-key Buffer-menu-mode-map "4" #'jcs-buffer-menu-sort-by-time)
(define-key Buffer-menu-mode-map "5" #'jcs-buffer-menu-sort-by-mode)
(define-key Buffer-menu-mode-map "6" #'jcs-buffer-menu-sort-by-file)

;;; Buffers
(define-key global-map "\C-a" #'jcs-mark-whole-buffer)
(define-key global-map "\er" #'revert-buffer)
(define-key global-map "\es" #'save-buffer)

(define-key global-map [S-tab] #'indent-for-tab-command)
(define-key global-map [backtab] #'indent-for-tab-command)
(define-key global-map (kbd "C-y") #'indent-for-tab-command)
(define-key global-map [C-tab] #'indent-region)

;;; Binary/Hex Editor
(require 'nhexl-mode)
(define-key nhexl-mode-map (kbd "<up>") #'previous-line)
(define-key nhexl-mode-map (kbd "<down>") #'next-line)
(define-key nhexl-mode-map (kbd "<right>") #'forward-char)
(define-key nhexl-mode-map (kbd "<left>") #'backward-char)

;;; Canceling Action.
;;(define-key global-map "\C-g" #'jcs-top-level)
(define-key global-map (kbd "<escape>") #'jcs-top-level)

;;; Comment/Uncomment
(define-key global-map (kbd "C-/") #'jcs-comment-uncomment-region-or-line)
(define-key global-map "\C-k\C-c" #'jcs-comment-region-or-line)
(define-key global-map "\C-k\C-u" #'jcs-uncomment-region-or-line)

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

(define-key global-map "" #'copy-region-as-kill)
(define-key global-map "" #'yank)
(define-key global-map "" #'nil)
(define-key global-map "" #'rotate-yank-pointer)
(define-key global-map "\eu" #'undo)
(define-key global-map "\e6" #'upcase-word)
(define-key global-map "\e^" #'captilize-word)
(define-key global-map "\e." #'fill-paragraph)

(define-key global-map "\eo" #'query-replace)

(define-key global-map "\e[" #'start-kbd-macro)
(define-key global-map "\e]" #'end-kbd-macro)
(define-key global-map "\e'" #'call-last-kbd-macro)

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

;;; File Explorer
(require 'sr-speedbar)
(define-key global-map (kbd "C-M-l") #'jcs-sr-speedbar-toggle)  ;; Compatible to `Visual Studio'.
(define-key global-map (kbd "C-b") #'jcs-sr-speedbar-toggle)    ;; Compatible to `VS Code'.
(define-key speedbar-mode-map (kbd "<backspace>") #'speedbar-up-directory)
(define-key speedbar-mode-map (kbd "<return>") #'jcs-speedbar-edit-line)
(define-key speedbar-mode-map (kbd "<f2>") #'speedbar-item-rename)

;;; File editing
(define-key global-map (kbd "M-k") #'jcs-maybe-kill-this-buffer)
(define-key global-map (kbd "M-K") #'jcs-reopen-this-buffer)
;;(define-key global-map (kbd "M-k") #'jcs-kill-this-buffer)
(define-key global-map [tab] #'jcs-tab-key)

;;; File Files
;;(define-key global-map "\ef" #'ido-find-file)
;;(define-key global-map "\eF" #'ido-find-file-other-window)
(define-key global-map (kbd "M-f") #'jcs-helm-find-files)
(define-key global-map (kbd "M-F") #'jcs-helm-find-files-other-window)
(define-key global-map (kbd "C-x M-f") #'helm-projectile-find-file)
(define-key global-map (kbd "C-x M-F") #'jcs-helm-projectile-find-file-other-window)

;;; Folding Settings
(define-key global-map (kbd "C-M-o") #'origami-close-all-nodes)
(define-key global-map (kbd "C-M-p") #'origami-open-all-nodes)

;;; Font
(define-key global-map (kbd "C-c f") #'jcs-change-font)

;;; Format file.
(define-key global-map "\C-k\C-f" #'indent-region)
(define-key global-map "\C-k\C-d" #'jcs-format-document)
(define-key global-map (kbd "C-S-f") #'jcs-format-region-or-document)
(define-key global-map "\C-xa" #'jcs-align-region-or-document)

;;; Goto Thing
(define-key global-map (kbd "M-g l") #'goto-line-preview)
;;(define-key global-map (kbd "M-g c") #'goto-char-preview)
(define-key global-map (kbd "M-g c") #'goto-char)

;;; Helm
(progn
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)

  (define-key global-map [f12] 'jcs-helm-gtags-to-def-dec)
  (define-key global-map [S-f12] 'jcs-helm-gtags-to-def-dec-other-window))

;;; Help
(define-key help-mode-map (kbd "C-c C-c") #'kill-ring-save)

;;; Impatient Mode
(define-key global-map "\C-wo" #'jcs-httpd-start)
(define-key global-map "\C-wp" #'jcs-httpd-stop)

;;; Kill Word
(define-key global-map (kbd "C-<backspace>") #'jcs-backward-delete-word)
(define-key global-map (kbd "C-<delete>") #'jcs-forward-delete-word)

(define-key global-map (kbd "M-<backspace>") #'jcs-backward-kill-word-capital)
(define-key global-map (kbd "M-<delete>") #'jcs-forward-kill-word-capital)

;;; Line Endings
(define-key global-map "\C-x\C-e" #'set-buffer-file-coding-system)

;;; Mark
(define-key global-map "\e " #'set-mark-command)
(define-key global-map (kbd "M-z") #'toggle-truncate-lines)

(define-key global-map "\e:" #'View-back-to-mark)
(define-key global-map "\e;" #'exchange-point-and-mark)

;;; *Messages*
(define-key messages-buffer-mode-map "\ek" #'jcs-message-erase-buffer)
(define-key messages-buffer-mode-map "\eK" #'jcs-message-erase-buffer-stay)

;;; Minimap
(define-key global-map "\C-cm" #'jcs-toggle-minimap)

;;; Mode toggle
(progn
  ;;(define-key global-map "\e`" #'jcs-insert-command-mode-toggle)
  (define-key global-map (kbd "C-~") #'jcs-depend-cross-mode-toggle)
  (define-key global-map (kbd "C-`") #'jcs-toggle-shell-window))
(progn
  (define-key global-map (kbd "C-c c") #'jcs-toggle-cc-mode)
  (define-key global-map (kbd "C-c r") #'rainbow-mode)
  (define-key global-map (kbd "C-x b") #'whitespace-mode))

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
  ;; NOTE(jenchieh): If there are corresponding file, then
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

;;; Script Executing
(progn
  ;; Run
  (define-key global-map "\e]" #'jcs-run-without-asking)
  (define-key global-map (kbd "<f5>") #'jcs-run-without-asking)

  ;; Build
  (define-key global-map (kbd "C-S-b") #'jcs-make-without-asking))

;;; Search Word
(progn
  ;; NOTE(jenchieh): Basic search is bind to `jcs-cross-mode' and `jcs-depend-mode'.
  ;; See `jcs-mode.el' file for the settings.
  (define-key global-map (kbd "C-r C-f") #'isearch-backward-regexp)

  (define-key global-map (kbd "M-S-<left>") #'jcs-isearch-backward-symbol-at-point)
  (define-key global-map (kbd "M-S-<right>") #'isearch-forward-symbol-at-point)

  (define-key isearch-mode-map (kbd "M-S-<left>") #'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-S-<right>") #'isearch-repeat-forward))

;;; Show Hover
(define-key global-map (kbd "C-k C-i") #'jcs-describe-thing-in-popup)

;;; Smooth Scrolling
(define-key global-map "\C-ca" #'jcs-toggle-sublimity-mode)

;;; Source Control
(define-key global-map (kbd "C-x g") #'magit-status)

;;; Startup Screen
(require 'dashboard)
(define-key dashboard-mode-map (kbd "<up>") #'jcs-previous-line)
(define-key dashboard-mode-map (kbd "<down>") #'jcs-next-line)
(define-key dashboard-mode-map (kbd "C-p") #'package-list-packages)

;;; Syntax Check
(require 'flycheck)
(define-key global-map (kbd "<f6>") #'jcs-flycheck-mode)
(define-key flycheck-error-list-mode-map (kbd "M-k") #'jcs-flycheck-mode)
(define-key flycheck-error-list-mode-map (kbd "M-K") #'flycheck-error-list-reset-filter)

;;; Tab Bar
(define-key global-map (kbd "C-t") #'jcs-toggle-tabbar-mode)
(define-key global-map [C-S-tab] #'tabbar-forward)
(define-key global-map [C-tab] #'tabbar-backward)

;;; Text Scale
(define-key global-map (kbd "C-=") #'jcs-text-scale-increase)
(define-key global-map (kbd "C--") #'jcs-text-scale-decrease)

;;; Transparent Window
(define-key global-map "\e`" #'jcs-toggle-transparent-frame)
(define-key global-map "\e=" #'jcs-increment-frame-transparent)
(define-key global-map "\e-" #'jcs-decrement-frame-transparent)

;;; Window
(progn
  (define-key global-map "\C-xn" #'jcs-new-frame)
  (define-key global-map "\C-xd" #'delete-frame)  ; delete the external frame.
  (define-key global-map "\C-xw" #'delete-window)  ; delete current window.
  (define-key global-map "\C-hh" #'jcs-toggle-window-split-hv)
  (define-key global-map "\C-we" #'jcs-toggle-enlarge-window-selected)
  (define-key global-map (kbd "C-\\") #'split-window-horizontally))

;;; Window Navigation
(progn
  (define-key global-map "\ew" #'jcs-other-window-next)
  (define-key global-map "\eq" #'jcs-other-window-prev))

(progn
  ;; NOTE(jenchieh): Make compatible to Vim.
  (define-key global-map (kbd "C-w <up>") #'windmove-up)
  (define-key global-map (kbd "C-w <down>") #'windmove-down)
  (define-key global-map (kbd "C-w <left>") #'windmove-left)
  (define-key global-map (kbd "C-w <right>") #'windmove-right))

(progn
  (define-key global-map (kbd "M-e") #'ace-window)

  (define-key global-map (kbd "M-1") #'jcs-ace-window-1)
  (define-key global-map (kbd "M-2") #'jcs-ace-window-2)
  (define-key global-map (kbd "M-3") #'jcs-ace-window-3)
  (define-key global-map (kbd "M-4") #'jcs-ace-window-4)
  (define-key global-map (kbd "M-5") #'jcs-ace-window-5)
  (define-key global-map (kbd "M-6") #'jcs-ace-window-6)
  (define-key global-map (kbd "M-7") #'jcs-ace-window-7)
  (define-key global-map (kbd "M-8") #'jcs-ace-window-8)
  (define-key global-map (kbd "M-9") #'jcs-ace-window-9)

  (define-key global-map (kbd "C-1") #'jcs-ace-window-1)
  (define-key global-map (kbd "C-2") #'jcs-ace-window-2)
  (define-key global-map (kbd "C-3") #'jcs-ace-window-3)
  (define-key global-map (kbd "C-4") #'jcs-ace-window-4)
  (define-key global-map (kbd "C-5") #'jcs-ace-window-5)
  (define-key global-map (kbd "C-6") #'jcs-ace-window-6)
  (define-key global-map (kbd "C-7") #'jcs-ace-window-7)
  (define-key global-map (kbd "C-8") #'jcs-ace-window-8)
  (define-key global-map (kbd "C-9") #'jcs-ace-window-9))

;;; Upper/Down case key binding.
(define-key global-map "\eu" #'upcase-word)
(define-key global-map "\ed" #'downcase-word)

;;; Undo/Redo
(define-key global-map "\C-z" #'jcs-undo)
(define-key global-map "\C-y" #'jcs-redo)

;;; Undo Tree
(define-key undo-tree-visualizer-mode-map (kbd "RET") #'undo-tree-visualizer-quit)
;; STUDY(jenchieh): `undo-tree''s minor mode will overwrite
;; the global key map's key bindings. What we need to do
;; is to remap this again...
(define-key undo-tree-map (kbd "C-/") #'jcs-comment-uncomment-region-or-line)


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

;; NOTE(jenchieh): Call depend mode once.
(call-interactively #'jcs-depend-mode)


(provide 'jcs-key)
;;; jcs-key.el ends here
