;;; jcs-key.el --- Global Key Definition.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;-------------------------------------------------------------------
;; Unset key binding
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-x w"))

(with-eval-after-load 'auto-highlight-symbol
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M--") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") 'nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") 'nil))

(define-key Buffer-menu-mode-map (kbd "C-k") nil)

(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-r") nil)


;;-------------------------------------------------------------------
;; Set key bindings

;;; *Messages*
(define-key global-map (kbd "M-m") #'jcs-message-buffer)
(define-key global-map (kbd "M-M") #'jcs-message-buffer-other-window)

(define-key messages-buffer-mode-map "\ek" #'jcs-message-erase-buffer)
(define-key messages-buffer-mode-map "\eK" #'jcs-message-erase-buffer-stay)

;;; *scratch*
(define-key global-map (kbd "M-s") #'jcs-scratch-buffer)
(define-key global-map (kbd "M-S") #'jcs-scratch-buffer-other-window)

;;; Admin
(define-key global-map (kbd "M-<f4>") #'save-buffers-kill-terminal)
(define-key global-map (kbd "M-<f5>") #'reload-emacs)
(define-key global-map (kbd "M-<f6>") #'restart-emacs)

;;; ag
(with-eval-after-load 'ag
  (define-key ag-mode-map (kbd "M-K") #'jcs-ag-refresh-search))

;;; Auto Completion
(with-eval-after-load 'company
  (define-key company-active-map [tab] #'jcs-tab-key)
  (define-key company-active-map (kbd "TAB") #'jcs-tab-key)
  (define-key company-active-map (kbd "C-s") #'jcs-untabify-save-buffer))

;;; Buffer Menu
(define-key global-map (kbd "M-b") #'buffer-menu)
(define-key global-map (kbd "M-B") #'buffer-menu-other-window)

(define-key Buffer-menu-mode-map (kbd "M-K") #'buffer-menu)
(define-key Buffer-menu-mode-map (kbd "C-k C-s") #'describe-bindings)
(define-key Buffer-menu-mode-map (kbd "M-s") #'jcs-scratch-buffer)

(with-eval-after-load 'jcs-buffer-menu
  (progn  ; Sort
    (define-key Buffer-menu-mode-map (kbd "M-1") #'jcs-buffer-menu-sort-by-visit)
    (define-key Buffer-menu-mode-map (kbd "M-2") #'jcs-buffer-menu-sort-by-buffer)
    (define-key Buffer-menu-mode-map (kbd "M-3") #'jcs-buffer-menu-sort-by-size)
    (define-key Buffer-menu-mode-map (kbd "M-4") #'jcs-buffer-menu-sort-by-time)
    (define-key Buffer-menu-mode-map (kbd "M-5") #'jcs-buffer-menu-sort-by-mode)
    (define-key Buffer-menu-mode-map (kbd "M-6") #'jcs-buffer-menu-sort-by-file))

  (progn  ; Searching / Filtering
    (define-key Buffer-menu-mode-map (kbd "<escape>")
      (lambda () (interactive) (buffer-menu) (top-level)))
    (define-key Buffer-menu-mode-map (kbd "<return>") #'jcs-buffer-menu-return)

    (dolist (key-str jcs-key-list)
      (define-key Buffer-menu-mode-map key-str
        (lambda () (interactive) (jcs--buffer-menu-input key-str))))

    (define-key Buffer-menu-mode-map (kbd "<backspace>")
      (lambda () (interactive) (jcs--buffer-menu-input "" -1)))))

;;; Buffers
(define-key global-map (kbd "C-a") #'jcs-mark-whole-buffer)
(define-key global-map (kbd "M-r") #'revert-buffer)

(define-key global-map [S-tab] #'indent-for-tab-command)
(define-key global-map [backtab] #'indent-for-tab-command)
(define-key global-map (kbd "C-y") #'indent-for-tab-command)

;;; Binary/Hex Editor
(with-eval-after-load 'nhexl-mode
  (define-key nhexl-mode-map (kbd "<up>") #'previous-line)
  (define-key nhexl-mode-map (kbd "<down>") #'next-line)
  (define-key nhexl-mode-map (kbd "<right>") #'forward-char)
  (define-key nhexl-mode-map (kbd "<left>") #'backward-char))

;;; Calculator
(define-key global-map (kbd "C-k =") #'jcs-calc-eval-region)

;;; Canceling Action.
;;(define-key global-map (kbd "C-g") #'top-level)
(define-key global-map (kbd "<escape>") #'top-level)

;;; Comment/Uncomment
(define-key global-map (kbd "C-/") #'jcs-comment-uncomment-region-or-line)
(define-key global-map (kbd "C-k C-c") #'jcs-comment-region-or-line)
(define-key global-map (kbd "C-k C-u") #'jcs-uncomment-region-or-line)

;;; Debug
(define-key global-map (kbd "C-S-d") #'dap-mode)

;;; Describe Thing
(define-key global-map (kbd "C-k C-s") #'describe-bindings)

;;; Editting
(define-key global-map [C-right] #'jcs-smart-forward-word)
(define-key global-map [C-left] #'jcs-smart-backward-word)
(define-key global-map [C-up] #'jcs-previous-blank-line)
(define-key global-map [C-down] #'jcs-next-blank-line)
(define-key global-map [pgup] #'forward-page)
(define-key global-map [pgdown] #'backward-page)
(define-key global-map [C-next] #'scroll-other-window)
(define-key global-map [C-prior] #'scroll-other-window-down)

(bind-key* (kbd "C-r DEL") #'jcs-backward-delete-current-char-repeat)
(bind-key* (kbd "C-r S-<backspace>") #'jcs-forward-delete-current-char-repeat)

(define-key prog-mode-map (kbd "<backspace>") #'jcs-smart-backspace)
(define-key global-map (kbd "<backspace>") #'jcs-real-backspace)
(define-key global-map (kbd "S-<backspace>") #'jcs-real-backspace)

(define-key prog-mode-map (kbd "<delete>") #'jcs-smart-delete)
(define-key global-map (kbd "<delete>") #'jcs-real-delete)
(define-key global-map (kbd "S-<delete>") #'jcs-real-delete)

(define-key prog-mode-map (kbd "SPC") #'jcs-smart-space)
(define-key global-map (kbd "SPC") #'jcs-real-space)
(define-key global-map (kbd "S-SPC") #'jcs-real-space)

(define-key global-map (kbd "C-S-d") #'jcs-duplicate-line)
(bind-key* (kbd "C-d") #'jcs-kill-whole-line)

(bind-key* (kbd "C-x") #'jcs-vs-cut-key)
(bind-key* (kbd "C-c") #'kill-ring-save)
(define-key prog-mode-map (kbd "C-v") #'jcs-smart-yank)
(define-key global-map (kbd "C-v") #'yank)

(define-key global-map (kbd "C-s") #'jcs-untabify-save-buffer)
(define-key global-map (kbd "C-S-s") #'jcs-save-all-buffers)
(define-key global-map (kbd "C-k s") #'jcs-reverse-tab-untab-save-buffer)

(define-key prog-mode-map (kbd "<up>") (jcs-get-prev/next-key-type 'previous))
(define-key global-map (kbd "<up>") #'previous-line)
(define-key prog-mode-map (kbd "<down>") (jcs-get-prev/next-key-type 'next))
(define-key global-map (kbd "<down>") #'next-line)

(define-key global-map (kbd "C-M-<up>") #'jcs-scroll-down-one-line)
(define-key global-map (kbd "C-M-<down>") #'jcs-scroll-up-one-line)

(define-key global-map (kbd "C-M-<left>") #'buf-move-left)
(define-key global-map (kbd "C-M-<right>") #'buf-move-right)

(progn  ; Navigating General Programming Symbols
  (define-key global-map (kbd "M-)") #'jcs-move-forward-close-paren)
  (define-key global-map (kbd "M-(") #'jcs-move-backward-open-paren)
  (define-key global-map (kbd "M-]") #'jcs-move-forward-close-sqr-paren)
  (define-key global-map (kbd "M-[") #'jcs-move-backward-open-sqr-paren)
  (define-key global-map (kbd "M-}") #'jcs-move-forward-close-curly-paren)
  (define-key global-map (kbd "M-{") #'jcs-move-backward-open-curly-paren)
  (define-key global-map (kbd "M-'") #'jcs-move-forward-single-quot)
  (define-key global-map (kbd "M-;") #'jcs-move-backward-single-quot)
  (define-key global-map (kbd "M-\"") #'jcs-move-forward-double-quot)
  (define-key global-map (kbd "M-:") #'jcs-move-backward-double-quot)
  (define-key global-map (kbd "M->") #'jcs-move-forward-greater-than-sign)
  (define-key global-map (kbd "M-<") #'jcs-move-backward-less-than-sign)

  (define-key global-map (kbd "M-.") #'jcs-move-forward-comma)
  (define-key global-map (kbd "M-,") #'jcs-move-backward-comma)
  (define-key global-map (kbd "C-M-.") #'jcs-move-forward-period)
  (define-key global-map (kbd "C-M-,") #'jcs-move-backward-period))


(progn  ; Changing/Deleting inside between Programming Symbols
  (define-key global-map (kbd "C-k i [") #'jcs-delete-inside-sqr-paren)
  (define-key global-map (kbd "C-k i ]") #'jcs-delete-inside-sqr-paren)
  (define-key global-map (kbd "C-k i (") #'jcs-delete-inside-paren)
  (define-key global-map (kbd "C-k i )") #'jcs-delete-inside-paren)
  (define-key global-map (kbd "C-k i {") #'jcs-delete-inside-curly-paren)
  (define-key global-map (kbd "C-k i }") #'jcs-delete-inside-curly-paren)
  (define-key global-map (kbd "C-k i '") #'jcs-delete-inside-single-quot)
  (define-key global-map (kbd "C-k i \"") #'jcs-delete-inside-double-quot)
  (define-key global-map (kbd "C-k i <") #'jcs-delete-inside-greater-less-sign)
  (define-key global-map (kbd "C-k i >") #'jcs-delete-inside-less-greater-sign)

  (define-key global-map (kbd "C-k i `") #'jcs-delete-inside-back-quot)
  (define-key global-map (kbd "C-k i ~") #'jcs-delete-inside-tilde)
  (define-key global-map (kbd "C-k i !") #'jcs-delete-inside-exclamation-mark)
  (define-key global-map (kbd "C-k i @") #'jcs-delete-inside-at-sign)
  (define-key global-map (kbd "C-k i #") #'jcs-delete-inside-sharp-sign)
  (define-key global-map (kbd "C-k i $") #'jcs-delete-inside-dollar-sign)
  (define-key global-map (kbd "C-k i %") #'jcs-delete-inside-percent-sign)
  (define-key global-map (kbd "C-k i ^") #'jcs-delete-inside-caret)
  (define-key global-map (kbd "C-k i &") #'jcs-delete-inside-and)
  (define-key global-map (kbd "C-k i *") #'jcs-delete-inside-asterisk)
  (define-key global-map (kbd "C-k i -") #'jcs-delete-inside-dash)
  (define-key global-map (kbd "C-k i _") #'jcs-delete-inside-underscore)
  (define-key global-map (kbd "C-k i =") #'jcs-delete-inside-equal)
  (define-key global-map (kbd "C-k i +") #'jcs-delete-inside-plus)

  (define-key global-map (kbd "C-k i \\") #'jcs-delete-inside-backslash)
  (define-key global-map (kbd "C-k i |") #'jcs-delete-inside-or)

  (define-key global-map (kbd "C-k i :") #'jcs-delete-inside-colon)
  (define-key global-map (kbd "C-k i ;") #'jcs-delete-inside-semicolon)
  (define-key global-map (kbd "C-k i ,") #'jcs-delete-inside-comma)
  (define-key global-map (kbd "C-k i .") #'jcs-delete-inside-period)
  (define-key global-map (kbd "C-k i /") #'jcs-delete-inside-slash)
  (define-key global-map (kbd "C-k i ?") #'jcs-delete-inside-question-mark))

;;; Error
(define-key global-map [f9] #'first-error)
(define-key global-map [f10] #'previous-error)
(define-key global-map [f11] #'next-error)

;;; ESUP
(with-eval-after-load 'esup
  (define-key esup-mode-map (kbd "C-z") #'undo-tree-undo)
  (define-key esup-mode-map (kbd "C-y") #'undo-tree-redo))

;;; File Explorer
(define-key global-map (kbd "C-M-l") #'neotree-toggle)  ; Compatible to `Visual Studio'.
(define-key global-map (kbd "C-b") #'neotree-toggle)    ; Compatible to `VS Code'.

;;; File editing
(define-key global-map (kbd "C-<f4>") #'kill-this-buffer)
(define-key global-map (kbd "M-k") #'jcs-maybe-kill-this-buffer)
(define-key global-map (kbd "M-K") #'jcs-reopen-this-buffer)
;;(define-key global-map (kbd "M-k") #'jcs-kill-this-buffer)
(define-key global-map [tab] #'jcs-tab-key)

;;; File Files
(define-key global-map (kbd "M-f") #'counsel-find-file)
(define-key global-map (kbd "C-p") #'counsel-find-file)
(define-key global-map (kbd "M-F") #'jcs-counsel-find-files-other-window)
(define-key global-map (kbd "C-k M-f") #'counsel-projectile-find-file)
(define-key global-map (kbd "C-k M-F") #'jcs-counsel-projectile-find-file-other-window)

;;; Folding Settings
(define-key global-map (kbd "C-k C-0") #'jcs-close-all-nodes)
(define-key global-map (kbd "C-k C-j") #'jcs-open-all-nodes)
(define-key global-map (kbd "C-{") #'jcs-close-node)
(define-key global-map (kbd "C-}") #'jcs-open-node)

;;; Font
(define-key global-map (kbd "C-k f") #'jcs-change-font)

;;; Format file.
(define-key global-map (kbd "C-k C-f") #'indent-region)
(define-key global-map (kbd "C-k C-d") #'jcs-format-document)
(define-key global-map (kbd "C-k a") #'jcs-align-region-or-document)

;;; Goto Address
(with-eval-after-load 'goto-addr
  (define-key goto-address-highlight-keymap (kbd "C-c") nil))

;;; Goto Definition
(define-key global-map [f12] #'jcs-goto-definition)
(define-key global-map [S-f12] #'jcs-goto-definition-other-window)

;;; Goto Thing
(define-key global-map (kbd "M-g c") #'goto-char-preview)
(define-key global-map (kbd "M-g l") #'goto-line-preview)

;;; Help
(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "C-c") #'kill-ring-save))

;;; Iedit
(bind-key* (kbd "C-r C-r") #'jcs-iedit-mode)

;;; Impatient Mode
(define-key global-map (kbd "C-w o") #'jcs-httpd-start)
(define-key global-map (kbd "C-w p") #'jcs-httpd-stop)

;;; Ivy / Counsel / Swiper
(progn
  (define-key global-map (kbd "M-x") #'counsel-M-x)
  (progn  ; Compatible to VSCode.
    (define-key global-map (kbd "C-S-p") #'counsel-M-x)
    (define-key global-map (kbd "<f1>") #'counsel-M-x))
  (define-key global-map (kbd "M-y") #'counsel-yank-pop)
  (with-eval-after-load 'counsel
    (define-key counsel-find-file-map (kbd "<backspace>") #'jcs-counsel-find-files-backspace)
    (define-key counsel-find-file-map (kbd "<return>") #'jcs-counsel-find-files-enter)
    (define-key counsel-find-file-map (kbd "/") #'jcs-counsel-find-files--slash)))

;;; Kill Ring
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "<escape>") #'kill-this-buffer))

;;; Kill Word
(define-key global-map (kbd "C-<backspace>") #'jcs-smart-backward-delete-word)
(define-key global-map (kbd "C-<delete>") #'jcs-smart-forward-delete-word)

(define-key global-map (kbd "M-<backspace>") #'jcs-backward-kill-word-capital)
(define-key global-map (kbd "M-<delete>") #'jcs-forward-kill-word-capital)

;;; Line Endings
(define-key global-map (kbd "C-k C-e") #'set-buffer-file-coding-system)

;;; Mark
(define-key global-map "\e " #'set-mark-command)
(define-key global-map (kbd "M-z") #'toggle-truncate-lines)

(define-key global-map "\e:" #'View-back-to-mark)
(define-key global-map "\e;" #'exchange-point-and-mark)

;;; Media
(with-eval-after-load 'ffmpeg-player
  (define-key ffmpeg-player-mode-map (kbd "M-k") #'jcs-media-close-media-window)
  (define-key ffmpeg-player-mode-map (kbd "M-K") #'ffmpeg-player-replay))

;;; Minimap
(define-key global-map (kbd "C-k m") #'jcs-toggle-minimap)

;;; Mode Line
(define-key global-map (kbd "C-M-m") #'feebleline-mode)

;;; Mode Toggle
(progn
  ;;(define-key global-map (kbd "M-`") #'jcs-insert-command-mode-toggle)
  (define-key global-map (kbd "C-k `") #'jcs-depend-cross-mode-toggle)
  (define-key global-map (kbd "C-~") #'jcs-shell-new-shell)
  (define-key global-map (kbd "C-`") #'jcs-toggle-shell-window))
(progn
  (define-key global-map (kbd "C-k c") #'jcs-toggle-cc-mode)
  (define-key global-map (kbd "C-k r") #'rainbow-mode))

;;; Mouse
(define-key global-map [mouse-2] #'mouse-set-point)

;;; Move Current Line Up or Down
(define-key global-map [M-up] #'move-text-up)
(define-key global-map [M-down] #'move-text-down)

;;; Mutliple Cursors
(define-key global-map (kbd "C-M-S-<up>") #'jcs-mc/mark-previous-like-this)
(define-key global-map (kbd "C-M-S-<down>") #'jcs-mc/mark-next-like-this)
(define-key global-map (kbd "C-M-_") #'jcs-mc/mark-previous-similar-this)
(define-key global-map (kbd "C-M-+") #'jcs-mc/mark-next-similar-this)

(with-eval-after-load 'multiple-cursors
  (define-key mc/keymap (kbd "<escape>") #'mc/keyboard-quit)
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-v") #'jcs-smart-yank))

;;; Navigation
(define-key global-map (kbd "C-<home>") #'jcs-beginning-of-buffer)
(define-key global-map (kbd "C-<end>") #'jcs-end-of-buffer)
(define-key global-map [home] #'jcs-beginning-of-line)
(define-key global-map [end] #'jcs-end-of-line)
(bind-key* (kbd "M-<left>") #'jcs-backward-word-capital)
(bind-key* (kbd "M-<right>") #'jcs-forward-word-capital)

;;; Open same file in other window.
(progn
  (define-key global-map (kbd "<f7>") #'jcs-same-file-other-window)
  ;; NOTE: If there are corresponding file, then
  ;; key <f8> should be replace by find corresponding file
  ;; interactive function call.
  (define-key global-map (kbd "<f8>") #'jcs-same-file-other-window))

;;; Open TODO file.
(define-key global-map (kbd "C-k t") #'jcs-open-project-todo-file)

;;; Open Log file.
(define-key global-map (kbd "C-k u") #'jcs-open-project-update-log-file)

;;; Overwrite
(define-key global-map [insert] #'jcs-overwrite-mode)

;;; Packages
(define-key global-map (kbd "C-k C-p") #'package-list-packages)
(define-key global-map (kbd "C-S-x") #'package-list-packages)
(define-key package-menu-mode-map (kbd "s") #'jcs-package-menu-filter-by-status)
(define-key package-menu-mode-map (kbd "u") #'jcs-package-upgrade-all)
(define-key package-menu-mode-map (kbd "C-k r m") #'package-autoremove)

;;; Process
(define-key global-map (kbd "M-p") #'list-processes)

;;; RE-Builder
(bind-key* (kbd "C-r b") #'jcs-re-builder)

;;; Read-Only
(bind-key* (kbd "C-r o") #'read-only-mode)

;;; Recent Files
(bind-key* (kbd "C-r f") #'recentf-open-files)

;; Rename file
(define-key global-map (kbd "M-<f2>") #'jcs-rename-current-buffer-file)

;;; Return
(define-key global-map (kbd "C-<return>") #'jcs-ctrl-return-key)

;;; Reveal In Folder
(define-key global-map (kbd "M-R") #'reveal-in-folder)

;;; Revert Buffer
(define-key global-map "\er" #'jcs-revert-buffer-no-confirm)

;;; Right Click Context
(define-key global-map [S-f10] #'right-click-context-menu)

;;; Script Executing (Output)
(define-key global-map (kbd "C-S-u") #'jcs-output-window)
(define-key global-map (kbd "M-o") #'jcs-dev-switch-to-output-buffer)
(progn
  (define-key global-map (kbd "<f5>") #'jcs-run-without-asking)     ; Run
  (define-key global-map (kbd "C-S-b") #'jcs-make-without-asking))  ; Build

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
  ;;(define-key isearch-mode-map (kbd "C-x") #'jcs-vs-cut-key)
  ;;(define-key isearch-mode-map (kbd "C-c") #'kill-ring-save)
  (define-key isearch-mode-map (kbd "C-v") #'isearch-yank-pop))

;;; Show Hover
(define-key global-map (kbd "C-k C-i") #'jcs-describe-thing-in-popup)

;;; Source Control
(define-key global-map (kbd "C-S-g") #'magit-status)

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
  (define-key dashboard-mode-map (kbd "g") #'jcs-dashboard-refresh-buffer)
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
  (define-key dashboard-mode-map (kbd "C-k C-p") #'package-list-packages)
  (define-key dashboard-mode-map (kbd "M-k") #'jcs-dashboard-maybe-kill-this-buffer)
  (define-key dashboard-mode-map (kbd "M-K") #'jcs-dashboard-refresh-buffer))

;;; Syntax Check
(define-key global-map (kbd "<f6>") #'jcs-flycheck-mode)
(with-eval-after-load 'flycheck
  (define-key flycheck-error-list-mode-map (kbd "M-k") #'jcs-flycheck-mode)
  (define-key flycheck-error-list-mode-map (kbd "M-K") #'flycheck-error-list-reset-filter))

;;; Tab Bar
(define-key global-map (kbd "C-t") #'jcs-toggle-tabbar-mode)
(bind-key* [C-S-tab] #'centaur-tabs-backward)
(bind-key* [C-tab] #'centaur-tabs-forward)
(define-key global-map (kbd "C-<prior>") #'centaur-tabs-backward)
(define-key global-map (kbd "C-<next>") #'centaur-tabs-forward)

;;; Tab Width
(define-key global-map (kbd "C-k >") #'jcs-inc-tab-width)
(define-key global-map (kbd "C-k <") #'jcs-dec-tab-width)

;;; Zoom
(define-key global-map (kbd "C-=") #'jcs-text-scale-increase)
(define-key global-map (kbd "C--") #'jcs-text-scale-decrease)
(define-key global-map (kbd "C-<kp-0>") #'jcs-reset-zoom)

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
  ;; TODO: Determine `toggle-frame-fullscreen' key.
  ;;(define-key global-map [f11] #'toggle-frame-fullscreen)
  (define-key global-map (kbd "C-k n") #'jcs-make-frame)
  (define-key global-map (kbd "C-k d") #'delete-frame)  ; delete the external frame.
  (define-key global-map (kbd "C-k w") #'jcs-balance-delete-window)  ; delete current window.
  (define-key global-map (kbd "C-h h") #'jcs-toggle-window-split-hv)
  (define-key global-map (kbd "C-w e") #'jcs-toggle-enlarge-window-selected)
  (define-key global-map (kbd "C-\\") #'jcs-balance-split-window-horizontally)
  (define-key global-map (kbd "C-|") #'jcs-balance-split-window-vertically))

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
(define-key global-map (kbd "C-k b") #'whitespace-mode)


(provide 'jcs-key)
;;; jcs-key.el ends here
