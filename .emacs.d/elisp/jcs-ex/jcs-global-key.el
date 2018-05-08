;; ========================================================================
;; $File: jcs-global-key.el $
;; $Date: 2017-03-17 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;================================================
;; JayCeS Global Key
;;================================================

;; Setup my key binding
(global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
(global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

;; no screwing with my middle mouse button
(global-unset-key [mouse-2])

(define-key global-map "\et" 'load-todo)
(define-key global-map "\eT" 'load-log)

(define-key global-map [f8] 'casey-replace-string)

(define-key global-map "\ep" 'quick-calc)

(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)

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
(define-key global-map "\es" 'save-buffer)

(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-for-tab-command)
(define-key global-map "\C-y" 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
;;(define-key global-map "      " 'indent-region)


;;; Bind the Key
;;(define-key global-map "\ef" 'ido-find-file)
;;(define-key global-map "\eF" 'ido-find-file-other-window)
(define-key global-map "\ef" 'jcs-helm-find-files)
(define-key global-map "\eF" 'jcs-helm-find-files-other-window)
;;(define-key global-map "\C-xf" 'jcs-smart-find-file-in-project)
;;(define-key global-map "\C-xF" 'jcs-smart-find-file-in-project-in-another-window)

(define-key global-map "\C-cd" 'duplicate-line)
;;(define-key global-map (kbd "C-d") 'kill-whole-line)   ;; Emacs default version
(define-key global-map (kbd "C-d") 'jcs-kill-whole-line)    ;; delete the line without copying!!
(define-key global-map "\C-x\C-x" 'kill-region)
(define-key global-map "\C-c\C-c" 'kill-ring-save)
(define-key global-map "\C-v" 'yank)
(define-key global-map "\C-s" 'jcs-untabify-save-buffer)
(define-key global-map (kbd "C-S-s") 'jcs-tabify-save-buffer)

(define-key global-map "\C-p" 'package-list-packages)

(define-key global-map (kbd "C-<return>") 'goto-address-at-point)

;;; Kill Word
(define-key global-map (kbd "C-<backspace>") 'jcs-backward-delete-word)
(define-key global-map (kbd "C-S-<backspace>") 'jcs-forward-delete-word)
(define-key global-map (kbd "M-<backspace>") 'jcs-backward-kill-word-capital)
(define-key global-map (kbd "M-S-<backspace>") 'jcs-forward-kill-word-capital)

;;; Navigate Word
(define-key global-map [home] 'back-to-indentation-or-beginning)

(define-key global-map (kbd "M-<left>") 'jcs-backward-capital-char)
(define-key global-map (kbd "M-<right>") 'jcs-forward-capital-char)
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>") 'nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>") 'nil)

;;; Search Word (SEE: `jcs-mode-el' will have detail mode config.)ee
;;(define-key global-map "\C-f" 'helm-do-ag-this-file)
;;(define-key global-map "\C-f" 'isearch-forward)  ;; NOTE(jenchieh): 'local' vs 'cross-platform' mode.
(define-key global-map "\C-r\C-f" 'isearch-backward-regexp)

(define-key global-map (kbd "M-S-<right>") 'jcs-search-forward-at-point)
(define-key global-map (kbd "M-S-<left>") 'jcs-search-backword-at-point)
(define-key isearch-mode-map (kbd "M-S-<right>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<left>") 'isearch-repeat-backward)

(define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") 'jcs-search-forward-at-point)
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") 'jcs-search-backword-at-point)

;;; Admin
(define-key global-map "\C-x\C-v" 'jcs-reload-emacs)

;;; Undo and Redo
(define-key global-map "\C-a" 'mark-whole-buffer)
(define-key global-map "\C-z" 'undo)
;;(define-key global-map "\C-y" 'redo)

;;; Text Scale.
(define-key global-map "\e=" 'text-scale-increase)
(define-key global-map "\e-" 'text-scale-decrease)
(define-key auto-highlight-symbol-mode-map "\e-" 'text-scale-decrease)

;;; Mode toggle
;;(define-key global-map "\e`" 'jcs-insert-command-mode-toggle)
(define-key global-map "\C-cd" 'jcs-toggle-shell-window) ; shell command
(define-key global-map (kbd "C-`") 'jcs-depend-cross-mode-toggle)

;;; Window
(define-key global-map "\C-xn" 'jcs-new-window)
(define-key global-map "\C-xd" 'delete-frame)  ; delete the external window
(define-key global-map "\C-hh" 'jcs-toggle-window-split)

;;; Rename file
(define-key global-map "\C-xr" 'jcs-rename-current-buffer-file)

;;; Transparent Window
(define-key global-map "\e`" 'jcs-toggle-transparency)
(define-key global-map (kbd "C-=") 'jcs-increment-frame-transparent)
(define-key global-map (kbd "C--") 'jcs-decrement-frame-transparent)

;;; Comment/Uncomment
(define-key global-map (kbd "C-/") 'jcs-comment-uncomment-region-or-line)
(define-key global-map "\C-k\C-c" 'jcs-comment-region-or-line)
(define-key global-map "\C-k\C-u" 'jcs-uncomment-region-or-line)

;;; Canceling Action.
;;(define-key global-map "\C-g" 'jcs-top-level)
(define-key global-map (kbd "<escape>") 'jcs-top-level)

;;; Open same file in other window.
(define-key global-map (kbd "<f7>") 'jcs-find-file-other-window)
;; NOTE(jenchieh): If there are corresponding file, then
;; key <f8> should be replace by find corresponding file
;; interactive function call.
(define-key global-map (kbd "<f8>") 'jcs-find-file-other-window)

(define-key global-map (kbd "S-<home>") 'jcs-smart-select-home)
(define-key global-map (kbd "S-<end>") 'jcs-smart-select-end)
(define-key global-map (kbd "<up>") 'jcs-smart-indent-up)
(define-key global-map (kbd "<down>") 'jcs-smart-indent-down)
(define-key global-map [C-M-up] 'scroll-down-one-line)
(define-key global-map [C-M-down] 'scroll-up-one-line)

(define-key global-map "\er" 'revert-buffer-no-confirm)

;;; iedit (SEE: `jcs-mode-el' will have detail mode config.)
;; Fix "iedit" bug for OSX
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;;; Font
(define-key global-map (kbd "C-c f") 'jcs-change-font)

;;; Switch between window
(define-key global-map "\ew" 'jcs-other-window-next)
(define-key global-map "\eq" 'jcs-other-window-prev)

;;; Buffer Menu
(define-key global-map "\em" 'jcs-buffer-menu)
(define-key global-map "\eM" 'buffer-menu-other-window)

;;; Run
(define-key global-map "\e]" 'run-without-asking)        ;; ALT-]
(define-key global-map (kbd "<f5>") 'run-without-asking)

;;; Build
(define-key global-map (kbd "C-B") 'make-without-asking)

;;; Open TODO file.
(define-key global-map "\C-t" 'open-todo-without-asking)

;;; Open Log file.
(define-key global-map "\C-u" 'open-update-log-without-asking)

;;; switch line-ending key
(define-key global-map "\C-x\C-e" 'set-buffer-file-coding-system)

;;; Upper/Down case key binding.
(define-key global-map "\eu" 'upcase-word)
(define-key global-map "\ed" 'downcase-word)

;;; format file.
(define-key global-map "\C-k\C-f" 'indent-region)
(define-key global-map "\C-k\C-d" 'jcs-format-document)
(define-key global-map (kbd "C-S-f") 'jcs-format-region-or-document)
(define-key global-map "\C-xa" 'jcs-align-region-or-document)

;;; org-mode
(define-key global-map "\C-xo" 'org-mode)

;;; File editing
(define-key global-map "\ek" 'jcs-maybe-kill-this-buffer)
(define-key global-map "\eK" 'jcs-kill-this-buffer)
(define-key global-map [tab] 'jcs-tab-key)

;;; Process
(define-key global-map "\ep" 'list-processes)

;;; ace window
(require 'ace-window)
(define-key global-map "\ee" 'ace-window)

(require 'neotree)
;;(define-key global-map [f8] 'neotree-toggle)
(define-key global-map (kbd "C-M-l") 'neotree-toggle)

;; Interface in Emacs using Git.
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)

;;; Folding Settings
(outline-minor-mode t)      ; turn on the folding
(define-key global-map (kbd "C-M-o") 'hide-other)
(define-key global-map (kbd "C-M-p") 'show-all)

;;; Minimap
(define-key global-map "\C-cm" 'jcs-toggle-minimap)

;;; Animate Scrolling
(define-key global-map "\C-ca" 'jcs-toggle-sublimity-mode)

;;; Move Current Line Up or Down
(define-key global-map [M-up] 'jcs-move-line-up)
(define-key global-map [M-down] 'jcs-move-line-down)

;;; Buffer List
(define-key Buffer-menu-mode-map "1" 'jcs-buffer-menu-sort-by-visit)
(define-key Buffer-menu-mode-map "2" 'jcs-buffer-menu-sort-by-buffer)
(define-key Buffer-menu-mode-map "3" 'jcs-buffer-menu-sort-by-size)
(define-key Buffer-menu-mode-map "4" 'jcs-buffer-menu-sort-by-time)
(define-key Buffer-menu-mode-map "5" 'jcs-buffer-menu-sort-by-mode)
(define-key Buffer-menu-mode-map "6" 'jcs-buffer-menu-sort-by-file)

;;;
;; minibuffer
;;
(setq jcs-minibuffer-active nil)
(add-hook 'minibuffer-setup-hook
          (lambda ()

            ;; Active trigger flag.
            (setq jcs-minibuffer-active t)

            (if (and (not (looking-back "/"))
                     ;; SEE(jenchieh): this trigger can be check
                     ;; at `jcs-helm.el' file.
                     (eq jcs-helm-find-files-active t))
                (progn
                  ;; NOTE(jenchieh): This will prevent missing the
                  ;; slash at the end of the search file path.
                  (insert "/")
                  ))))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            ;; De-active trigger flag.
            (setq jcs-minibuffer-active nil)

            (jcs-reload-active-mode)
            ;; NOTE: disable the file after we do close minibuffer.
            (setq jcs-helm-find-files-active nil)
            ))

;;; Navigating Parentheses
;;; SOURCE: https://www.emacswiki.org/emacs/NavigatingParentheses
(define-key global-map (kbd "M-)") (quote jcs-move-forward-close-paren))
(define-key global-map (kbd "M-(") (quote jcs-move-backward-open-paren))
(define-key global-map (kbd "M-]") (quote jcs-move-forward-close-sqrParen))
(define-key global-map (kbd "M-[") (quote jcs-move-backward-open-sqrParen))
(define-key global-map (kbd "M-}") (quote jcs-move-forward-close-curlyParen))
(define-key global-map (kbd "M-{") (quote jcs-move-backward-open-curlyParen))

;; Navigating Single Quotation Mark.
(define-key global-map (kbd "M-'") (quote jcs-move-forward-single-quot))
(define-key global-map (kbd "M-;") (quote jcs-move-backward-single-quot))

;; Navigating Double Quotation Mark.
(define-key global-map "\e\"" (quote jcs-move-forward-double-quot))
(define-key global-map "\e:" (quote jcs-move-backward-double-quot))

;;; Web mode
(require 'impatient-mode)
(define-key global-map "\C-wo" 'jcs-httpd-start)
(define-key global-map "\C-wp" 'jcs-httpd-stop)

;;; Helm
;; More key binding in `jcs-helm.elq'


(defun jcs-global-key-rebind ()
  "Some key are not allow to bind, the solution here is just re-bind
the key everytime the mode changes."

  ;; Replace
  (define-key global-map (kbd "C-r C-r") 'iedit-mode)

  ;; Kill Buffer
  (define-key global-map (kbd "C-r DEL") 'jcs-backward-delete-current-char-repeat)
  (define-key global-map (kbd "C-r S-<backspace>") 'jcs-forward-delete-current-char-repeat)

  ;; re-builder
  (define-key global-map "\C-re" 'jcs-re-builder)
  )

;;------------------------------
;; ENABLE / DISABLE THE MODE
;;------------------------------
;;===========================
;; Compile lanauge!
;;===========================
;;(require 'c-mode)                       ;; C/C++
;;(require 'c++-mode)
(define-key global-map "\C-cc" 'jcs-toggle-cc-mode)
(require 'jdee)                         ;; Java
(define-key global-map "\C-cj" 'jdee-mode)

;;===========================
;; Scripting/Interpreter
;;===========================
(require 'php-mode)                     ;; PHP
(define-key global-map "\C-xp" 'php-mode)
(require 'web-mode)                     ;; html, css, js
(define-key global-map "\C-xw" 'web-mode)
(require 'js2-mode)                     ;; js
(define-key global-map "\C-xj" 'js2-mode)

;;===========================
;; Cross Language support
;;===========================
(require 'rainbow-mode)
(define-key global-map "\C-cr" 'rainbow-mode)
(require 'blank-mode)
(define-key global-map "\C-xb" 'blank-mode)
