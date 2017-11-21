;; This is the start of jcs-global-key.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-global-key.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2017-03-17 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-global-key is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-global-key is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;================================================
;; JayCeS Global Key
;;================================================

;;; bind the key
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
(define-key global-map "\C-s" 'jcs-save-buffer)

(define-key global-map "\C-p" 'package-list-packages)
(define-key global-map "\e=" 'text-scale-increase)
(define-key global-map "\e-" 'text-scale-decrease)
(define-key global-map "\C-t" 'transient-mark-mode)
(define-key global-map [home] 'back-to-indentation-or-beginning)

(define-key global-map (kbd "C-<backspace>") 'jcs-backward-delete-word)
(define-key global-map (kbd "C-M-<backspace>") 'jcs-backward-kill-word-capital)
(define-key global-map (kbd "C-M-<left>") 'jcs-backward-capital-char)
(define-key global-map (kbd "C-M-<right>") 'jcs-forward-capital-char)

;;(define-key global-map "\em" 'describe-mode)

(define-key global-map (kbd "C-<return>") 'goto-address-at-point)

;;; Admin
(define-key global-map "\C-x\C-v" 'jcs-reload-emacs)

;;; Undo and Redo
(define-key global-map "\C-a" 'mark-whole-buffer)
(define-key global-map "\C-z" 'undo)
;;(define-key global-map "\C-y" 'redo)

;;; Search Word (SEE: `jcs-mode-el' will have detail mode config.)ee
;;(define-key global-map "\C-f" 'helm-do-ag-this-file)
;;(define-key global-map "\C-f" 'isearch-forward)
(define-key global-map "\C-r\C-f" 'isearch-backward-regexp)

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

;; Open same file in other window.
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
(define-key global-map (kbd "C-r C-r") 'iedit-mode)

;;; Switch between window
(define-key global-map "\ew" 'jcs-other-window-next)
(define-key global-map "\eq" 'jcs-other-window-prev)

;;; Buffer Menu
(define-key global-map "\em" 'buffer-menu)
(define-key global-map "\eM" 'buffer-menu-other-window)

;;; Run
(define-key global-map "\e]" 'run-without-asking)        ;; ALT-]
(define-key global-map (kbd "<f5>") 'run-without-asking)

;;; Build
(define-key global-map (kbd "C-B") 'make-without-asking)

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
(define-key global-map "\eK" 'kill-this-buffer)

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
                  )
              )
            ))

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


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-global-key.el file
