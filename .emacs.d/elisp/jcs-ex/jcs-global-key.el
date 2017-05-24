;; This is the start of jcs-file-info-format.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-global-key.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2017-03-17 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-file-info-format is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-file-info-format is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;================================================
;; JayCeS Global Key
;;================================================

;;; unbind the key
(global-unset-key "\C-k")

;;; bind the key
;;(global-set-key "\ef" 'ido-find-file)
;;(global-set-key "\eF" 'ido-find-file-other-window)
(global-set-key "\ef" 'jcs-helm-find-files)
(global-set-key "\eF" 'jcs-helm-find-files-other-window)
(global-set-key "\C-xf" 'jcs-smart-find-file-in-project)
(global-set-key "\C-xF" 'jcs-smart-find-file-in-project-in-another-window)

(global-set-key "\C-cd" 'duplicate-line)
;;(global-set-key (kbd "C-d") 'kill-whole-line)   ;; Emacs default version
(define-key global-map (kbd "C-d") 'jcs-kill-whole-line)    ;; delete the line without copying!!
(define-key global-map "\C-x\C-x" 'kill-region)
(define-key global-map "\C-c\C-c" 'kill-ring-save)
(global-set-key "\C-v" 'yank)
(global-set-key "\C-s" 'jcs-save-buffer)
(global-set-key "\C-f" 'isearch-forward)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key "\C-z" 'undo)

(global-set-key "\C-p" 'package-list-packages)
(global-set-key "\e=" 'text-scale-increase)
(global-set-key "\e-" 'text-scale-decrease)
(global-set-key (kbd "C-<backspace>") 'my-backward-delete-word)
(global-set-key "\C-t" 'transient-mark-mode)
(define-key global-map [home] 'back-to-indentation-or-beginning)

(global-set-key "\em" 'describe-mode)
(global-set-key "\C-cd" 'jcs-toggle-shell-window) ; shell command

(global-set-key "\C-xn" 'jcs-new-window)
(global-set-key "\C-xd" 'delete-frame)  ; delete the external window


;;; Mode toggle
;;(global-set-key "\e`" 'jcs-mode-toggle)

;;; Rename file
(define-key global-map "\C-xr" 'jcs-rename-current-buffer-file)

;;; Transparent Window
(global-set-key "\e`" 'jcs-toggle-transparency)
(define-key global-map (kbd "C-=") 'jcs-increment-frame-transparent)
(define-key global-map (kbd "C--") 'jcs-decrement-frame-transparent)

;;; Comment/Uncomment
(global-set-key (kbd "C-/") 'jcs-comment-uncomment-region-or-line)
(global-set-key "\C-k\C-c" 'jcs-comment-region-or-line)
(global-set-key "\C-k\C-u" 'jcs-uncomment-region-or-line)

;;; canceling action.
(global-set-key "\C-g" 'jcs-top-level)

;; Open same file in other window.
(global-set-key (kbd "<f8>") 'jcs-find-file-other-window)

(define-key global-map (kbd "S-<home>") 'jcs-smart-select-home)
(define-key global-map (kbd "S-<end>") 'jcs-smart-select-end)
(global-set-key (kbd "<up>") 'jcs-smart-indent-up)
(global-set-key (kbd "<down>") 'jcs-smart-indent-down)
(define-key global-map [M-up] 'scroll-down-one-line)
(define-key global-map [M-down] 'scroll-up-one-line)

(global-set-key "\er" 'revert-buffer-no-confirm)

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
(global-set-key "\C-x\C-e" 'set-buffer-file-coding-system)

;;; Upper/Down case key binding.
(define-key global-map "\eu" 'upcase-word)
(define-key global-map "\ed" 'downcase-word)

;;; format file.
(global-set-key "\C-k\C-f" 'indent-region)
(global-set-key "\C-k\C-d" 'jcs-format-document)
(define-key global-map "\C-xa" 'align)
(define-key global-map "\C-x\C-a" 'jcs-align-document)

;;; org-mode
(define-key global-map "\C-xo" 'org-mode)

;;; File editing
(define-key global-map "\ek" 'jcs-maybe-kill-this-buffer)
(define-key global-map "\eK" 'kill-this-buffer)

;;; ace window
(require 'ace-window)
(define-key global-map "\ee" 'ace-window)

(require 'zencoding-mode)
(define-key global-map "\C-xz" 'zencoding-mode)

;;; Blank Mode
(require 'blank-mode)
(autoload 'blank-mode           "blank-mode" "Toggle blank visualization."        t)
(autoload 'blank-toggle-options "blank-mode" "Toggle local `blank-mode' options." t)

;;; Neo Tree
(require 'neotree)
(setq neo-window-position t)              ;; set window to the right
(setq neo-smart-open t)

;;(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-M-l") 'neotree-toggle)

;;; Folding Settings
(outline-minor-mode t)      ; turn on the folding
(global-set-key (kbd "C-M-o") 'hide-other)
(global-set-key (kbd "C-M-p") 'show-all)

;;; Minimap
(define-key global-map "\C-cm" 'jcs-toggle-minimap)

;;; Animate Scrolling
(define-key global-map "\C-ca" 'sublimity-mode)

;;;
;; minibuffer
;;
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (local-set-key (kbd "\C-g") 'jcs-top-level)))


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
(global-set-key "\C-cj" 'jdee-mode)

;;===========================
;; Scripting/Interpreter
;;===========================
(require 'php-mode)                     ;; PHP
(global-set-key "\C-xp" 'php-mode)
(require 'web-mode)                     ;; html, css, js
(global-set-key "\C-xw" 'web-mode)
(require 'js2-mode)                     ;; js
(global-set-key "\C-xj" 'js2-mode)

;;===========================
;; Cross Language support
;;===========================
(require 'rainbow-mode)
(global-set-key "\C-cr" 'rainbow-mode)
(require 'blank-mode)
(global-set-key "\C-xb" 'blank-mode)



;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
