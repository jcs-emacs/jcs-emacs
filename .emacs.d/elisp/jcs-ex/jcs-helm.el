;; This is the start of jcs-file-info-format.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-heml.el             -*- Emacs-Lisp -*-

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
;; JayCeS Helm
;;================================================

;; 相關教學:
;; * http://emacsist.com/10295

(require 'helm-config)

(helm-mode 1)
(helm-autoresize-mode 1)
;;(setq helm-ff-auto-update-initial-value nil)    ; 禁止自動補全

;;;
;; Helm Key bindings
;;
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key [f12] 'jcs-helm-gtags-to-def-dec)
(global-set-key [S-f12] 'jcs-helm-gtags-to-def-dec-other-window)

;;;
;; Helm minibuffer key bindings
;;
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)

;;;
;; Helm search configuration.
;;
(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-M-x-fuzzy-match                  t   ; 模糊搜索
      helm-buffers-fuzzy-matching           t
      helm-locate-fuzzy-match               t
      helm-recentf-fuzzy-match              t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)

;;;
;; Helm Functions
;;

;; TOPIC(jenchieh): How do I make pressing <RET> in helm-find-files open the directory?
;; SOURCE(jenchieh): http://emacs.stackexchange.com/questions/3798/how-do-i-make-pressing-ret-in-helm-find-files-open-the-directory
;;;###autoload
(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (and (equal "Find Files" (assoc-default 'name (helm-get-current-source)))
           (equal args nil)
           (stringp (helm-get-selection))
           (not (file-directory-p (helm-get-selection))))
      (progn

        (jcs-helm-execute-persistent-action)

        (helm-maybe-exit-minibuffer)
        )
    (apply orig-fun args)
    )
  )
(advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)

;;;###autoload
(defun fu/helm-find-files-navigate-back (orig-fun &rest args)
  (if (looking-back "/")
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)

;;;
;; Add a function call before the helm window is active.
;;
(add-hook 'helm-before-initialize-hook 'jcs-helm-before-initialize-hook)

;;;
;; NOTE(jenchieh): Make Helm window at the bottom WITHOUT
;; using any extra package.
;; SOURCE(jenchieh): https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))


;;;
;; Customize Helm Themes
;;
;; Title
(set-face-attribute 'helm-source-header nil
                    :background "#161616"
                    :foreground "steel blue")
;; Selection
(set-face-attribute 'helm-selection nil
                    :background "midnight blue"
                    :foreground "#40FF40")

;;;
;; NOTE(jenchieh): You will need GNU GLOBAL executable in order
;; to make the tag system work.
;;
(require 'helm-gtags)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize 'helm-gtags' plugin
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-file-info-format.el file
