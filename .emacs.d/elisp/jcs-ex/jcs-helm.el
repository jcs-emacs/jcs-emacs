;; ========================================================================
;; $File: jcs-helm.el $
;; $Date: 2017-03-17 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;================================================
;; JayCeS Helm
;;================================================

;; 相關教學:
;; * http://emacsist.com/10295

(require 'helm-config)

(helm-mode 1)
(helm-autoresize-mode 1)

;;; Helm Key bindings
(require 'helm-config)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
;;(setq helm-ff-auto-update-initial-value nil)    ; 禁止自動補全

(define-key global-map [f12] 'jcs-helm-gtags-to-def-dec)
(define-key global-map [S-f12] 'jcs-helm-gtags-to-def-dec-other-window)

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
        (helm-maybe-exit-minibuffer))
    (apply orig-fun args)))
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
;; `helm-find-files-hook'
;;
(setq jcs-helm-find-files-active nil)

;;;###autoload
(defun jcs-helm-find-files-hook ()
  "Hook after `helm-find-files' initialized."
  (interactive)

  ;; SEE(jenchieh): `jcs-global-key.el' file,
  ;; and `minibuffer-setup-hook'.
  (setq jcs-helm-find-files-active t)
  )
(add-hook 'helm-find-files-after-init-hook 'jcs-helm-find-files-hook)

;;;
;; `helm-colors'
;;
;; NOTE(jenchieh): make key insert 'HEX' and 'Name'
;;
(defvar helm-color-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "RET") 'helm-color-run-insert-name)
    (define-key map (kbd "C-c N") 'helm-color-run-kill-name)
    (define-key map (kbd "M-RET") 'helm-color-run-insert-rgb)
    (define-key map (kbd "C-c R") 'helm-color-run-kill-rgb)
    map))

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
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'jayces-mode-hook 'helm-gtags-mode)
(add-hook 'js2-mode-hook 'helm-gtags-mode)
(add-hook 'lua-mode-hook 'helm-gtags-mode)
(add-hook 'nasm-mode-hook 'helm-gtags-mode)

;; customize 'helm-gtags' plugin
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))
