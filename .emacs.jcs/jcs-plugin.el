;;; jcs-plugin.el --- Plugin Configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))


(use-package company
  :ensure t
  :config
  (use-package company-quickhelp
    :ensure t
    :config
    (setq company-quickhelp-delay 0.3)

    (setq company-quickhelp-color-background "#FFF08A")

    (company-quickhelp-mode t))

  ;; TOPIC(jenchieh): How add company-dabbrev to the Company completion popup?
  ;; URL(jenchieh): https://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
  (add-to-list 'company-backends '(company-capf :with company-dabbrev-code))

  ;; TOPIC(jenchieh): Switching from AC
  ;; URL(jenchieh): https://github.com/company-mode/company-mode/wiki/Switching-from-AC
  (defun jcs-company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-minimum-prefix-length 2)
    (setq company-idle-delay 0.2)
    ;;(setq company-tooltip-idle-delay 0.1)

    (setq company-selection-wrap-around 'on)

    (custom-set-faces
     '(company-preview
       ((t (:foreground "dark gray" :underline t))))
     '(company-preview-common
       ((t (:inherit company-preview))))
     '(company-tooltip
       ((t (:background "light gray" :foreground "black"))))
     '(company-tooltip-selection
       ((t (:background "steel blue" :foreground "white"))))
     '(company-tooltip-common
       ((((type x)) (:inherit company-tooltip :weight bold))
        (t (:inherit company-tooltip))))
     '(company-tooltip-common-selection
       ((((type x)) (:inherit company-tooltip-selection :weight bold))
        (t (:inherit company-tooltip-selection))))
     '(company-scrollbar-fg
       ((t (:background "black"))))
     '(company-scrollbar-bg
       ((t (:background "dark gray"))))))

  (jcs-company-ac-setup)
  (global-company-mode t))


(use-package flycheck
  :config
  (use-package flycheck-popup-tip
    :config
    ;;(flycheck-popup-tip-mode t)
    )
  ;;(global-flycheck-mode t)
  )


(use-package find-file-in-project
  :config
  (autoload 'find-file-in-project "find-file-in-project" nil t)
  (autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
  (autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
  (autoload 'ffip-show-diff "find-file-in-project" nil t)
  (autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
  (autoload 'ffip-ivy-resume "find-file-in-project" nil t)

  ;; You prefer ido-mode?
  ;;(setq ffip-prefer-ido-mode t)
  )


(require 'sublimity-scroll)
(require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)

(use-package sublimity
  :config
  ;; default on or off?
  ;; NOTE(jenchieh): This also trigger the animate scrolling too.
  (sublimity-mode 1)

  ;; Scroll Speed.
  (setq sublimity-scroll-weight 2  ;; [Default : 2]
        sublimity-scroll-drift-length 2)  ;; [Default : 2]


  (setq sublimity-map-size 0)  ;; [Default : 10]
  (setq sublimity-map-fraction 0.3)  ;; [Default : 0.3]
  (setq sublimity-map-text-scale -7)  ;; [Default: -7]

  ;; NOTE(jenchieh): When a positive integer is set, buffer
  ;; width is truncated to this value and drawn centered. To
  ;; cancel this feature, set this value nil.
  (setq sublimity-attractive-centering-width nil)  ;; [Default : 110]

  ;; NOTE(jenchieh): With the setting above, minimap is displayed
  ;; after 5 seconds of idle time. When sublimity-map-set-delay
  ;; is called with nil, then minimap is shown with no delay. This
  ;; defers from setting delay to 0, especially when used with
  ;; sublimity-scroll, in the sense that minimap looks not deleted
  ;; at all but gets worse performance.

  ;; ATTENTION(jenchieh): Set it to very hight so it will never
  ;; reach the timer error.
  (sublimity-map-set-delay 40000000)

  ;; NOTE(jenchieh): sublimity-map-setup-hook will run when
  ;; minimap is created.
  (add-hook 'sublimity-map-setup-hook
            (lambda ()
              (setq buffer-face-mode-face '(:family "Monospace"))
              (buffer-face-mode)))

  ;; NOTE(jenchieh): Following functions are available to hide
  ;; some UI parts.
  ;;(sublimity-attractive-hide-bars)
  ;;(sublimity-attractive-hide-vertical-border)
  ;;(sublimity-attractive-hide-fringes)
  ;;(sublimity-attractive-hide-modelines)
  )


;;; Uniquify
;; NOTE: meaningful names for buffers with the same name from
;; prelude.
;; SOURCE(jenchieh): http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )


;;; Preprocessor/Marcos highlight.
(use-package preproc-font-lock
  :config
  (preproc-font-lock-global-mode t)
  (preproc-font-lock-mode t)
  (set-face-attribute 'preproc-font-lock-preprocessor-background
                      nil
                      :background "#333333"
                      :inherit nil))


(use-package whitespace
  :config
  (autoload 'whitespace-mode "whitespace-mode" "Toggle whitespace visualization." t)
  (autoload 'whitespace-toggle-options "whitespace-mode" "Toggle local `whitespace-mode' options." t)
  ;; All the face can be find here.
  ;; URL: https://www.emacswiki.org/emacs/BlankMode
  (set-face-attribute 'whitespace-indentation
                      nil
                      :background "grey20"
                      :foreground "aquamarine3")
  (set-face-attribute 'whitespace-trailing
                      nil
                      :background "grey20"
                      :foreground "red"))


(use-package sr-speedbar
  :config
  ;;(setq sr-speedbar-auto-refresh nil)
  (setq speedbar-show-unknown-files t) ; show all files
  (setq speedbar-use-images nil) ; use text for buttons
  ;;(setq sr-speedbar-right-side nil) ; put on left side
  )


(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))


(use-package powerline
  :config
  (powerline-default-theme)
  ;;(powerline-center-theme)
  ;;(powerline-center-evil-theme)
  ;;(powerline-vim-theme)
  ;;(powerline-nano-theme)

  ;; NOTE(jenchieh):
  ;; The separator to use for the default theme.
  ;; Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
  ;; butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
  ;; utf-8.
  (setq powerline-default-separator 'wave))


(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)

  ;; Current highlight. (Cursor point currently on.)
  (custom-set-faces
   '(ahs-plugin-defalt-face ((t (:foreground nil :background "#123E70"))))
   '(ahs-face ((t (:foreground nil :background "#113D6F"))))
   '(ahs-definition-face ((t (:foreground nil :background "#113D6F"))))
   )

  ;; Current highlight. (Cursor point currently on.)
  (set-face-attribute 'ahs-plugin-defalt-face nil
                      :box '(:line-width -1 :color "#525D68" :style pressed-button)
                      ;;:underline nil
                      )

  ;; Other highlight. (Same words in the buffer)
  (set-face-attribute 'ahs-face nil
                      :box '(:line-width -1 :color "#525D68" :style pressed-button)
                      ;;:underline nil
                      )

  (set-face-attribute 'ahs-definition-face nil
                      :box '(:line-width -1 :color "#525D68" :style pressed-button)
                      ;;:underline nil
                      )

  ;; Number of seconds to wait before highlighting symbol.
  (custom-set-variables '(ahs-idle-interval 0.3)))


;;; Visual RegExp
(require 'visual-regexp)


(use-package which-key
  :config
  (which-key-mode)

  ;; Provide following type: `minibuffer', `side-window', `frame'.
  (setq which-key-popup-type 'side-window)

  ;; location of which-key window. valid values: top, bottom, left, right,
  ;; or a list of any of the two. If it's a list, which-key will always try
  ;; the first location first. It will go to the second location if there is
  ;; not enough room to display any keys in the first location
  (setq which-key-side-window-location 'bottom)

  ;; max width of which-key window, when displayed at left or right.
  ;; valid values: number of columns (integer), or percentage out of current
  ;; frame's width (float larger than 0 and smaller than 1)
  (setq which-key-side-window-max-width 0.33)

  ;; max height of which-key window, when displayed at top or bottom.
  ;; valid values: number of lines (integer), or percentage out of current
  ;; frame's height (float larger than 0 and smaller than 1)
  (setq which-key-side-window-max-height 0.25)

  ;; Set the time delay (in seconds) for the which-key popup to appear. A value of
  ;; zero might cause issues so a non-zero value is recommended.
  (setq which-key-idle-delay 1.0))


(use-package undo-tree
  :config
  ;; Enable `undo-tree' as default.
  (global-undo-tree-mode t))


(use-package line-reminder
  :config
  (global-line-reminder-mode t))


(use-package tabbar
  :config
  ;; Turn-off `tabbar-mode' as default.
  (tabbar-mode 0)  )


(use-package indent-info
  :config
  (global-indent-info-mode +1))


(use-package right-click-context
  :config
  (right-click-context-mode 1))


(use-package goto-line-preview
  :ensure t
  :config
  (defun jcs-advice-goto-line-preview-after ()
    "Advice after execute `goto-line-preview' command."
    (call-interactively #'recenter))
  (advice-add 'goto-line-preview :after #'jcs-advice-goto-line-preview-after))


(use-package dimmer
  :config
  (dimmer-mode)
  (setq dimmer-fraction 0.2))


(use-package reload-emacs
  :config
  (defun jcs-advice-reload-emacs-after ()
    "Advice after execute `reload-emacs' command."
    ;; Split window horizontally if full width.
    (when (and (window-full-width-p)
               (= (length (window-list)) 1))
      (jcs-balance-split-window-horizontally))

    ;; Restore to what ever state it was.
    ;;
    ;; NOTE(jenchieh): we need these two lines because we need it
    ;; for solving after reloading Emacs, there are some space at
    ;; the bottom. Which is weird and I have no idea why...
    (toggle-frame-maximized)
    (toggle-frame-maximized)

    ;; When frame not maximize we make sure it maximized.
    (unless (jcs-is-frame-maximize-p)
      (toggle-frame-maximized)))
  (advice-add 'reload-emacs :after #'jcs-advice-reload-emacs-after))


(require 'helm)

(use-package helm-config
  :config
  ;; 相關教學:
  ;; * http://emacsist.com/10295

  (helm-mode 1)
  (helm-autoresize-mode 1)

  ;;; Helm Key bindings
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  ;;(setq helm-ff-auto-update-initial-value nil)    ; 禁止自動補全

  (define-key global-map [f12] 'jcs-helm-gtags-to-def-dec)
  (define-key global-map [S-f12] 'jcs-helm-gtags-to-def-dec-other-window)

  ;; Helm minibuffer key bindings
  (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)


  ;; Helm search configuration.
  (setq helm-split-window-in-side-p           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-M-x-fuzzy-match                  t   ; 模糊搜索
        helm-buffers-fuzzy-matching           t
        helm-locate-fuzzy-match               t
        helm-recentf-fuzzy-match              t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t)


  ;; NOTE(jenchieh): Make Helm window at the bottom WITHOUT
  ;; using any extra package.
  ;; SOURCE(jenchieh): https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))

  ;;
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

  ;;
  ;; Customize Helm Themes
  ;;

  ;; Title
  (set-face-attribute 'helm-source-header nil
                      :background "#161616"
                      :foreground "steel blue")
  ;; Selection
  (set-face-attribute 'helm-selection nil
                      :background "midnight blue"
                      :foreground "#40FF40"))

;;;
;; NOTE(jenchieh): You will need GNU GLOBAL executable in order
;; to make the tag system work.
;;
(use-package helm-gtags
  :config
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
   '(helm-gtags-auto-update t)))


(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  (yas-global-mode 1))


(use-package origami
  :config
  (global-origami-mode t))


(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
