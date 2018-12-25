;; ========================================================================
;; $File: jcs-plugin.el $
;; $Date: 2017-05-29 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2017 by Shen, Jen-Chieh $
;; ========================================================================


;;================================================
;; JayCeS Environment Settings.
;;================================================

;;===========================
;; Find file in project
;;----------------------

(require 'find-file-in-project)

(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
(autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
(autoload 'ffip-ivy-resume "find-file-in-project" nil t)

;; You prefer ido-mode?
;;(setq ffip-prefer-ido-mode t)


;;===========================
;; Sublimity
;;----------------------
;; URL(jenchieh): https://github.com/zk-phi/sublimity
(require 'sublimity)
(require 'sublimity-scroll)

;; default on or off?
;; NOTE(jenchieh): This also trigger the animate scrolling too.
(sublimity-mode 1)

;; Scroll Speed.
(setq sublimity-scroll-weight 2  ;; [Default : 2]
      sublimity-scroll-drift-length 2)  ;; [Default : 2]

(require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)

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

;;; Uniquify
;; NOTE: meaningful names for buffers with the same name from
;; prelude.
;; SOURCE(jenchieh): http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
;; URL: https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;;; Preprocessor/Marcos highlight.
(require 'preproc-font-lock)
(preproc-font-lock-global-mode t)
(preproc-font-lock-mode t)
(set-face-attribute 'preproc-font-lock-preprocessor-background
                    nil
                    :background "#333333"
                    :inherit nil)


;;; Blank Mode
(require 'whitespace)
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
                    :foreground "red")


;;; Speedbar
(require 'sr-speedbar)
;;(setq sr-speedbar-auto-refresh nil)
(setq speedbar-show-unknown-files t) ; show all files
(setq speedbar-use-images nil) ; use text for buttons
;;(setq sr-speedbar-right-side nil) ; put on left side

;;; Execute path from shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;;; Powerline
(require 'powerline)
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
(setq powerline-default-separator 'wave)

;;; Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;; Auto Highlight symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

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
(custom-set-variables '(ahs-idle-interval 0.3))

;;; Flycheck
(require 'flycheck)
;;(global-flycheck-mode t)  ;; Enable global `flycheck'?

;;; Flymake
(require 'flymake)

;;; Helm
(require 'helm)

;;; Visual RegExp
(require 'visual-regexp)

;;; Which Key
(require 'which-key)
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
(setq which-key-idle-delay 1.0)

;;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode t)  ;; Enable `undo-tree' as default.

;;; Line Reminder
(require 'line-reminder)
(global-line-reminder-mode t)

;;; Tabbar
(require 'tabbar)
(tabbar-mode 0)  ;; Turn-off `tabbar-mode' as default.

;;; Indent Info
(require 'indent-info)
(global-indent-info-mode +1)

;;; Right Click Context
(require 'right-click-context)
(right-click-context-mode 1)
