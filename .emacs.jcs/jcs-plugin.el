;;; jcs-plugin.el --- Plugin Configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package auto-highlight-symbol
  :defer t
  :init
  ;; Number of seconds to wait before highlighting symbol.
  (setq ahs-idle-interval 0.3)
  :config
  ;; Current highlight. (Cursor point currently on.)
  (set-face-attribute 'ahs-plugin-defalt-face nil
                      :foreground nil
                      :background "#123E70"
                      :box '(:line-width -1 :color "#525D68" :style pressed-button)
                      :underline nil)
  ;; Other highlight. (Same words in the buffer)
  (set-face-attribute 'ahs-face nil
                      :foreground nil
                      :background "#113D6F"
                      :box '(:line-width -1 :color "#525D68" :style pressed-button)
                      :underline nil)
  (set-face-attribute 'ahs-definition-face nil
                      :foreground nil
                      :background "#113D6F"
                      :box '(:line-width -1 :color "#525D68" :style pressed-button)
                      :underline nil))


(use-package centaur-tabs
  :defer t
  :init
  (setq centaur-tabs-set-icons nil)
  (setq centaur-tabs-style "wave")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*"))


(use-package company
  :defer t
  :config
  ;; TOPIC: How add company-dabbrev to the Company completion popup?
  ;; URL: https://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
  (add-to-list 'company-backends '(company-capf :with company-dabbrev-code))

  ;; TOPIC: Switching from AC
  ;; URL: https://github.com/company-mode/company-mode/wiki/Switching-from-AC
  (defun jcs-company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-minimum-prefix-length 2)
    (setq company-idle-delay 0.1)
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

(use-package company-quickhelp
  :defer t
  :init
  (setq company-quickhelp-delay 0.3)
  (setq company-quickhelp-color-background "#FFF08A")
  (with-eval-after-load 'company
    (company-quickhelp-mode t)))

(use-package company-statistics
  :defer t
  :init
  (with-eval-after-load 'company
    (company-statistics-mode 1)))


(use-package dashboard
  :defer t
  :init
  (setq dashboard-banner-logo-title "[J C S • E M A C S]")
  (setq dashboard-footer-icon "")
  (setq dashboard-footer "╬ Copyright © 2015 Shen, Jen-Chieh ╬")
  (setq dashboard-init-info (format "%d + %d packages loaded in %0.1f seconds"
                                    (length package-activated-list)
                                    (length jcs-package-manually-install-list)
                                    (string-to-number jcs-package-init-time)))
  (setq dashboard-items '((recents  . 10)
                          (projects . 10)
                          ;;(bookmarks . 10)
                          ;;(agenda . 10)
                          ;;(registers . 10)
                          ))
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-navigator nil)
  :config
  (defun jcs-dashboard-get-banner-path (&rest _)
    "Return the full path to banner."
    "~/.emacs.jcs/banner/sink.txt")
  (advice-add #'dashboard-get-banner-path :override #'jcs-dashboard-get-banner-path)
  (dashboard-setup-startup-hook))


(use-package diminish
  :defer t
  :config
  (diminish 'abbrev-mode)
  (with-eval-after-load 'alt-codes (diminish 'alt-codes-mode))
  (diminish 'auto-fill-mode)
  (with-eval-after-load 'auto-highlight-symbol (diminish 'auto-highlight-symbol-mode))
  (with-eval-after-load 'auto-rename-tag (diminish 'auto-rename-tag-mode))
  (with-eval-after-load 'beacon (diminish 'beacon-mode))
  (with-eval-after-load 'company (diminish 'company-mode))
  (diminish 'eldoc-mode)
  (with-eval-after-load 'face-remap (diminish 'buffer-face-mode))
  (with-eval-after-load 'flycheck
    (diminish 'flycheck-mode)
    (require 'flycheck-popup-tip)
    (flycheck-popup-tip-mode t))
  (with-eval-after-load 'helm-mode (diminish 'helm-mode))
  (with-eval-after-load 'highlight-indent-guides (diminish 'highlight-indent-guides-mode))
  (with-eval-after-load 'impatient-mode (diminish 'impatient-mode))
  (with-eval-after-load 'line-reminder (diminish 'line-reminder-mode))
  (with-eval-after-load 'indicators (diminish 'indicators-mode))
  (diminish 'outline-minor-mode)
  (diminish 'overwrite-mode)
  (with-eval-after-load 'page-break-lines (diminish 'page-break-lines-mode))
  (with-eval-after-load 'projectile (diminish 'projectile-mode))
  (with-eval-after-load 'right-click-context (diminish 'right-click-context-mode))
  (with-eval-after-load 'shift-select (diminish 'shift-select-minor-mode))
  (with-eval-after-load 'show-eol (diminish 'show-eol-mode))
  (with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
  (with-eval-after-load 'view (diminish 'view-mode))
  (with-eval-after-load 'which-key (diminish 'which-key-mode))
  (with-eval-after-load 'whitespace
    (diminish 'whitespace-mode)
    (diminish 'whitespace-newline-mode)
    (diminish 'global-whitespace-mode)
    (diminish 'global-whitespace-newline-mode))
  (with-eval-after-load 'yasnippet (diminish 'yas-minor-mode)))


(use-package dimmer
  :defer t
  :init
  (setq dimmer-fraction 0.2))


(use-package dumb-jump
  :defer t
  :init
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-force-searcher 'ag))


(use-package eww
  :defer t
  :init
  (setq eww-search-prefix "https://www.google.com/search?q="))


(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package feebleline
  :defer t
  :config
  (setq feebleline-msg-functions
        '(;;-- Left
          (jcs-current-major-mode :pre " [" :face font-lock-constant-face :post "]")
          ((lambda ()
             (let ((project-root (cdr (project-current))))
               (if (and project-root
                        (buffer-file-name))
                   (concat " { " (file-name-nondirectory (directory-file-name project-root)) " }")
                 " { ╬ }"))))
          ((lambda () "-"))
          (feebleline-file-modified-star :pre "" :face font-lock-constant-face :post " ")
          (buffer-name :pre "" :face font-lock-keyword-face :post " ")
          ;;-- Right
          (feebleline-line-number :pre "[ " :fmt "%s" :post "" :align right)
          (feebleline-column-number :pre " : " :fmt "%s" :post " ] " :align right)
          ((lambda () (format-time-string "[%Y-%m-%d %H:%M:%S]")) :align right)
          ))

  (defun jcs-advice-feebleline-mode-after (&rest _)
    "Advice after execute `feebleline-mode'."
    (if feebleline-mode
        (jcs-walk-through-all-windows-once
         (lambda ()
           (setq mode-line-format nil)))
      (jcs-walk-through-all-windows-once
       (lambda ()
         (setq mode-line-format feebleline--mode-line-format-previous)))))
  (advice-add 'feebleline-mode :after #'jcs-advice-feebleline-mode-after))


(use-package goto-char-preview
  :defer t
  :config
  (defun jcs-advice-goto-char-preview-after ()
    "Advice after execute `goto-char-preview' command."
    (call-interactively #'recenter))
  (advice-add 'goto-char-preview :after #'jcs-advice-goto-char-preview-after))

(use-package goto-line-preview
  :defer t
  :config
  (defun jcs-advice-goto-line-preview-after ()
    "Advice after execute `goto-line-preview' command."
    (call-interactively #'recenter))
  (advice-add 'goto-line-preview :after #'jcs-advice-goto-line-preview-after))


(use-package helm
  :defer t
  :init
  ;; 禁止自動補全
  ;;(setq helm-ff-auto-update-initial-value nil)

  ;; Helm search configuration.
  (setq helm-split-window-inside-p            t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-M-x-fuzzy-match                  t   ; 模糊搜索
        helm-buffers-fuzzy-matching           t
        helm-locate-fuzzy-match               t
        helm-recentf-fuzzy-match              t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t)

  ;; NOTE: Make Helm window at the bottom WITHOUT using any extra package.
  ;; SOURCE: https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))

  (with-eval-after-load 'helm-color
    ;; `helm-colors'
    ;; NOTE: make key insert 'HEX' and 'Name'
    (defvar helm-color-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (define-key map (kbd "RET") 'helm-color-run-insert-name)
        (define-key map (kbd "C-c N") 'helm-color-run-kill-name)
        (define-key map (kbd "M-RET") 'helm-color-run-insert-rgb)
        (define-key map (kbd "C-c R") 'helm-color-run-kill-rgb)
        map))))

(use-package helm-ag
  :defer t
  :config
  (setq helm-source-do-ag
        (helm-build-async-source "The Silver Searcher"
          :init 'helm-ag--do-ag-set-command
          :candidates-process 'helm-ag--do-ag-candidate-process
          :persistent-action  'helm-ag--persistent-action
          :action helm-ag--actions
          :nohighlight t
          :requires-pattern 2
          :candidate-number-limit 9999
          :keymap helm-do-ag-map
          :follow (and helm-follow-mode-persistent 1))))

(use-package helm-file-preview
  :defer t
  :init
  (with-eval-after-load 'helm
    (helm-file-preview-mode 1)))


(use-package highlight-indent-guides
  :defer t
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  :config
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))


(use-package hl-todo
  :defer t
  :init
  (setq hl-todo-keyword-faces
        '(("HOLD" . "#d0bf8f")
          ("TODO" . "red")
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . "#7cb8bb")
          ("OKAY" . "#7cb8bb")
          ("DONT" . "#5f7f5f")
          ("FAIL" . "#8c5353")
          ("DONE" . "#afd8af")
          ("NOTE"   . "dark green")
          ("KLUDGE" . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("TEMP"   . "turquoise")
          ("FIXME"  . "red")
          ("XXX+"   . "#cc9393")
          ("\\?\\?\\?+" . "#cc9393")

          ("ATTENTION" . "red")
          ("STUDY" . "yellow")
          ("IMPORTANT" . "yellow")
          ("CAUTION" . "yellow")
          ("OPTIMIZE" . "yellow")
          ("DESCRIPTION" . "dark green")
          ("TAG" . "dark green")
          ("OPTION" . "dark green")
          ("DEBUG" . "turquoise")
          ("DEBUGGING" . "turquoise")
          ("TEMPORARY" . "turquoise")
          ("SOURCE" . "PaleTurquoise2")
          ("URL" . "PaleTurquoise2")
          ("IDEA" . "green yellow")
          ("OR" . "green yellow")
          ("OBSOLETE" . "DarkOrange3")
          ("DEPRECATED" . "DarkOrange3")
          ("TOPIC" . "slate blue")
          ("SEE" . "slate blue")
          ))
  :config
  (defun jcs--hl-todo--inside-comment-or-string-p ()
    "Redefine `hl-todo--inside-comment-or-string-p', for accurate highlighting."
    (jcs-inside-comment-or-string-p))
  (advice-add #'hl-todo--inside-comment-or-string-p :override #'jcs--hl-todo--inside-comment-or-string-p))


(use-package isearch
  :defer t
  :config
  (defun jcs-isearch-mode-hook ()
    "Paste the current symbol when `isearch' enabled."
    (cond ((use-region-p)
           (progn
             (deactivate-mark)
             (ignore-errors
               (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))))
          ((memq this-command '(jcs-isearch-project-backward-symbol-at-point))
           (when (char-or-string-p isearch-project-thing-at-point)
             (backward-word 1)
             (isearch-project-isearch-yank-string isearch-project-thing-at-point)
             (isearch-repeat-backward)))))
  (add-hook 'isearch-mode-hook #'jcs-isearch-mode-hook))

(use-package isearch-project
  :defer t
  :init
  (setq isearch-project-ignore-paths '(".vs/"
                                       ".vscode/"
                                       "bin/"
                                       "build/"
                                       "build.min/"
                                       "node_modules/"
                                       "res/")))


(use-package line-reminder
  :defer t
  :init
  (setq line-reminder-show-option (if (display-graphic-p) 'indicators 'linum)))


(use-package multiple-cursors
  :defer t
  :init
  (defun jcs-mc/-cancel-multiple-cursors ()
    "Cancel the `multiple-cursors' behaviour."
    (require 'multiple-cursors)
    (when (> (mc/num-cursors) 1)
      (mc/keyboard-quit)))
  (advice-add 'jcs-previous-blank-line :after #'jcs-mc/-cancel-multiple-cursors)
  (advice-add 'jcs-next-blank-line :after #'jcs-mc/-cancel-multiple-cursors))


(use-package origami
  :defer t
  :config
  (global-origami-mode t))


(use-package popup
  :defer t
  :config
  (defvar jcs-popup-mouse-events-flag nil
    "Check if `popup-menu-item-of-mouse-event' is called.")
  (defvar jcs-popup-selected-item-flag nil
    "Check if `popup-selected-item' is called.")

  (defun jcs-popup-clicked-on-menu-p ()
    "Check if the user actually clicked on the `popup' object."
    (and jcs-popup-mouse-events-flag
         (not jcs-popup-selected-item-flag)))

  (defun jcs-advice-popup-menu-item-of-mouse-event-after (event)
    "Advice after execute `popup-menu-item-of-mouse-event' command."
    (setq jcs-popup-mouse-events-flag t)
    (setq jcs-popup-selected-item-flag nil))
  (advice-add 'popup-menu-item-of-mouse-event :after #'jcs-advice-popup-menu-item-of-mouse-event-after)

  (defun jcs-advice-popup-selected-item-after (popup)
    "Advice after execute `popup-selected-item' command."
    (setq jcs-popup-selected-item-flag t)
    (setq jcs-popup-selected-item-flag (jcs-last-input-event-p "mouse-1")))
  (advice-add 'popup-selected-item :after #'jcs-advice-popup-selected-item-after)

  (defun jcs-advice-popup-select-around (orig-fun &rest args)
    "Advice around execute `popup-draw' command."
    (let ((do-orig-fun t))
      (when (and (jcs-last-input-event-p "mouse-1")
                 (not (jcs-popup-clicked-on-menu-p)))
        (keyboard-quit)
        (setq do-orig-fun nil))
      (when do-orig-fun
        (apply orig-fun args))))
  (advice-add 'popup-draw :around #'jcs-advice-popup-select-around))


(use-package powerline
  :defer t
  :init
  ;; NOTE:
  ;; The separator to use for the default theme.
  ;;
  ;; Valid Values: alternate, arrow, arrow-fade, bar, box,
  ;; brace, butt, chamfer, contour, curve, rounded, roundstub,
  ;; wave, zigzag, utf-8.
  (setq powerline-default-separator 'wave))


(use-package preproc-font-lock
  :defer t
  :config
  (set-face-attribute 'preproc-font-lock-preprocessor-background
                      nil
                      :background nil
                      :inherit nil))


(use-package projectile
  :defer t
  :init
  (setq projectile-current-project-on-switch 'keep)
  :config
  (add-hook 'projectile-after-switch-project-hook #'jcs-dashboard-refresh-buffer))


(use-package region-occurrences-highlighter
  :defer t
  :init
  (setq region-occurrences-highlighter-min-size 1)
  :config
  (set-face-attribute 'region-occurrences-highlighter-face
                      nil
                      :background "#113D6F"
                      :inverse-video nil)
  (add-hook 'prog-mode-hook #'region-occurrences-highlighter-mode))


(use-package reload-emacs
  :defer t
  :init
  (setq reload-emacs-load-path '("~/.emacs.jcs/"
                                 "~/.emacs.jcs/func/"
                                 "~/.emacs.jcs/mode/"))
  :config
  (defun jcs-advice-reload-emacs-after ()
    "Advice after execute `reload-emacs' command."
    ;; Split window horizontally if full width.
    (when (and (window-full-width-p)
               (= (length (window-list)) 1))
      (jcs-balance-split-window-horizontally))

    ;; Restore to what ever state it was.
    ;;
    ;; NOTE: we need these two lines because we need it for
    ;; solving after reloading Emacs, there are some space at
    ;; the bottom. Which is weird and I have no idea why...
    (toggle-frame-maximized)
    (toggle-frame-maximized)

    ;; When frame not maximize we make sure it maximized.
    (unless (jcs-is-frame-maximize-p)
      (toggle-frame-maximized))

    ;; Refresh `dashboard' buffer if is on `dashboard' buffer.
    (when (or (jcs-is-current-major-mode-p "dashboard-mode"))
      (let ((prev-pt (point)))
        (save-excursion
          (dashboard-refresh-buffer))
        (goto-char prev-pt))))
  (advice-add 'reload-emacs :after #'jcs-advice-reload-emacs-after))


(use-package right-click-context
  :defer t
  :config
  ;;;###autoload
  (defun right-click-context-menu ()
    "Open Right Click Context menu."
    (interactive)
    (let ((popup-menu-keymap (copy-sequence popup-menu-keymap)))
      (define-key popup-menu-keymap [mouse-3] #'right-click-context--click-menu-popup)
      (let ((value (popup-cascade-menu (right-click-context--build-menu-for-popup-el (right-click-context--menu-tree) nil))))
        (when (and (jcs-popup-clicked-on-menu-p)
                   value)
          (if (symbolp value)
              (call-interactively value t)
            (eval value)))))))


(use-package shift-select
  :defer t
  :config
  (defun jcs-advice-shift-select-pre-command-hook-after ()
    "Advice after execute `shift-select-pre-command-hook'."
    (when (and shift-select-active
               (not this-command-keys-shift-translated))
      (let ((sym-lst '(jcs-smart-indent-up
                       jcs-smart-indent-down)))
        (when (jcs-is-contain-list-symbol sym-lst this-command)
          (deactivate-mark)))))
  (advice-add 'shift-select-pre-command-hook :after #'jcs-advice-shift-select-pre-command-hook-after))


(use-package show-eol
  :defer t
  :config
  (show-eol-set-mark-with-string 'newline-mark "¶")

  (defun jcs-advice-show-eol-enable-before ()
    "Advice before execute `show-eol-enable' command."
    (face-remap-add-relative 'whitespace-newline :inverse-video t))
  (advice-add 'show-eol-enable :before #'jcs-advice-show-eol-enable-before)

  (defun jcs-advice-show-eol-disable-before ()
    "Advice before execute `show-eol-disable' command."

    (face-remap-add-relative 'whitespace-newline :inverse-video nil))
  (advice-add 'show-eol-disable :before #'jcs-advice-show-eol-disable-before))


(use-package sql-indent
  :defer t
  :config
  ;; URL: https://www.emacswiki.org/emacs/SqlIndent

  ;; 1 = 2 spaces,
  ;; 2 = 4 spaces,
  ;; 3 = 6 spaces,
  ;; n = n * 2 spaces,
  ;; etc.
  (setq sql-indent-offset 1))


(use-package sr-speedbar
  :defer t
  :init
  ;;(setq sr-speedbar-auto-refresh nil)
  (setq speedbar-show-unknown-files t)  ; show all files
  (setq speedbar-use-images nil)  ; use text for buttons
  ;;(setq sr-speedbar-right-side nil)  ; put on left side
  )


(use-package sublimity
  :defer t
  :init
  (with-eval-after-load 'sublimity-attractive
    ;; NOTE: Following functions are available to hide
    ;; some UI parts.
    ;;(sublimity-attractive-hide-bars)
    ;;(sublimity-attractive-hide-vertical-border)
    ;;(sublimity-attractive-hide-fringes)
    ;;(sublimity-attractive-hide-modelines)
    )

  (with-eval-after-load 'sublimity-map
    (setq sublimity-map-size 0)         ; [Default : 10]
    (setq sublimity-map-fraction 0.3)   ; [Default : 0.3]
    (setq sublimity-map-text-scale -7)  ; [Default: -7]

    ;; NOTE: With the setting above, minimap is displayed after
    ;; 5 seconds of idle time. When sublimity-map-set-delay is
    ;; called with nil, then minimap is shown with no delay. This
    ;; defers from setting delay to 0, especially when used with
    ;; sublimity-scroll, in the sense that minimap looks not deleted
    ;; at all but gets worse performance.

    ;; ATTENTION: Set it to very hight so it will never
    ;; reach the timer error.
    (sublimity-map-set-delay 40000000)

    ;; NOTE: sublimity-map-setup-hook will run when
    ;; minimap is created.
    (add-hook 'sublimity-map-setup-hook
              (lambda ()
                (setq buffer-face-mode-face '(:family "Monospace"))
                (buffer-face-mode))))


  ;; NOTE: When a positive integer is set, buffer width
  ;; is truncated to this value and drawn centered. To
  ;; cancel this feature, set this value nil.
  (setq sublimity-attractive-centering-width nil)  ;; [Default : 110]

  (with-eval-after-load 'sublimity-scroll
    ;; Scroll Speed.
    (setq sublimity-scroll-weight 2         ; [Default : 2]
          sublimity-scroll-drift-length 2)  ; [Default : 2]
    ))


(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode t))


(use-package use-ttf
  :defer t
  :init
  ;; List of TTF fonts you want to use in the currnet OS.
  (setq use-ttf-default-ttf-fonts '(;; >> Classic Console <<
                                    "/.emacs.jcs/fonts/clacon.ttf"
                                    ;; >> Ubuntu Mono <<
                                    "/.emacs.jcs/fonts/UbuntuMono-R.ttf"))
  ;; Name of the font we want to use as default.
  ;; This you need to check the font name in the system manually.
  (setq use-ttf-default-ttf-font-name "Ubuntu Mono"))


(use-package web-mode
  :defer t
  :init
  ;; Associate an engine
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))
        )

  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

  ;; Indentation
  ;; NOTE: HTML element offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; NOTE: CSS offset indentation
  (setq web-mode-css-indent-offset 2)
  ;; NOTE: Script/code offset indentation (for JavaScript,
  ;;                                           Java,
  ;;                                           PHP,
  ;;                                           Ruby,
  ;;                                           Go,
  ;;                                           VBScript,
  ;;                                           Python, etc.)
  (setq web-mode-code-indent-offset 2)

  ;; Left padding
  (setq web-mode-style-padding 2)   ;; For `<style>' tag
  (setq web-mode-script-padding 2)  ;; For `<script>' tag
  (setq web-mode-block-padding 0)   ;; For `php', `ruby', `java', `python', `asp', etc.

  ;; Offsetless Elements
  ;; NOTE: Do not make these lists to one list.
  ;; They are totally different list.
  ;; NOTE: This variable is from `web-mode' itself.
  (setq web-mode-offsetless-elements '("html"))

  ;; NOTE: Do not make these lists to one list.
  ;; They are totally different list.
  (defvar jcs-web-mode-offsetless-elements-toggle '("html")
    "List of HTML elements you want to be toggable to the
`wen-mode-offsetless-elements' list in Web mode.")

  ;; Comments
  ;;(setq web-mode-comment-style 2)

  ;; Snippets
  (setq web-mode-extra-snippets
        '(("erb" . (("toto" . ("<% toto | %>\n\n<% end %>"))))
          ("php" . (("dowhile" . ("<?php do { ?>\n\n<?php } while (|); ?>"))
                    ("debug" . ("<?php error_log(__LINE__); ?>"))))
          ))

  ;; Auto-pairs
  (setq web-mode-extra-auto-pairs
        '(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))
          ))

  ;; Enable / disable features
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-heredoc-fontification t)

  ;; Keywords / Constants
  ;;(setq web-mode-extra-constants '(("php" . ("CONS1" "CONS2"))))

  ;; Highlight current HTML element
  (setq web-mode-enable-current-element-highlight t)

  ;; You can also highlight the current column with
  (setq web-mode-enable-current-column-highlight t)

  :config
  ;; Associate a content type
  (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

  ;; Syntax Highlighting
  (set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
  (set-face-attribute 'web-mode-block-comment-face nil :foreground (face-foreground font-lock-comment-face))
  (set-face-attribute 'web-mode-comment-face nil :foreground (face-foreground font-lock-comment-face))
  (set-face-attribute 'web-mode-css-property-name-face nil :foreground (face-foreground jcs-css-type-face)))


(use-package wgrep
  :defer t
  :init
  (setq wgrep-auto-save-buffer t))


(use-package which-key
  :defer t
  :init
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


(use-package whitespace
  :defer t
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


(use-package windmove
  :defer t
  :init
  (setq windmove-wrap-around t)
  (defconst jcs-windmove-max-move-count 25
    "Possible maximum windows count."))


(use-package yascroll
  :defer t
  :init
  (setq yascroll:delay-to-hide 0.8)
  :config
  (require 'cl)
  (global-yascroll-bar-mode 1))


(use-package yasnippet
  :defer t
  :config
  (require 'yasnippet-snippets)
  (yas-global-mode 1))


(provide 'jcs-plugin)
;;; jcs-plugin.el ends here
