;;; jcs-ui.el --- Better lookings and appearances  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;;; General UX

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-after-kill-buffer-p t  ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"  ; don't muck with special buffers
        uniquify-separator "/"))

(setq ring-bell-function #'ignore
      visible-bell nil)

(use-package buffer-wrap
  :hook (( backtrace-mode Buffer-menu-mode package-menu-mode)
         . buffer-wrap-mode)
  :config
  (defun jcs--buffer-wrap--fixed-window-off ()
    "Fixed windows is off after wrapping."
    (let ((max-ln (+ (line-number-at-pos (point-max)) buffer-wrap--relative-max-line)))
      (when (= max-ln (line-number-at-pos (point)))
        (jcs-recenter-top-bottom 'bottom))))

  (defun jcs--buffer-wrap--fixed-fake-header ()
    "Fixed line offset consider fake header calculation."
    (when tabulated-list-format
      (unless (ignore-errors (tabulated-list-get-entry))
        (cond ((= 0 buffer-wrap--delta-lines)
               (goto-char (point-min)))
              ((< 0 buffer-wrap--delta-lines)
               (ignore-errors (forward-line 1)))
              (t
               (elenv-goto-line (1- (line-number-at-pos (point-max))))))
        (unless (ignore-errors (tabulated-list-get-entry))
          (ignore-errors (forward-line 1))))))

  (jcs-add-hook 'buffer-wrap-post-command-hook
    (jcs--buffer-wrap--fixed-fake-header)
    (jcs--buffer-wrap--fixed-window-off)))

;;
;;; Font

(defconst jcs-standard-font-size '((width  . (1920 . 160))
                                   (height . (1080 . 160)))
  "The standard font to calculate the default font size.")

(defconst jcs-default-font-size
  (let* ((mpw (elenv-monitor-pixel-width))
         (mph (elenv-monitor-pixel-height))
         (is-v (< mph mpw))
         (sfs (alist-get (if is-v 'width 'height) jcs-standard-font-size)))
    (floor (* (/ (float (if is-v mpw mph))
                 (car sfs))
              (cdr sfs))))
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

(use-package use-ttf
  :init
  (setq use-ttf-default-ttf-fonts
        (mapcar (lambda (file) (concat user-emacs-directory file))
                '("fonts/clacon.ttf"
                  "fonts/DejaVuSans.ttf"
                  "fonts/DejaVuSansMono.ttf"
                  "fonts/NFM.ttf"                      ; nerd-icons
                  "fonts/NotoSans-Regular.ttf"
                  "fonts/NotoSansSymbols-Regular.ttf"
                  "fonts/Quivira.otf"
                  "fonts/Symbola.otf"                  ; for unicode
                  "fonts/UbuntuMono-R.ttf"))
        use-ttf-default-ttf-font-name "Ubuntu Mono"))

;;
;;; Highlight

(use-package auto-highlight-symbol
  :bind ( :map auto-highlight-symbol-mode-map
          ("M-S-<right>")
          ("M-S-<left>")
          ("M--")
          ("M-<left>")
          ("M-<right>"))
  :init
  (setq ahs-idle-interval 0.15))

(use-package region-occurrences-highlighter
  :init
  (setq region-occurrences-highlighter-min-size 1
        region-occurrences-highlighter-all-visible-buffers nil))

(use-package highlight-numbers
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;
;;; Line Numbers

(use-package display-line-numbers-mode
  :init
  (setq-default
   ;; Explicitly define a width to reduce the cost of on-the-fly computation
   display-line-numbers-width 3
   ;; Show absolute line numbers for narrowed regions to make it easier to tell the
   ;; buffer is narrowed, and where you are, exactly.
   display-line-numbers-widen t))

(use-package line-reminder
  :hook (display-line-numbers-mode
         . (lambda ()
             (when elenv-graphic-p
               (line-reminder-mode (if display-line-numbers-mode 1 -1)))))
  :init
  (setq line-reminder-show-option 'indicators
        line-reminder-thumbnail t)
  (unless elenv-graphic-p
    (setq line-reminder-saved-sign "|"
          line-reminder-modified-sign "|"
          line-reminder-thumb-modified-sign "|"
          line-reminder-thumb-saved-sign "|")))

;;
;;; Columns

(setq-default fill-column 80)
(column-number-mode 1)

;;
;;; Scrolling

(setq mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)

(setq scroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq hscroll-margin 2
      hscroll-step 1)

(use-package auto-scroll-bar  ; show/hide on availability
  :init
  (setq auto-scroll-bar-horizontal t
        auto-scroll-bar-disabled-major-modes '(dashboard-mode)))

(use-package better-scroll
  :init
  (setq better-scroll-align-type 'relative
        better-scroll-allow-boundary-movement t))

;;
;;; Parenthesis

(use-package paren
  :init
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;;
;;; Whitespace

(use-package whitespace
  :init
  (setq whitespace-line-column nil
        whitespace-style
        '( face indentation tabs tab-mark spaces space-mark newline newline-mark
           trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark     ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark   ?\  [?·] [?.]))))

(use-package whitespace-cleanup-mode
  :init
  (setq whitespace-cleanup-mode-preserve-point t
        whitespace-cleanup-mode-only-if-initially-clean nil
        whitespace-cleanup-mode-ignore-modes
        '( special-mode comint-mode cider-repl-mode haskell-interactive-mode
           text-mode markdown-mode org-mode
           conf-javaprop-mode ini-mode
           view-mode diff-mode
           snippet-mode)))

;;
;;; Image

(setq image-animate-loop t)

;;
;;; Window

(push '(fullscreen . maximized) default-frame-alist)  ; full screen

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(setq windmove-wrap-around t)

(use-package balance-windows
  :init
  (setq balanced-windows-commands
        '( delete-window jcs-delete-window quit-window
           split-window-horizontally split-window-vertically)))

(use-package winum
  :init
  (setq winum-scope 'frame-local))

(use-package repos-window
  :init
  (setq repos-window-commands '( hl-todo-previous
                                 hl-todo-next)
        repos-window-switch-commands '( push-button
                                        compile-goto-error)))

;;
;;; Line Endings

(use-package show-eol
  :config
  (jcs-advice-add 'show-eol-enable :before
    (face-remap-add-relative 'whitespace-newline :inverse-video t))
  (jcs-advice-add 'show-eol-disable :before
    (face-remap-add-relative 'whitespace-newline :inverse-video nil)))

;;
;;; Dialog

(setq use-file-dialog nil
      use-dialog-box nil)

;;
;;; ^L

(use-package page-break-lines
  :init
  (setq page-break-lines-modes '( browse-kill-ring-mode
                                  emacs-lisp-mode lisp-mode
                                  scheme-mode
                                  outline-mode
                                  help-mode)))

(provide 'jcs-ui)
;;; jcs-ui.el ends here
