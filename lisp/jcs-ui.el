;;; jcs-ui.el --- Better lookings and appearances  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;;; General UX

(leaf uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-after-kill-buffer-p t  ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"  ; don't muck with special buffers
        uniquify-separator "/"))

(setq ring-bell-function #'ignore
      visible-bell nil)

(leaf buffer-wrap
  :hook (( backtrace-mode-hook Buffer-menu-mode-hook package-menu-mode-hook)
         . buffer-wrap-mode)
  :defer-config
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
               (jcs-goto-line (1- (line-number-at-pos (point-max))))))
        (unless (ignore-errors (tabulated-list-get-entry))
          (ignore-errors (forward-line 1))))))

  (jcs-add-hook 'buffer-wrap-post-command-hook
    (jcs--buffer-wrap--fixed-fake-header)
    (jcs--buffer-wrap--fixed-window-off)))

;;
;;; Font

(defconst jcs-default-font-size 160
  "Default font size, the value is in 1/10pt, so 100 will give you 10pt, etc.")

(leaf use-ttf
  :init
  (setq use-ttf-default-ttf-fonts
        (mapcar (lambda (file) (concat user-emacs-directory file))
                '("fonts/clacon.ttf"
                  "fonts/DejaVuSans.ttf"
                  "fonts/DejaVuSansMono.ttf"
                  "fonts/NotoSans-Regular.ttf"
                  "fonts/NotoSansSymbols-Regular.ttf"
                  "fonts/Quivira.otf"
                  "fonts/Symbola.otf"  ; for unicode
                  "fonts/UbuntuMono-R.ttf"))
        use-ttf-default-ttf-font-name "Ubuntu Mono"))

;;
;;; Highlight

(leaf auto-highlight-symbol
  :init
  (setq ahs-idle-interval 0.15))

(leaf region-occurrences-highlighter
  :init
  (setq region-occurrences-highlighter-min-size 1))

(leaf highlight-numbers
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;
;;; Line Numbers

(leaf display-line-numbers-mode
  :init
  (setq-default
   ;; Explicitly define a width to reduce the cost of on-the-fly computation
   display-line-numbers-width 3
   ;; Show absolute line numbers for narrowed regions to make it easier to tell the
   ;; buffer is narrowed, and where you are, exactly.
   display-line-numbers-widen t))

(leaf line-reminder
  :hook (display-line-numbers-mode-hook
         . (lambda () (line-reminder-mode (if display-line-numbers-mode 1 -1))))
  :init
  (setq line-reminder-show-option (if elenv-graphic-p 'indicators 'linum)
        line-reminder-thumbnail t)
  (unless elenv-graphic-p
    (setq line-reminder-saved-sign " |"
          line-reminder-modified-sign " |")))

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

(leaf auto-scroll-bar  ; show/hide on availability
  :init
  (setq auto-scroll-bar-horizontal t
        auto-scroll-bar-disabled-major-modes '(dashboard-mode)))

(leaf better-scroll
  :init
  (setq better-scroll-align-type 'relative
        better-scroll-allow-boundary-movement t))

;;
;;; Parenthesis

(leaf paren
  :init
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;;
;;; Whitespace

(leaf whitespace
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.]))))

(leaf whitespace-cleanup-mode
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
(unless noninteractive (ignore-errors (split-window-horizontally)))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(setq windmove-wrap-around t)

(leaf balance-windows
  :init
  (setq balanced-windows-commands
        '( delete-window quit-window
           split-window-horizontally split-window-vertically)))

(leaf winum
  :init
  (setq winum-scope 'frame-local))

;;
;;; Sideline

(leaf sideline
  :hook ((flycheck-mode-hook . sideline-mode)
         (flymake-mode-hook  . sideline-mode))
  :init
  (setq sideline-delay 0.2
        sideline-backends-left '((sideline-color . up))
        sideline-backends-right '((sideline-lsp      . up)
                                  (sideline-flycheck . down)
                                  (sideline-flymake  . down))
        sideline-display-backend-name t
        sideline-display-backend-type 'inner))

(leaf sideline-flycheck :hook (flycheck-mode-hook . sideline-flycheck-setup))

;;
;;; Line Endings

(leaf show-eol
  :defer-config
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

(leaf page-break-lines
  :init
  (setq page-break-lines-modes '( browse-kill-ring-mode
                                  emacs-lisp-mode lisp-mode
                                  scheme-mode
                                  outline-mode
                                  help-mode)))

(provide 'jcs-ui)
;;; jcs-ui.el ends here
