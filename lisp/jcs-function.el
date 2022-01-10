;;; jcs-function.el --- Self defines function  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'jcs-util)
(require 'jcs-window)
(require 'jcs-nav)

;;
;; (@* "Advices" )
;;

(defun jcs--recenter--advice-after ()
  "Advice for commands that we do recenter after execution."
  (call-interactively #'recenter))

;;
;; (@* "*Backtrace*" )
;;

(defconst jcs-backtrace-buffer-name "*Backtrace*"
  "Name of the backtrace buffer.")

(defvar jcs-backtrace--occurs-last-command nil
  "Check if backtrace occurs last command.")

(defvar jcs-backtrace--dedicated-window nil
  "Record down backtrace dedicated window.")

(defun jcs-backtrace--dedicated-window-p (&optional win)
  "Return non-nil if WIN the backtrace dedicated window."
  (equal (or win (get-buffer-window)) jcs-backtrace--dedicated-window))

(defun jcs-hit-backtrace ()
  "Do stuff when backtrace occures."
  (jcs-red-mode-line)  ; When error, use red mode line.
  (jcs-no-log-apply
    (message "[INFO] Oops, error occurs! Please see backtrace for more information")))

(defun jcs-backtrace--ensure-stay-in-buffer ()
  "Ensure stay in backtrace buffer base on conditions."
  (let ((backtrace-killed-p (not (get-buffer-window))))
    (when (or (jcs-backtrace--dedicated-window-p) backtrace-killed-p)
      (switch-to-buffer jcs-backtrace-buffer-name))))

(defun jcs-reload-active-mode-with-error-handle ()
  "Reload the active by handling the error occurrence."
  (unless (minibufferp)
    (if (jcs-backtrace-occurs-p)
        (progn
          (ignore-errors
            (jcs-hit-backtrace)
            (setq jcs-backtrace--occurs-last-command t)
            (jcs-backtrace--ensure-stay-in-buffer)
            (if jcs-backtrace--dedicated-window
                (when (and (not (eq (selected-window) jcs-backtrace--dedicated-window))
                           (jcs-buffer-shown-in-multiple-window-p jcs-backtrace-buffer-name))
                  (jcs-maybe-kill-this-buffer))
              (setq jcs-backtrace--dedicated-window (get-buffer-window jcs-backtrace-buffer-name)))))
      (when jcs-backtrace--occurs-last-command
        (jcs-reload-active-mode)
        (setq jcs-backtrace--occurs-last-command nil)
        (when (windowp jcs-backtrace--dedicated-window)
          (ignore-errors (delete-window jcs-backtrace--dedicated-window)))
        (setq jcs-backtrace--dedicated-window nil)))))

;;
;; (@* "*Messages*" )
;;

(defconst jcs-message-buffer-name "*Messages*"
  "Name of the message buffer.")

(defun jcs-message-buffer ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer jcs-message-buffer-name))

(defun jcs-message-buffer-other-window ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window jcs-message-buffer-name))

(defun jcs-message-erase-buffer ()
  "Erase the *Messages* buffer."
  (interactive)
  (let ((is-killed (jcs-maybe-kill-this-buffer)))
    ;; Message one message to retrieve `*Message*' buffer prepare for next use.
    ;; Or else it some operation might prompt some issue that needed `*Message*'
    ;; buffer to be exists.
    (when is-killed (message "Retrieving %s buffer.." jcs-message-buffer-name))))

(defun jcs-message-erase-buffer-stay ()
  "Reopen *Messages* buffer."
  (interactive)
  (jcs-message-erase-buffer)
  (switch-to-buffer jcs-message-buffer-name))

;;
;; (@* "*scratch*" )
;;

(defconst jcs-scratch-buffer-name "*scratch*"
  "Name of the scratch buffer.")

(defvar jcs-scratch--content ""
  "Record down the scratch content string.")

(defun jcs-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (switch-to-buffer jcs-scratch-buffer-name))

(defun jcs-scratch-buffer-other-window ()
  "Start a new scratch buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window jcs-scratch-buffer-name))

(defun jcs-new-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (jcs-scratch-buffer)
  (erase-buffer)
  (insert jcs-scratch--content)
  (goto-char (point-min))
  (lisp-interaction-mode))

(defun jcs-scratch-buffer-maybe-kill ()
  "Kill buffer scratch."
  (interactive)
  (require 'jcs-undo)
  (if (string= (buffer-name) jcs-scratch-buffer-name)
      (progn (jcs-undo-kill-this-buffer) (jcs-bury-buffer))
    (jcs-maybe-kill-this-buffer)))

(defun jcs-scratch-buffer-refresh ()
  "Refresh scratch buffer."
  (interactive)
  (if (string= (buffer-name) jcs-scratch-buffer-name)
      (jcs-new-scratch-buffer)
    (jcs-reopen-this-buffer)))

;;
;; (@* "Buffer Menu" )
;;

(defconst jcs-buffer-menu-buffer-name "*Buffer List*"
  "Name of the buffer menu's buffer.")

(defconst jcs--buffer-menu-search-title "Search: "
  "Search bar title in `buffer-menu''s buffer.")

(defvar jcs--buffer-menu-return-delay nil
  "Record if hit return when display not ready; once it is ready we redo the action.")

(defvar jcs--buffer-menu--first-enter nil
  "Record if fake header already appears.")

(jcs-advice-add 'list-buffers-noselect :after
  (setq jcs--buffer-menu-return-delay nil)
  (unless jcs-buffer--menu-switch-buffer-refreshing
    (setq jcs--buffer-menu--first-enter nil)
    (setq-local tabulated-list--header-string jcs--buffer-menu-search-title)))

(defvar jcs-buffer--menu-switch-buffer-refreshing nil
  "Flag to check if current buffer menu refresing.")

(defun jcs-buffer-menu-refresh-buffer ()
  "Update buffer menu buffer."
  (interactive)
  (unless (string= (jcs-buffer-name-or-buffer-file-name) jcs-buffer-menu-buffer-name)
    (save-window-excursion
      (let (tabulated-list--header-string) (jcs-mute-apply (buffer-menu)))
      (when jcs-buffer--menu-switch-buffer-refreshing
        (jcs--buffer-menu-trigger-filter))
      (bury-buffer)))
  (jcs-safe-jump-shown-to-buffer
   jcs-buffer-menu-buffer-name
   :success
   (lambda ()
     (when (and (tabulated-list-header-overlay-p) (= (line-number-at-pos) 1))
       (jcs-goto-line 2)))))

(defun jcs-buffer-menu-safe-refresh ()
  "Safely refresh `buffer menu`'s buffer."
  (unless jcs-buffer--menu-switch-buffer-refreshing
    (let ((jcs-buffer--menu-switch-buffer-refreshing t))
      (jcs-buffer-menu-refresh-buffer))))

;;
;; (@* "Calculator" )
;;

(defun jcs-calc-eval-region ()
  "Eval the arithmetic expression in the region and replace it with the result."
  (interactive)
  (if (not (use-region-p))
      (message "[INFO] Trying to use calc eval but with no region selected")
    (let ((val (calc-eval (buffer-substring (region-beginning) (region-end)))))
      (jcs-delete-region)
      (insert val))))

;;
;; (@* "Dashboard" )
;;

(defvar dashboard-buffer-name)

(defun jcs-dashboard (&optional ow)
  "Jump to the dashboard buffer, if doesn't exists create one.
OW is the other window flag."
  (interactive)
  (jcs-switch-to-buffer dashboard-buffer-name ow)
  (unless (eq major-mode 'dashboard-mode) (dashboard-mode))
  (jcs-dashboard-refresh-buffer))

(defun jcs-dashboard-other-window ()
  "Just like `jcs-dashboard', but open on the other window."
  (interactive)
  (jcs-dashboard t))

(defvar jcs-dashboard--force-refresh-p nil
  "Force refresh dashboard buffer when non-nil.")

(defvar jcs-dashboard--refreshing-p nil
  "Flag to check if current dashboard refresing.")

(defvar jcs-dashboard--last-ls-path nil
  "Record down the last current path.")

(defun jcs-dashboard-refresh-buffer ()
  "Update dashboard buffer by killing it and start a new one."
  (interactive)
  (when (or (not after-init-time)
            (jcs-buffer-shown-p dashboard-buffer-name 'strict)
            jcs-dashboard--force-refresh-p)
    (jcs-mute-apply
      (jcs-save-window-excursion
        (let ((dashboard-ls-path (jcs-last-default-directory)))
          (when (or (not (active-minibuffer-window))
                    (and (not jcs-minibuf-enabled-p) (not (jcs-minibuf-prompt-p))))
            (save-window-excursion (dashboard-refresh-buffer))))))))

(defun jcs-dashboard-safe-refresh-buffer (&optional force)
  "Safely refresh the dashboard buffer if needed.

If optional argument FORCE is non-nil, force refresh it."
  (when (and after-init-time
             (boundp 'dashboard-buffer-name)
             (jcs-buffer-shown-p dashboard-buffer-name 'strict))
    (unless jcs-dashboard--refreshing-p
      (let ((jcs-dashboard--refreshing-p t)
            (ls-path (jcs-last-default-directory)))
        (when (or force (not (string= jcs-dashboard--last-ls-path ls-path)))
          (setq jcs-dashboard--last-ls-path ls-path)
          (jcs-safe-jump-shown-to-buffer
           dashboard-buffer-name
           :type 'strict
           :success (lambda () (jcs-dashboard-refresh-buffer))))))))

(defun jcs-dashboard--get-banner-path ()
  "Return banner path."
  (concat
   user-emacs-directory
   (cond ((display-graphic-p)
          (if (jcs-light-theme-p) "banners/sink_black.png" "banners/sink_white.png"))
         (t "banners/sink.txt"))))

;;
;; (@* "ElDoc" )
;;

(defun jcs-eldoc-message-now () "Show eldoc message now." (interactive))

(defun jcs-eldoc--message-command-p (command)
  "Advice overwrite `eldoc--message-command-p' COMMAND."
  ;; One can also loop through `eldoc-message-commands' and empty it out
  (memq command
        '(jcs-eldoc-message-now
          mouse-set-point
          jcs-real-space jcs-smart-space
          jcs-real-backspace jcs-smart-backspace
          previous-line next-line
          jcs-smart-indent-up jcs-smart-indent-down
          jcs-py-indent-up jcs-py-indent-down
          left-char right-char
          jcs-smart-forward-word jcs-smart-backward-word
          jcs-backward-word-capital jcs-forward-word-capital
          beginning-of-line end-of-line
          jcs-beginning-of-line jcs-end-of-line)))
(advice-add 'eldoc--message-command-p :override #'jcs-eldoc--message-command-p)

;;
;; (@* "Electric Pair" )
;;

(defun jcs-elec-pair-add (lst-pr)
  "Append a list of pair (LST-PR) to current buffer."
  (require 'elec-pair)
  (setq-local electric-pair-pairs (append electric-pair-pairs lst-pr)
              electric-pair-text-pairs electric-pair-pairs))

;;
;; (@* "Expand Region" )
;;

(defun jcs-er/contract-region ()
  "Wrapper for function `er/contract-region' from `expand-region'."
  (interactive)
  (require 'expand-region)
  (er/contract-region 1))

(defconst jcs--er/commands
  '(er/expand-region er/contract-region jcs-er/contract-region)
  "List of commands that active `expand-region'.")

(defvar-local jcs--er/marking-p nil
  "Resolve marking for `expand-region'.")

(defun jcs--er/prepare-command ()
  "Preparation for each `expand-region' command."
  (setq web-mode-expand-previous-state nil))

(defun jcs--er/resolve-region ()
  "Resolve marking while no longer expanding region."
  (if (memq this-command jcs--er/commands)
      (progn
        (unless jcs--er/marking-p (jcs--er/prepare-command))
        (setq jcs--er/marking-p t)
        (when (and (not (use-region-p)) jcs--er/history-last)
          (let ((start (car jcs--er/history-last))
                (end (cdr jcs--er/history-last)))
            (unless (= start end)
              (goto-char start)
              (set-mark end)))))
    (when jcs--er/marking-p
      (setq jcs--er/marking-p nil)
      (deactivate-mark))))

(defvar-local jcs--er/history-last nil
  "Record the last item from er/history.")

(defun jcs--er/record-history ()
  "Record the last item from variable `er/history'."
  (when (featurep 'expand-region)
    (setq jcs--er/history-last (nth 0 er/history))))

(defun jcs-safe-er/expand-list (data &optional append)
  "Safe way to modify expand list from `expand-region'."
  (require 'expand-region)
  (unless (listp data) (setq data (list data)))
  (setq er/try-expand-list (if append (append data er/try-expand-list) data))
  (delete-dups er/try-expand-list))

;;
;; (@* "Iedit" )
;;

(defun jcs-iedit-mode ()
  "Enable Iedit mode in the safe way."
  (interactive)
  (require 'iedit)
  (let ((kill-ring kill-ring))
    (if iedit-mode
        (call-interactively #'iedit-mode)
      (when (or (word-at-point) (symbol-at-point))
        (call-interactively #'iedit-mode))))
  ;; Call this function just to update `kill-ring'.
  (when (and (not iedit-mode) kill-ring) (current-kill 1))
  iedit-mode)

;;
;; (@* "Line Numbers" )
;;

(defun jcs-update-line-number-each-window ()
  "Update each window's line number mode."
  (interactive)
  (jcs-walk-windows #'jcs-active-line-numbers-by-mode nil t))

(defun jcs-safe-display-line-numbers (act)
  "Active `display-line-numbers' by ACT."
  (require 'display-line-numbers)
  (if (and (numberp act) (>= act 1))
      (unless display-line-numbers-mode (display-line-numbers-mode 1))
    (when display-line-numbers-mode (display-line-numbers-mode -1))))

(defun jcs-safe-display-linum (act)
  "Active `linum' by ACT."
  (require 'linum)
  (if (and (numberp act) (>= act 1)) (unless linum-mode (linum-mode 1))
    (when linum-mode (linum-mode -1))))

(defun jcs-safe-line-numbers-active (act)
  "Safe way to active (ACT) line numbers."
  (if (display-graphic-p) (jcs-safe-display-line-numbers act)
    (jcs-safe-display-linum act)))

(defun jcs-active-line-numbers-by-mode ()
  "Active line number by mode."
  (interactive)
  (require 'line-reminder)
  (if (or (minibufferp)
          (and (jcs-contain-list-string-regexp jcs-line-numbers-ignore-buffers (buffer-name))
               (not (member (buffer-name) jcs-line-numbers-ignore-buffer-exceptions)))
          (memq major-mode jcs-line-numbers-ignore-modes))
      (progn
        (when line-reminder-mode (line-reminder-mode -1))
        (jcs-safe-line-numbers-active -1))
    (unless line-reminder-mode (line-reminder-mode 1))
    (jcs-safe-line-numbers-active 1)))

;;
;; (@* "Minimap" )
;;

(defun jcs-toggle-minimap ()
  "Toggle minimap."
  (interactive)
  (user-error "[INFO] Minimap no longer supported in this configuration"))

;;
;; (@* "Prettify / Minify" )
;;

(defun jcs-prettify-contents ()
  "Prettify contents by file type."
  (interactive)
  (require 'sgml-mode)
  (let* ((inhibit-modification-hooks t)
         (bound (jcs-region-bound))
         (start (car bound)) (end (cdr bound)))
    (cl-case major-mode
      (`json-mode (json-reformat-region start end))
      ((or nxml-mode xml-mode web-mode html-mode) (sgml-pretty-print start end))
      (t (user-error "[WARNING] No prettify command in this context")))))

(defun jcs-minify-contents ()
  "Minify contents by removing newlines and whitespaces."
  (interactive)
  (let* ((inhibit-modification-hooks t)
         (bound (jcs-region-bound))
         (start (car bound)) (end (cdr bound)))
    (delete-whitespace-rectangle start end)
    (goto-char start)
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

;;
;; (@* "Re-Builder" )
;;

(defconst jcs-re-builder-buffer-name "*RE-Builder*"
  "Name of the re-builder buffer.")

(defun jcs-re-builder (type)
  "Rewrap `re-builder' function to ask search case TYPE."
  (interactive
   (list (completing-read
          "Enable case sensitive?" '("Case Sensitive"
                                     "Case Insensitive"))))
  (let ((case-fold-search (string= type "Case Insensitive")))
    (re-builder)))

;;
;; (@* "Syntax Checker" )
;;

(defun jcs-flycheck-mode ()
  "Flycheck mode toggle."
  (interactive)
  (require 'flycheck)
  (if (string= (buffer-name) flycheck-error-list-buffer)
      (jcs-safe-jump-shown-to-buffer
       (buffer-name flycheck-error-list-source-buffer)
       :success #'jcs-flycheck-mode
       :error #'jcs-maybe-kill-this-buffer)
    (call-interactively #'flycheck-mode)
    (if flycheck-mode
        (progn
          (save-window-excursion (call-interactively #'flycheck-list-errors))
          (save-selected-window
            (jcs-switch-to-next-window-larger-in-height)
            (switch-to-buffer flycheck-error-list-buffer)))
      (jcs-safe-jump-shown-to-buffer
       flycheck-error-list-buffer
       :success #'jcs-maybe-kill-this-buffer)))
  flycheck-mode)

;;
;; (@* "Tab Bar" )
;;

(defun jcs-toggle-tabbar-mode ()
  "Toggle tab bar."
  (interactive)
  (jcs-enable-disable-mode-if 'centaur-tabs-mode (not centaur-tabs-mode))  ; toggle
  (jcs-walk-windows
   (lambda ()  ; insist with new result
     (jcs-enable-disable-mode-if 'centaur-tabs-mode centaur-tabs-mode))
   nil t))

;;
;; (@* "Terminal / Shell" )
;;

(defun jcs-toggle-shell-window ()
  "Toggle Shell Command prompt."
  (interactive)
  (require 'jcs-shell)
  (jcs-safe-jump-shown-to-buffer
   (multi-shell--prefix-name)
   :success #'jcs-hide-shell-window
   :error #'jcs-show-shell-window))

(defun jcs-shell-new-shell ()
  "Create a new shell window."
  (interactive)
  (require 'jcs-shell)
  (jcs-safe-jump-shown-to-buffer
   (multi-shell--prefix-name)
   :success (lambda () (other-window -2) (other-window 1) (multi-shell))
   :error #'jcs-show-shell-window))

;;
;; (@* "Zoom" )
;;

(defun jcs-reset-zoom ()
  "Reset zoom level."
  (interactive)
  (text-scale-set 0))

(defun jcs-text-scale-delta (vec)
  "Scale the text by passing `vec' value.
VEC : Either position or negative number."
  (let ((was-dln display-line-numbers-mode))
    ;; NOTE: Known `text-scale-increase' and `text-scale-decrease' ruin the
    ;; margin of the `linum-mode'. Disable it before ruining it, to avoid the bug.
    (when was-dln (display-line-numbers-mode -1))
    (if (> vec 0)
        (call-interactively #'text-scale-increase)
      (call-interactively #'text-scale-decrease))
    ;; Renable line number mode.
    (when was-dln (display-line-numbers-mode 1))))

(defun jcs-text-scale-increase ()
  "Scale the text up."
  (interactive)
  (jcs-text-scale-delta 1))

(defun jcs-text-scale-decrease ()
  "Scale the text down."
  (interactive)
  (jcs-text-scale-delta -1))

;;
;; (@* "Tips" )
;;

(cl-defun jcs-pop-tooltip (string &key point (timeout 300) (height 30))
  "Pop up an tooltip depends on the graphic used.

STRING is the content of the toolip. The location POINT. TIMEOUT for not forever
delay. HEIGHT of the tooltip that will display."
  (require 'asoc)
  (require 'pos-tip) (require 'popup)
  (let ((bg (asoc-get company-box-doc-frame-parameters 'background-color))
        (fg (asoc-get company-box-doc-frame-parameters 'foreground-color)))
    (if (display-graphic-p)
        (pos-tip-show string `(,fg . ,bg) point nil timeout)
      (popup-tip string :point point :around t :height height :scroll-bar t :margin t))
    t))

(defun jcs--describe-symbol-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (with-temp-buffer
      (jcs-mute-apply (help-mode) (describe-symbol thing))
      (buffer-string))))

(defun jcs-tip-describe-it ()
  "Describe symbol at point."
  (let* ((help-xref-following t)
         (desc (jcs--describe-symbol-string))
         (timeout 300))
    (if (string-empty-p desc)
        (error "[ERROR] No description at point")
      (jcs-pop-tooltip desc :point (point) :timeout timeout))))

(defun jcs-describe-thing-in-popup ()
  "Show current symbol info."
  (interactive)
  (require 'define-it)
  (if (jcs--lsp-connected-p)
      (progn (require 'lsp-ui)
             (ignore-errors (call-interactively #'lsp-ui-doc-glance)))
    (unless (ignore-errors (jcs-tip-describe-it))
      (unless (ignore-errors (jcs-path-info-at-point))
        (define-it-at-point)))
    ;; In case we are using region, cancel the select region.
    (deactivate-mark)))

;;
;; (@* "Yasnippet" )
;;

(defun jcs-yas-expand ()
  "Yasnippet expand current point."
  (interactive)
  (require 'yasnippet-snippets)
  (call-interactively #'yas-expand))

(provide 'jcs-function)
;;; jcs-function.el ends here
