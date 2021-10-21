;;; jcs-function.el --- Self defines function  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(defun jcs-backtrace-dedicated-window-p (&optional win)
  "Check if WIN the backtrace dedicated window."
  (unless win (setq win (get-buffer-window)))
  (equal win jcs-backtrace--dedicated-window))

(defun jcs-hit-backtrace ()
  "Do stuff when backtrace occures."
  (jcs-red-mode-line)  ; When error, use red mode line.
  (jcs-no-log-apply
    (message "[INFO] Oops, error occurs! Please see backtrace for more information")))

(defun jcs-backtrace--ensure-stay-in-buffer ()
  "Ensure stay in backtrace buffer base on conditions."
  (let ((backtrace-killed-p (not (get-buffer-window))))
    (when (or (jcs-backtrace-dedicated-window-p) backtrace-killed-p)
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

(defvar jcs--message-buffer--first-init-p nil
  "Flag to check if message buffer first initialized with hook runs.")

(defun jcs-message-buffer ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer jcs-message-buffer-name)
  (jcs--message-buffer--first-load))

(defun jcs-message-buffer-other-window ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window jcs-message-buffer-name)
  (jcs--message-buffer--first-load))

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

(defun jcs--message-buffer--first-load ()
  "First load message buffer, ensure the hook runs."
  (unless jcs--message-buffer--first-init-p
    (messages-buffer-mode)
    (setq jcs--message-buffer--first-init-p t)))

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
;; (@* "Autio Highlight Symbol" )
;;

(defun jcs--ahs--set-face (face bg)
  "Set FACE with BG and BOX for `auto-highlight-symbol'."
  (when (boundp face)
    (set-face-attribute face nil :foreground nil :background bg
                        :box `(:line-width -1 :style pressed-button :color "#525D68"))))

(defun jcs-reset-ahs-by-theme ()
  "Reset `auto-highlight-symbol' by theme."
  (let* ((light-p (jcs-is-light-theme-p))
         (focused-color (if light-p "#E2E6D6" "#123E70"))
         (unfocused-color (if light-p "#F1F2EE" "#0E3056")))
    (jcs--ahs--set-face 'ahs-plugin-default-face focused-color)
    (jcs--ahs--set-face 'ahs-plugin-default-face-unfocused unfocused-color)
    (if light-p
        (progn
          (jcs--ahs--set-face 'ahs-face focused-color)
          (jcs--ahs--set-face 'ahs-definition-face focused-color)
          (jcs--ahs--set-face 'ahs-face-unfocused unfocused-color)
          (jcs--ahs--set-face 'ahs-definition-face-unfocused unfocused-color))
      (jcs--ahs--set-face 'ahs-face focused-color)
      (jcs--ahs--set-face 'ahs-definition-face focused-color)
      (jcs--ahs--set-face 'ahs-face-unfocused unfocused-color)
      (jcs--ahs--set-face 'ahs-definition-face-unfocused unfocused-color))))

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

(defun jcs--buffer-menu--advice-after (&rest _)
  "Advice execute after `list-buffers-noselect' command."
  (setq jcs--buffer-menu-return-delay nil)
  (unless jcs-buffer--menu-switch-buffer-refreshing
    (setq jcs--buffer-menu--first-enter nil)
    (setq-local tabulated-list--header-string jcs--buffer-menu-search-title)))
(advice-add 'list-buffers-noselect :after #'jcs--buffer-menu--advice-after)

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
;; (@* "Cheat Sheet" )
;;

(defun jcs-alt-codes-table ()
  "Display basic Alt-Codes table."
  (interactive)
  (jcs-display-file "~/.emacs.jcs/data/charset/alt-code.txt" "*Alt Codes*" nil))

(defun jcs-ascii-table ()
  "Display basic ASCII table."
  (interactive)
  (jcs-display-file "~/.emacs.jcs/data/charset/ascii.txt" "*ASCII*" nil))

(defun jcs-algorithm-cheat-sheet ()
  "Display basic Alt-Codes table."
  (interactive)
  (jcs-html-preview "~/.emacs.jcs/data/algorithm/cheat-sheet.html" "*Algorithm Cheat Sheet*" nil))

(defun jcs-data-structure-cheat-sheet ()
  "Display basic Alt-Codes table."
  (interactive)
  (jcs-display-file "~/.emacs.jcs/data/data-structure/cheat-sheet.txt" "*Data Structure Cheat Sheet*" nil))

;;
;; (@* "Dashboard" )
;;

(defvar dashboard-buffer-name)

(defun jcs-dashboard (&optional ow)
  "Jump to the dashboard buffer, if doesn't exists create one.
OW is the other window flag."
  (interactive)
  (jcs-switch-to-buffer dashboard-buffer-name ow)
  (unless (jcs-is-current-major-mode-p "dashboard-mode") (dashboard-mode))
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
  (when (or (not jcs-emacs-ready-p)
            (jcs-buffer-shown-p dashboard-buffer-name 'strict)
            jcs-dashboard--force-refresh-p)
    (jcs-mute-apply
      (jcs-save-window-excursion
        (let ((dashboard-ls-path (jcs-last-default-directory)))
          (when (and (or (not (active-minibuffer-window))
                         (and (not jcs-minibuf-enabled-p) (not (jcs-minibuf-prompt-p)))))
            (save-window-excursion (dashboard-refresh-buffer))))))))

(defun jcs-dashboard-safe-refresh-buffer (&optional force)
  "Safely refresh the dashboard buffer if needed.

If optional argument FORCE is non-nil, force refresh it."
  (when (and (bound-and-true-p jcs-emacs-ready-p)
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
  "Return the path of the banner."
  (cond ((display-graphic-p)
         (if (jcs-is-light-theme-p) "~/.emacs.jcs/banner/sink_black.png"
           "~/.emacs.jcs/banner/sink_white.png"))
        (t "~/.emacs.jcs/banner/sink.txt")))

(defun jcs-reset-dashboard-banner-by-theme ()
  "Reset dashboard banner."
  (interactive)
  (setq dashboard-startup-banner (jcs-dashboard--get-banner-path))
  (let ((logo-title-fg "cyan1") (heading-fg "#17A0FB") (wb-fg "light steel blue"))
    (when (jcs-is-light-theme-p)
      (setq logo-title-fg "#616161"
            heading-fg "#727272"
            wb-fg "#1475B7"))
    (jcs--set-common-face 'dashboard-banner-logo-title logo-title-fg)
    (jcs--set-common-face 'dashboard-heading heading-fg)
    (set-face-attribute 'widget-button nil :weight 'normal :foreground wb-fg))
  (jcs-dashboard-refresh-buffer))

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
          jcs-previous-line jcs-next-line
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

(defun jcs-make-electric-pair-pairs-local (lst-pr)
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
  (let ((kill-ring kill-ring))
    (require 'iedit)
    (if iedit-mode
        (call-interactively #'iedit-mode)
      (when (or (jcs-get-word-at-point) (jcs-get-symbol-at-point))
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
               (not (jcs-contain-list-string jcs-line-numbers-ignore-buffer-exceptions (buffer-name))))
          (jcs-contain-list-string jcs-line-numbers-ignore-modes (symbol-name major-mode)))
      (progn
        (when line-reminder-mode (line-reminder-mode -1))
        (jcs-safe-line-numbers-active -1))
    (unless line-reminder-mode (line-reminder-mode 1))
    (jcs-safe-line-numbers-active 1)))

;;
;; (@* "Media" )
;;

(defun jcs-media-find-file ()
  "Open the media file."
  (interactive)
  (require 'jcs-media)
  (let (do-play media-path)
    (if ffmpeg-player--buffer
        (when  (yes-or-no-p "There is video playing, kill it? ")
          (jcs-safe-jump-shown-to-buffer
           "[*]ffmpeg-player[*]: "
           :success (lambda () (jcs-media-close-media-window))
           :error (lambda () (with-current-buffer ffmpeg-player--buffer
                               (jcs-media-close-media-window))))
          (setq do-play t))
      (setq do-play t))
    (when do-play
      (setq media-path (jcs-select-file))
      (when media-path
        (jcs-save-window-excursion
          (save-window-excursion (ffmpeg-player-video media-path)))
        (jcs-media--open-media-window)))))

;;
;; (@* "Minimap" )
;;

(defun jcs-toggle-minimap ()
  "Toggle minimap."
  (interactive)
  (user-error "Minimap no longer supported in this configuration"))

;;
;; (@* "Parentheses" )
;;

(defun jcs-reset-show-paren-by-theme ()
  "Reset `paren' by theme."
  (require 'paren)
  (let ((color (if (jcs-is-light-theme-p) "#C6E370" "#113D6F")))
    (set-face-background 'show-paren-match color)))

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
    (cond ((jcs-is-current-major-mode-p '("json-mode"))
           (json-reformat-region start end))
          ((jcs-is-current-major-mode-p '("nxml-mode" "xml-mode"
                                          "web-mode" "html-mode"))
           (sgml-pretty-print start end))
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

(defun jcs-reb-maybe-kill-this-buffer ()
  "Kill this buffer in `re-builder' mode."
  (interactive)
  (let (is-killed)
    (setq is-killed (jcs-maybe-kill-this-buffer))
    (when is-killed (delete-window))))

(defun jcs-re-builder (type)
  "Rewrap `re-builder' function to ask search case TYPE."
  (interactive
   (list (completing-read
          "Enable case sensitive?" '("Case Sensitive"
                                     "Case Insensitive"))))
  (let ((case-fold-search (string= type "Case Insensitive")))
    (re-builder)))

;;
;; (@* "Shift Select" )
;;

(defun jcs-toggle-shift-select-mode ()
  "Toggle `shift-select-mode'."
  (interactive)
  (if shift-select-mode
      (jcs-disable-shift-select-mode)
    (jcs-enable-shift-select-mode)))

(defun jcs-enable-shift-select-mode ()
  "Enable `shift-select-mode'."
  (interactive)
  (setq shift-select-mode t))

(defun jcs-disable-shift-select-mode ()
  "Enable `shift-select-mode'."
  (interactive)
  (setq shift-select-mode nil))

;;
;; (@* "Sort" )
;;

(defun jcs-sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

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
  (jcs-enable-disable-mode-by-condition 'centaur-tabs-mode (not centaur-tabs-mode))
  (jcs-reset-tabbar-theme)
  ;; Loop through all window so all windows take effect.
  (jcs-buffer-visible-list))

(defun jcs-reset-tabbar-theme ()
  "Set the tabbar theme to match the current theme color."
  (when centaur-tabs-mode
    (let* ((is-light (jcs-is-light-theme-p))
           (bg-default (if is-light "#D3D3D3" "#1D1D1D"))
           (bg-tab-unselected (if is-light "#E8E8E8" "#3D3C3D"))
           (fg-tab-unselected "grey50")
           (bg-tab-selected (if is-light "#E8E8E8" "#31343E"))
           (fg-tab-selected (if is-light "black" "white")))
      (set-face-attribute centaur-tabs-display-line nil :background bg-default
                          :box nil :overline nil :underline nil)
      (custom-set-faces
       `(centaur-tabs-default ((t (:background ,bg-default))))
       `(centaur-tabs-unselected
         ((t (:background ,bg-tab-unselected :foreground ,fg-tab-unselected))))
       `(centaur-tabs-selected
         ((t (:background ,bg-tab-selected :foreground ,fg-tab-selected))))
       `(centaur-tabs-unselected-modified
         ((t (:background ,bg-tab-unselected :foreground ,fg-tab-unselected))))
       `(centaur-tabs-selected-modified
         ((t (:background ,bg-tab-selected :foreground ,fg-tab-selected))))
       `(centaur-tabs-modified-marker-unselected
         ((t (:background ,bg-tab-unselected :foreground ,fg-tab-unselected))))
       `(centaur-tabs-modified-marker-selected
         ((t (:background ,bg-tab-selected :foreground ,fg-tab-selected))))))))

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
    ;; NOTE: Known `text-scale-increase' and
    ;; `text-scale-decrease' ruin the margin of the
    ;; `linum-mode'. Disable it before ruining it, to
    ;; avoid the bug.
    (when was-dln (display-line-numbers-mode -1))
    (if (jcs-is-positive vec)
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
  (require 'flycheck) (require 'pos-tip) (require 'popup)
  (let ((was-flycheck (if flycheck-mode 1 -1))
        (bg (cdr (assoc 'background-color company-box-doc-frame-parameters)))
        (fg (cdr (assoc 'foreground-color company-box-doc-frame-parameters))))
    (if (display-graphic-p)
        (pos-tip-show string `(,fg . ,bg) point nil timeout)
      (popup-tip string :point point :around t :height height :scroll-bar t :margin t))
    (flycheck-mode was-flycheck)
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
  (require 'ffap)
  (if (and (boundp 'lsp-mode) lsp-mode)
      (ignore-errors (call-interactively #'lsp-ui-doc-show))
    (unless (ignore-errors (jcs-tip-describe-it))
      (unless (ignore-errors (jcs-path-info-at-point))
        (define-it-at-point)))
    ;; In case we are using region, cancel the select region.
    (deactivate-mark)))

;;
;; (@* "Todo" )
;;

(defvar jcs-hl-todo-not-found-prev nil
  "See if found the previous `hl-todo' matches.")

(defvar jcs-hl-todo-not-found-next nil
  "See if found the next `hl-todo' matches.")

(defun jcs-hl-todo-previous (&optional no-prompt)
  "Around `hl-todo-previous' command.
NO-PROMPT : Don't prompt the overwrap message."
  (interactive)
  (require 'hl-todo)
  (setq jcs-hl-todo-not-found-next nil)
  (if jcs-hl-todo-not-found-prev
      (progn
        (setq jcs-hl-todo-not-found-prev nil)
        (goto-char (point-max))
        (call-interactively #'hl-todo-previous))
    (let ((before-pt (point)))
      (ignore-errors (call-interactively #'hl-todo-previous))
      (if (not (= (point) before-pt))
          (setq jcs-hl-todo-not-found-prev nil)
        (setq jcs-hl-todo-not-found-prev t)
        (if no-prompt
            (jcs-hl-todo-previous)
          (message "%s" (propertize "user-error: No more matches :: overwrap"
                                    'face '(:foreground "cyan"))))))))

(defun jcs-hl-todo-next (&optional no-prompt)
  "Around `hl-todo-next' command.
NO-PROMPT : Don't prompt the overwrap message."
  (interactive)
  (require 'hl-todo)
  (setq jcs-hl-todo-not-found-prev nil)
  (if jcs-hl-todo-not-found-next
      (progn
        (setq jcs-hl-todo-not-found-next nil)
        (goto-char (point-min))
        (call-interactively #'hl-todo-next))
    (let ((before-pt (point)))
      (ignore-errors (call-interactively #'hl-todo-next))
      (if (not (= (point) before-pt))
          (setq jcs-hl-todo-not-found-next nil)
        (setq jcs-hl-todo-not-found-next t)
        (if no-prompt
            (jcs-hl-todo-next)
          (message "%s" (propertize "user-error: No more matches :: overwrap"
                                    'face '(:foreground "cyan"))))))))

;;
;; (@* "Truncate Lines" )
;;

(defun jcs-enable-truncate-lines ()
  "Enable truncate lines."
  (interactive)
  (jcs-mute-apply (toggle-truncate-lines 1))
  (when (eq this-command 'jcs-enable-truncate-lines)
    (message "Truncate long lines enabled")))

(defun jcs-disable-truncate-lines ()
  "Disable truncate lines."
  (interactive)
  (jcs-mute-apply (toggle-truncate-lines -1))
  (when (eq this-command 'jcs-disable-truncate-lines)
    (message "Truncate long lines disabled")))

;;
;; (@* "Yascroll" )
;;

(defun jcs-reset-yascroll-color-by-theme ()
  "Reset yascroll color base on the theme color."
  (interactive)
  (let ((target-color (if (jcs-is-light-theme-p) "#C2C3C9" "#686868")))
    (if (display-graphic-p)
        (set-face-attribute 'yascroll:thumb-fringe nil
                            :background target-color :foreground target-color)
      (set-face-attribute 'yascroll:thumb-text-area nil
                          :background target-color :foreground target-color))))

;;
;; (@* "Yasnippet" )
;;

(defun jcs-yas-expand ()
  "Yasnippet expand current point."
  (interactive)
  (require 'yasnippet-snippets)
  (call-interactively #'yas-expand))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Load files.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;; Utilities
(require 'jcs-math)
(require 'jcs-util)
(require 'jcs-frame)
(require 'jcs-window)
(jcs-with-eval-after-load-multiple '(shell eshell) (require 'jcs-shell))
(with-eval-after-load 'feebleline (require 'jcs-feebleline))
(with-eval-after-load 'ivy (require 'jcs-ivy))
(with-eval-after-load 'lsp-mode (require 'jcs-lsp))

;; Editing
(add-hook 'Buffer-menu-mode-hook (lambda () (require 'jcs-buffer-menu)))
(with-eval-after-load 'dashboard (require 'jcs-dashboard))
(require 'jcs-nav)
(require 'jcs-edit)
(require 'jcs-comment)
(require 'jcs-vs)

;; For Specific Mode
(with-eval-after-load 'web-mode (require 'jcs-web))

(provide 'jcs-function)
;;; jcs-function.el ends here
