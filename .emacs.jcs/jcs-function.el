;;; jcs-function.el --- Self defines function.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; *Messages*

;;;###autoload
(defun jcs-message-buffer ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;###autoload
(defun jcs-message-buffer-other-window ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer-other-window "*Messages*"))

;;;###autoload
(defun jcs-message-erase-buffer ()
  "Erase the *Messages* buffer."
  (interactive)
  (let ((is-killed nil))
    ;; Kill it first.
    (setq is-killed (jcs-maybe-kill-this-buffer))

    ;; Message one message to retrieve `*Message*' buffer
    ;; prepare for next use. Or else it some operation
    ;; might prompt some issue that needed `*Message*'
    ;; buffer to be exists.
    (message "Retrieve *Message* buffer..")

    (when is-killed
      (jcs-safe-jump-shown-to-buffer
       "*Buffer List*"
       ;; NOTE: Refresh buffer menu once.
       (lambda () (buffer-menu))))))

;;;###autoload
(defun jcs-message-erase-buffer-stay ()
  "Reopen *Messages* buffer."
  (interactive)
  (jcs-message-erase-buffer)
  (switch-to-buffer "*Messages*"))

;;----------------------------------------------------------------------------
;; *scratch*

;;;###autoload
(defun jcs-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;;;###autoload
(defun jcs-scratch-buffer-other-window ()
  "Start a new scratch buffer."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))

;;;###autoload
(defun jcs-new-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (jcs-scratch-buffer)
  (erase-buffer)
  (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.\n")
  (insert ";; To create a file, visit it with <open> and enter text in its buffer.\n\n")
  (goto-char (point-min)))

;;;###autoload
(defun jcs-scratch-buffer-maybe-kill ()
  "Kill buffer scratch."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (jcs-bury-buffer)
    (jcs-maybe-kill-this-buffer)))

;;;###autoload
(defun jcs-scratch-buffer-refresh ()
  "Refresh scratch buffer."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (jcs-new-scratch-buffer)
    (jcs-reopen-this-buffer)))

;;----------------------------------------------------------------------------
;; ag

(defun jcs-ag-get-search-string-by-current-buffer-name ()
  "Get the `search-string' by the current `buffer-name'."
  (let* ((mode-name-splits (split-string (buffer-name) " "))
         (regexp-str (nth 2 mode-name-splits))
         (regexp-lst '())
         (search-string nil))
    (when regexp-str
      (setq regexp-lst (split-string regexp-str ":"))
      (setq search-string (nth 1 regexp-lst)))
    search-string))

;;;###autoload
(defun jcs-ag-refresh-search ()
  "Refresh the search result by searching the `regexp' once again."
  (interactive)
  (let ((search-string (jcs-ag-get-search-string-by-current-buffer-name)))
    (ag-project-regexp search-string)
    (jcs-wgrep-change-to-wgrep-mode)
    (message "Refresh search result with regexp: %s" search-string)))

;;;###autoload
(defun jcs-ag-project-regexp ()
  "Use `wgrep' to replace the word in the entire project."
  (interactive)
  ;; open search result menu.
  (call-interactively #'ag-project-regexp)
  (jcs-jump-shown-to-buffer "*ag")
  ;; Make result menu editable.
  (jcs-wgrep-change-to-wgrep-mode))

;;----------------------------------------------------------------------------
;; Autio Highlight Symbol

(defun jcs--ahs--set-face (pt pt-box ot ot-box)
  "Set `auto-highlight-symbol' face's with PT, PT-BOX, OT, OT-BOX."
  (let ((box-face '()))
    (setq box-face (jcs-form-p-symbol box-face :line-width -1))
    (setq box-face (jcs-form-p-symbol box-face :style 'pressed-button))
    (let ((box-face box-face))
      (setq box-face (jcs-form-p-symbol box-face :color pt-box))
      ;; Current highlight. (Cursor point currently on.)
      (set-face-attribute 'ahs-plugin-defalt-face nil
                          :foreground nil
                          :background pt
                          :box box-face
                          :underline nil))
    (setq box-face (jcs-form-p-symbol box-face :color ot-box))
    ;; Other highlight. (Same words in the buffer)
    (set-face-attribute 'ahs-face nil
                        :foreground nil
                        :background ot
                        :box box-face
                        :underline nil)
    (set-face-attribute 'ahs-definition-face nil
                        :foreground nil
                        :background ot
                        :box box-face
                        :underline nil)))

(defun jcs-reset-ahs-by-theme ()
  "Reset `auto-highlight-symbol' by theme."
  (if (jcs-is-light-color (face-background 'default))
      (jcs--ahs--set-face "#E2E6D6" "#525D68"
                          "#DDE2CD" "#525D68")
    (jcs--ahs--set-face "#123E70" "#525D68"
                        "#113D6F" "#525D68")))

;;----------------------------------------------------------------------------
;; Buffer Menu

(defconst jcs-buffer-menu-search-title "Search: "
  "Search bar title in `buffer-menu''s buffer.")

(defvar jcs-buffer-menu-return-delay nil
  "Record if hit return when display not ready; once it is ready we redo the action.")

(defun jcs--buffer-menu--advice-before (&rest _)
  "Advice before execute `buffer-menu' command."
  (setq jcs-buffer-menu-return-delay nil)
  (setq tabulated-list--header-string jcs-buffer-menu-search-title))
(advice-add 'buffer-menu :before #'jcs--buffer-menu--advice-before)


(defvar jcs-buffer-menu-switch-buffer-refreshing nil
  "Flag to check if current buffer menu refresing.")

;;;###autoload
(defun jcs-buffer-menu-refresh-buffer ()
  "Update buffer menu buffer."
  (interactive)
  (unless (string= (jcs-buffer-name-or-buffer-file-name) "*Buffer List*")
    (save-window-excursion
      (let ((was-fake-header-printed nil) (get-title ""))
        (when (get-buffer "*Buffer List*")
          (with-current-buffer "*Buffer List*"
            (save-excursion
              (goto-char (point-min))
              (setq get-title (thing-at-point 'line)))
            (when get-title
              (setq was-fake-header-printed (string-match-p jcs-buffer-menu-search-title get-title)))))
        (let (tabulated-list--header-string) (jcs-mute-apply #'buffer-menu))
        (when jcs-buffer-menu-switch-buffer-refreshing
          (jcs--buffer-menu-trigger-filter was-fake-header-printed)))
      (bury-buffer)))
  (with-current-buffer "*Buffer List*"
    (require 'diminish-buffer)
    (diminish-buffer--refresh-buffer-menu)))

(defun jcs-buffer-menu-safe-refresh ()
  "Safely refresh `buffer menu`'s buffer."
  (unless jcs-buffer-menu-switch-buffer-refreshing
    (let ((jcs-buffer-menu-switch-buffer-refreshing t))
      (jcs-buffer-menu-refresh-buffer))))

;;----------------------------------------------------------------------------
;; Calculator

;;;###autoload
(defun jcs-calc-eval-region ()
  "Eval the arithmetic expression in the region and replace it with the result."
  (interactive)
  (if (not (use-region-p))
      (message "Trying to use calc eval but with no region selected")
    (let ((val (calc-eval (buffer-substring (region-beginning) (region-end)))))
      (delete-region (region-beginning) (region-end))
      (insert val))))

;;----------------------------------------------
;; Cheat Sheet

;;;###autoload
(defun jcs-alt-codes-table ()
  "Display basic Alt-Codes table."
  (interactive)
  (jcs-display-file "~/.emacs.jcs/data/charset/alt-code.txt" "*Alt Codes*" nil))

;;;###autoload
(defun jcs-ascii-table ()
  "Display basic ASCII table."
  (interactive)
  (jcs-display-file "~/.emacs.jcs/data/charset/ascii.txt" "*ASCII*" nil))

;;;###autoload
(defun jcs-algorithm-cheat-sheet ()
  "Display basic Alt-Codes table."
  (interactive)
  (jcs-html-preview "~/.emacs.jcs/data/algorithm/cheat-sheet.html" "*Algorithm Cheat Sheet*" nil))

;;;###autoload
(defun jcs-data-structure-cheat-sheet ()
  "Display basic Alt-Codes table."
  (interactive)
  (jcs-display-file "~/.emacs.jcs/data/data-structure/cheat-sheet.txt" "*Data Structure Cheat Sheet*" nil))

;;----------------------------------------------------------------------------
;; Dashboard

;;;###autoload
(defun jcs-dashboard (&optional ow)
  "Jump to the dashboard buffer, if doesn't exists create one.
OW is the other window flag."
  (interactive)
  (if ow
      (switch-to-buffer-other-window dashboard-buffer-name)
    (switch-to-buffer dashboard-buffer-name))
  (unless (jcs-is-current-major-mode-p "dashboard-mode")
    (dashboard-mode))
  (jcs-dashboard-refresh-buffer))

;;;###autoload
(defun jcs-dashboard-other-window ()
  "Just like `jcs-dashboard', but open on the other window."
  (interactive)
  (jcs-dashboard t))

;;;###autoload
(defun jcs-dashboard-refresh-buffer ()
  "Update dashboard buffer by killing it and start a new one."
  (interactive)
  (jcs-mute-apply
   (lambda ()
     (let ((db-id-lst (jcs-get-window-id-by-buffer-name dashboard-buffer-name))
           (buf-lns '()) (buf-cls '()) (index 0))
       (save-selected-window
         (dolist (win-id db-id-lst)
           (jcs-ace-select-window win-id)
           (push (line-number-at-pos) buf-lns)
           (push (current-column) buf-cls)))
       (setq buf-lns (reverse buf-lns))
       (setq buf-cls (reverse buf-cls))
       (when (jcs-buffer-exists-p dashboard-buffer-name)
         (kill-buffer dashboard-buffer-name))
       (dashboard-insert-startupify-lists)
       (save-selected-window
         (dolist (win-id db-id-lst)
           (jcs-ace-select-window win-id)
           (switch-to-buffer dashboard-buffer-name)
           (jcs-goto-line (nth index buf-lns))
           (move-to-column (nth index buf-cls))
           (setq index (1+ index))))))))

;;;###autoload
(defun jcs-dashboard-maybe-kill-this-buffer ()
  "Kill the dashboard buffer then open the new one immediately."
  (interactive)
  (jcs-maybe-kill-this-buffer)
  (jcs-dashboard-refresh-buffer)
  (jcs-buffer-menu-refresh-buffer))

;;;###autoload
(defun jcs-reset-dashboard-banner-by-theme ()
  "Reset dashboard banner."
  (interactive)
  (if (jcs-is-light-color (face-background 'default))
      (setq dashboard-startup-banner "~/.emacs.jcs/banner/sink_black.png")
    (setq dashboard-startup-banner "~/.emacs.jcs/banner/sink_white.png"))
  (let ((logo-title-fg "cyan1")
        (heading-fg "#17A0FB")
        (wb-fg "light steel blue"))
    (when (jcs-is-light-color (face-background 'default))
      (setq logo-title-fg "#616161")
      (setq heading-fg "#727272")
      (setq wb-fg "#1475B7"))
    (jcs--set-common-face 'dashboard-banner-logo-title logo-title-fg)
    (jcs--set-common-face 'dashboard-heading heading-fg)
    (set-face-attribute 'widget-button nil :weight 'normal :foreground wb-fg))
  (jcs-dashboard-refresh-buffer))

;;----------------------------------------------------------------------------
;; ElDoc

;;;###autoload
(defun jcs-eldoc-message-now () "Show eldoc message now." (interactive))

(defun jcs-eldoc--message-command-p (command)
  "Advice overwrite `eldoc--message-command-p' COMMAND."
  ;; One can also loop through `eldoc-message-commands' and empty it out
  (memq command '(jcs-eldoc-message-now
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

;;----------------------------------------------------------------------------
;; Electric Pair

(defun jcs-make-electric-pair-pairs-local (lst-pr)
  "Append a list of pair to local mode.
LST-PR: List of pair."
  (setq-local electric-pair-pairs (append electric-pair-pairs lst-pr))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

;;----------------------------------------------------------------------------
;; Iedit

;;;###autoload
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

;;----------------------------------------------------------------------------
;; Line Numbers

;;;###autoload
(defun jcs-update-line-number-each-window ()
  "Update each window's line number mode."
  (interactive)
  (jcs-walk-through-all-windows-once
   (lambda ()
     (jcs-active-line-numbers-by-mode))))

(defun jcs-safe-display-line-numbers (act)
  "Active `display-line-numbers' by ACT."
  (require 'display-line-numbers)
  (if (and (numberp act) (>= act 1))
      (unless display-line-numbers-mode (display-line-numbers-mode 1))
    (when display-line-numbers-mode (display-line-numbers-mode ac-1))))

(defun jcs-safe-display-linum (act)
  "Active `linum' by ACT."
  (require 'linum)
  (if (and (numberp act) (>= act 1))
      (unless linum-mode (linum-mode 1))
    (when linum-mode (linum-mode -1))))

;;;###autoload
(defun jcs-active-line-numbers-by-mode ()
  "Active line number by mode."
  (interactive)
  (require 'line-reminder)
  (if (or (minibufferp)
          (and (jcs-is-contain-list-string-regexp jcs-line-numbers-ignore-buffers (buffer-name))
               (not (jcs-is-contain-list-string jcs-line-numbers-ignore-buffer-exceptions (buffer-name))))
          (jcs-is-contain-list-string jcs-line-numbers-ignore-modes (symbol-name major-mode)))
      (progn
        (when line-reminder-mode (line-reminder-mode -1))
        (if (display-graphic-p)
            (jcs-safe-display-line-numbers -1)
          (jcs-safe-display-linum -1)))
    (unless line-reminder-mode (line-reminder-mode 1))
    (if (display-graphic-p)
        (jcs-safe-display-line-numbers 1)
      (jcs-safe-display-linum 1))))

;;----------------------------------------------------------------------------
;; Media

;;;###autoload
(defun jcs-media-find-file ()
  "Open the media file."
  (interactive)
  (require 'jcs-media)
  (let ((do-play nil) (media-path nil))
    (if ffmpeg-player--buffer
        (when  (yes-or-no-p "There is video playing, kill it? ")
          (jcs-safe-jump-shown-to-buffer
           "[*]ffmpeg-player[*]: "
           (lambda () (jcs-media-close-media-window))
           (lambda ()
             (with-current-buffer ffmpeg-player--buffer
               (jcs-media-close-media-window))))
          (setq do-play t))
      (setq do-play t))
    (when do-play
      (setq media-path (jcs-select-file))
      (when media-path
        (save-window-excursion (ffmpeg-player-video media-path))
        (jcs-media--open-media-window)))))

;;----------------------------------------------------------------------------
;; Minimap

;;;###autoload
(defun jcs-toggle-minimap ()
  "Toggle minimap."
  (interactive)
  (user-error "Minimap no longer supported in this configuration"))

;;----------------------------------------------------------------------------
;; Prettify/Minify

;;;###autoload
(defun jcs-prettify-buffer-contents ()
  "Prettify the buffer contents by file type."
  (interactive)
  (require 'sgml-mode)
  (cl-case major-mode
    ('json-mode (json-reformat-region (point-min) (point-max)))
    ('web-mode (sgml-pretty-print (point-min) (point-max)))
    (t (user-error "No prettify command in this context"))))

;;;###autoload
(defun jcs-minify-buffer-contents ()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

;;----------------------------------------------------------------------------
;; Re-Builder

;;;###autoload
(defun jcs-reb-maybe-kill-this-buffer ()
  "Kill this buffer in `re-builder' mode."
  (interactive)
  (let ((is-killed nil))
    ;; maybe kill this buffer.
    (setq is-killed (jcs-maybe-kill-this-buffer))
    (when is-killed
      ;; then delete this window.
      (delete-window))))

;;;###autoload
(defun jcs-re-builder (type)
  "Rewrap `re-builder' function.
TYPE : enable/disable case sensitive?"
  (interactive
   (list (completing-read
          "Enable case sensitive?" '("Case Sensitive"
                                     "Case Insensitive"))))

  (if (string= type "Case Sensitive")
      (setq case-fold-search nil)
    (setq case-fold-search t))  ;; This is default.

  ;; Start `RE-Builder' mode.
  (re-builder)

  ;; Set back to default.
  (setq case-fold-search t))

;;----------------------------------------------------------------------------
;; Shift Select

;;;###autoload
(defun jcs-toggle-shift-select-mode ()
  "Toggle `shift-select-mode'."
  (interactive)
  (if shift-select-mode
      (jcs-disable-shift-select-mode)
    (jcs-enable-shift-select-mode)))

;;;###autoload
(defun jcs-enable-shift-select-mode ()
  "Enable `shift-select-mode'."
  (interactive)
  (setq shift-select-mode t))

;;;###autoload
(defun jcs-disable-shift-select-mode ()
  "Enable `shift-select-mode'."
  (interactive)
  (setq shift-select-mode nil))

;;----------------------------------------------------------------------------
;; Speedbar

(defvar jcs-speedbar-opening-buffer-file-name nil
  "Record down the current speedbar is opening which buffer.")

;;;###autoload
(defun jcs-speedbar-edit-line ()
  "Customize `speedbar-edit-line' function."
  (interactive)
  (let ((is-opening-a-file t))
    (save-excursion
      (beginning-of-line)

      ;; Weird character infront, ignore them by moving forward.
      (forward-char 1)  ;; 0
      (forward-char 1)  ;; :
      (forward-char 1)  ;; < or [, < stand for directory and [ stand for file.

      (when (jcs-current-char-equal-p "<")
        (setq is-opening-a-file nil)))

    ;; Call it normally.
    (call-interactively #'speedbar-edit-line)

    (call-interactively #'sr-speedbar-select-window)

    ;; If is file..
    (when is-opening-a-file
      (let (;; Preserve the previous selected window.
            (record-selected-window jcs-sr-speedbar-record-selected-window))
        ;; Back to previous select window.
        ;;
        ;; NOTE: This will change the previous selected window value.
        (jcs-other-window-prev)

        ;; Record the opening buffer/file down.
        (setq jcs-speedbar-opening-buffer-file-name (buffer-file-name))

        ;; Maybe kill it, because we are going to open it in originally
        ;; selected window instead of the default window after close we
        ;; the `speedbar' window.
        ;;
        ;; NOTE: This seems like it will change the
        ;; `jcs-sr-speedbar-record-selected-window', value by calling
        ;; `jcs-other-window-next' or `jcs-other-window-prev' functions.
        ;; So we also need to wrap this function inside the `let' operation.
        (jcs-maybe-kill-this-buffer)

        ;; Restore previous selected window.
        (setq jcs-sr-speedbar-record-selected-window record-selected-window))

      ;; Close the speedbar window.
      (jcs-sr-speedbar-toggle))))

(defvar jcs-sr-speedbar-window-all-on-right t
  "Make speedbar open on the right of all window.")

(defvar jcs-sr-speedbar-record-selected-window nil
  "Record down the current selected window before toggle.")

;;;###autoload
(defun jcs-sr-speedbar-toggle ()
  "Toggle the speedbar window."
  (interactive)
  (require 'sr-speedbar)
  (if (sr-speedbar-exist-p)
      (progn
        ;; Close it.
        (call-interactively #'sr-speedbar-toggle)

        ;; Go back to previous selected/editing window.
        (select-window jcs-sr-speedbar-record-selected-window)

        ;; Try to open a recorded opening file.
        (when jcs-speedbar-opening-buffer-file-name
          (find-file jcs-speedbar-opening-buffer-file-name)))
    (setq jcs-sr-speedbar-record-selected-window (selected-window))

    (let ((default-directory default-directory)
          (pro-dir (cdr (project-current))))
      ;; NOTE: Use current buffer directory as default.
      (when (buffer-file-name)
        (setq default-directory (f-dirname (buffer-file-name))))

      ;; NOTE: If found project directory, use project directory.
      (when pro-dir
        (setq default-directory pro-dir))

      ;; Esure speedbar is active.
      (call-interactively #'sr-speedbar-toggle)
      (call-interactively #'sr-speedbar-toggle)

      ;; Refresh the speedbar object after the `default-directory'
      ;; has been set.
      (call-interactively #'speedbar-refresh)

      ;; Goto very right/left of the window.
      (if jcs-sr-speedbar-window-all-on-right
          (jcs-move-to-rightmost-window nil)
        (jcs-move-to-leftmost-window nil))

      ;; Open it.
      (call-interactively #'sr-speedbar-toggle))

    ;; Select the speedbar window.
    (call-interactively #'sr-speedbar-select-window)))

(defun jcs-update-speedbar-record-after-select-new-window ()
  "Update speedbar by selecting new window."
  (when (and (functionp 'sr-speedbar-exist-p)
             (sr-speedbar-exist-p)
             (not (jcs-is-current-major-mode-p "speedbar-mode")))
    (setq jcs-sr-speedbar-record-selected-window (selected-window))))

;;----------------------------------------------------------------------------
;; Syntax Check

;;;###autoload
(defun jcs-flycheck-mode ()
  "Flycheck mode toggle."
  (interactive)
  (require 'flycheck)
  (if (string= (buffer-name) flycheck-error-list-buffer)
      (if (ignore-errors (jcs-jump-shown-to-buffer (buffer-name flycheck-error-list-source-buffer)))
          (jcs-flycheck-mode)
        (jcs-maybe-kill-this-buffer))
    (call-interactively #'flycheck-mode)
    (if flycheck-mode
        (call-interactively #'flycheck-list-errors)
      (save-selected-window
        (when (ignore-errors (jcs-jump-shown-to-buffer flycheck-error-list-buffer))
          (jcs-maybe-kill-this-buffer))))
    ;; STUDY: For some reason, we need to walk
    ;; through all windows once in order to display
    ;; the `flycheck-list-errors' in other window.
    (jcs-walk-through-all-windows-once))
  flycheck-mode)

;;----------------------------------------------------------------------------
;; Tab Bar

;;;###autoload
(defun jcs-toggle-tabbar-mode ()
  "Toggle tab bar."
  (interactive)
  (if centaur-tabs-mode
      (centaur-tabs-mode -1)
    (centaur-tabs-mode 1))
  ;; Loop through all window so all windows take effect.
  (jcs-buffer-visible-list))

;;----------------------------------------------------------------------------
;; Tab Width

;;;###autoload
(defun jcs-inc-tab-width ()
  "Increase tab width by 2."
  (interactive)
  (jcs-delta-tab-width 2)
  (indent-for-tab-command))

;;;###autoload
(defun jcs-dec-tab-width ()
  "Decrease tab width by 2."
  (interactive)
  (jcs-delta-tab-width -2)
  (indent-for-tab-command))

;;----------------------------------------------------------------------------
;; Terminal / Shell

;;;###autoload
(defun jcs-toggle-shell-window ()
  "Toggle Shell Command prompt."
  (interactive)
  (require 'jcs-shell)
  (if (ignore-errors (jcs-jump-shown-to-buffer (multi-shell--prefix-name)))
      (jcs-hide-shell-window)
    (jcs-show-shell-window)))

;;;###autoload
(defun jcs-shell-new-shell ()
  "Create a new shell window."
  (interactive)
  (require 'jcs-shell)
  (if (ignore-errors (jcs-jump-shown-to-buffer (multi-shell--prefix-name)))
      (progn
        (other-window -2)
        (other-window 1)
        (multi-shell))
    (jcs-show-shell-window)))

;;----------------------------------------------------------------------------
;; Zoom

;;;###autoload
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

;;;###autoload
(defun jcs-text-scale-increase ()
  "Scale the text up."
  (interactive)
  (jcs-text-scale-delta 1))

;;;###autoload
(defun jcs-text-scale-decrease ()
  "Scale the text down."
  (interactive)
  (jcs-text-scale-delta -1))

;;----------------------------------------------------------------------------
;; Tips

(cl-defun jcs-pop-tooltip (string &key point (timeout 300) (height 30))
  "Pop up an tooltip depends on the graphic used.

STRING is the content of the toolip. The location POINT. TIMEOUT for not forever
delay. HEIGHT of the tooltip that will display."
  (if (display-graphic-p)
      (progn
        (require 'pos-tip)
        (pos-tip-show string `(,company-quickhelp-color-foreground . ,company-quickhelp-color-background) point nil timeout))
    (require 'popup)
    (popup-tip string :point point :around t :height height :scroll-bar t :margin t)))

(defun jcs--describe-symbol-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (with-temp-buffer
      (jcs-mute-apply 'help-mode)
      (jcs-mute-apply 'describe-symbol thing)
      (buffer-string))))

(defun jcs-tip-describe-it ()
  "Describe symbol at point."
  (let* ((help-xref-following t)
         (description (jcs--describe-symbol-string))
         (timeout 300))
    (if (string-empty-p description)
        (error "[ERROR] No description at point")
      (jcs-pop-tooltip description :point (point) :timeout timeout))))

;;;###autoload
(defun jcs-describe-thing-in-popup ()
  "Show current symbol info."
  (interactive)
  (if (and (boundp 'flycheck-mode) flycheck-mode)
      (user-error "[INFO] Describe thing is unused when 'flycheck' is enabled")
    (unless (ignore-errors (jcs-tip-describe-it))
      (require 'define-it)
      (define-it-at-point))))

;;----------------------------------------------------------------------------
;; Todo

(defvar jcs-hl-todo-not-found-prev nil
  "See if found the previous `hl-todo' matches.")

(defvar jcs-hl-todo-not-found-next nil
  "See if found the next `hl-todo' matches.")

;;;###autoload
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

;;;###autoload
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

;;----------------------------------------------------------------------------
;; Truncate Lines

;;;###autoload
(defun jcs-enable-truncate-lines ()
  "Enable truncate lines."
  (interactive)
  (unless truncate-lines
    (toggle-truncate-lines)))

;;;###autoload
(defun jcs-disable-truncate-lines ()
  "Disable truncate lines."
  (interactive)
  (when truncate-lines
    (toggle-truncate-lines)))

;;----------------------------------------------------------------------------
;; wgrep

;;;###autoload
(defun jcs-wgrep-finish-edit ()
  "Wrap `wgrep-finish-edit' command from `wgrep-mode'."
  (interactive)
  (save-excursion
    (wgrep-finish-edit)
    (wgrep-change-to-wgrep-mode)))

;;;###autoload
(defun jcs-wgrep-change-to-wgrep-mode ()
  "Safely switch to `wgrep-mode'."
  (interactive)
  ;; NOTE: Here we try to change to `wgrep-mode' until we finally changed.
  (while (not (ignore-errors (wgrep-change-to-wgrep-mode)))
    (sleep-for 0.5)
    (message "Switching to `wgrep-mode'..."))
  (message "Switched to `wgrep-mode'."))

;;----------------------------------------------------------------------------
;; Yascroll

;;;###autoload
(defun jcs-reset-yascroll-color-by-theme ()
  "Reset yascroll color base on the theme color."
  (interactive)
  (let ((target-color "#424242"))
    (when (jcs-is-light-color (face-background 'default))
      (setq target-color "#C1C1C1"))
    (set-face-attribute 'yascroll:thumb-fringe
                        nil
                        :background target-color
                        :foreground target-color)))

;;----------------------------------------------------------------------------
;; Yasnippet

;;;###autoload
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
(jcs-with-eval-after-load-multiple
 '(shell eshell-mode) #'(lambda () (require 'jcs-shell)))
(with-eval-after-load 'feebleline (require 'jcs-feebleline-func))
(with-eval-after-load 'ivy (require 'jcs-ivy-func))

;; Editing
(add-hook 'Buffer-menu-mode-hook (lambda () (require 'jcs-buffer-menu)))
(with-eval-after-load 'dashboard (require 'jcs-dashboard))
(require 'jcs-nav)
(require 'jcs-edit)
(require 'jcs-comment)
(require 'jcs-vs-func)

;; For Specific Mode
(with-eval-after-load 'preproc-font-lock (require 'jcs-preproc-func))

(with-eval-after-load 'org (require 'jcs-org-func))
(with-eval-after-load 'cc-mode
  (require 'jcs-cc-func)
  (require 'jcs-java-func))
(with-eval-after-load 'csharp-mode (require 'jcs-csharp-func))
(with-eval-after-load 'make-mode (require 'jcs-make-func))
(with-eval-after-load 'lua-mode (require 'jcs-lua-func))
(with-eval-after-load 'nasm-mode (require 'jcs-nasm-func))
(with-eval-after-load 'python-mode (require 'jcs-python-func))
(with-eval-after-load 'sh-script (require 'jcs-sh-func))
(with-eval-after-load 'css-mode (require 'jcs-css-func))
(with-eval-after-load 'web-mode (require 'jcs-web-func))
(with-eval-after-load 'yaml-mode (require 'jcs-yaml-func))
(require 'jcs-oop-func)


(provide 'jcs-function)
;;; jcs-function.el ends here
