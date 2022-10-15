;;; jcs-function.el --- Self defines function  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "Eval" )
;;

(jcs-advice-add '(eval-buffer eval-defun eval-region) :after (deactivate-mark))

;;
;; (@* "Navigate to Error" )
;;

(jcs-advice-add '(push-button compile-goto-error) :around
  ;; Exection runs after navigate buffer that is different than the caller.
  (let ((prev-buf (current-buffer)))
    (apply arg0 args)
    (unless (eq prev-buf (current-buffer))  ; Different button, recenter it.
      (jcs-recenter-top-bottom 'middle))))

;;
;; (@* "Advices" )
;;

(defun jcs--recenter--advice-after (&rest _)
  "Advice for commands that we do recenter after execution."
  (call-interactively #'recenter))

;;
;; (@* "*Backtrace*" )
;;

(defconst jcs-backtrace-buffer-name "*Backtrace*"
  "Name of the backtrace buffer.")

(defun jcs-hit-backtrace ()
  "Do stuff when backtrace occures."
  (jcs-red-mode-line)  ; When error, turn red
  (msgu-inhibit-log
    (message "[INFO] Oops, error occurs! Please see backtrace for more information")))

(defun jcs-backtrace-occurs-p ()
  "Check if the backtrace occurs."
  (jcs-with-current-buffer jcs-backtrace-buffer-name
    (not (string-empty-p (buffer-string)))))

(defun jcs-backtrace-exit ()
  "Exit backtrace."
  (jcs-when-buffer-window jcs-backtrace-buffer-name
    (let (buffer-read-only) (erase-buffer) (bury-buffer))
    (unless (window-full-height-p) (delete-window))))

;;
;; (@* "*Messages*" )
;;

(defun jcs-messages ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun jcs-messages-other-window ()
  "Switch to `*Messages*' buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window (messages-buffer)))

(defun jcs-messages-maybe-kill-this-buffer ()
  "Erase the *Messages* buffer."
  (interactive)
  ;; Message one message to retrieve `*Message*' buffer prepare for next use.
  ;; Or else it some operation might prompt some issue that needed `*Message*'
  ;; buffer to be exists.
  (when (jcs-maybe-kill-this-buffer)
    (message ".") (jcs-messages-erase-buffer)))

(defun jcs-messages-erase-buffer ()
  "Reopen *Messages* buffer."
  (interactive)
  (with-current-buffer (messages-buffer)
    (let (buffer-read-only)
      (erase-buffer)
      (insert (format "Retrieving %s buffer..\n" (buffer-name)))
      (message nil))))  ; clear echo area

;;
;; (@* "*scratch*" )
;;

(defun jcs-scratch-buffer-p ()
  "Return non-nil if current buffer the scratch buffer."
  (equal (current-buffer) (get-scratch-buffer-create)))

(defun jcs-scratch-other-window ()
  "Start a new scratch buffer."
  (interactive)
  (jcs-switch-to-buffer-other-window (get-scratch-buffer-create)))

(defun jcs-new-scratch-buffer ()
  "Start a new scratch buffer."
  (interactive)
  (scratch-buffer)
  (erase-buffer)
  (ignore-errors (insert (substitute-command-keys initial-scratch-message)))
  (goto-char (point-min))
  (lisp-interaction-mode))

(defun jcs-scratch-buffer-maybe-kill ()
  "Kill buffer scratch."
  (interactive)
  (if (jcs-scratch-buffer-p)
      (progn
        (jcs-funcall-fboundp #'undo-tree-kill-visualizer)
        (jcs-bury-buffer))
    (jcs-maybe-kill-this-buffer)))

(defun jcs-scratch-buffer-refresh ()
  "Refresh scratch buffer."
  (interactive)
  (if (jcs-scratch-buffer-p) (jcs-new-scratch-buffer) (jcs-reopen-this-buffer)))

;;
;; (@* "Buffer Menu" )
;;

(defun jcs-buffer-menu-p ()
  "Check if current major mode `buffer-menu'."
  (eq major-mode 'Buffer-menu-mode))

(defun jcs-buffer-menu-refresh-buffer ()
  "Update buffer menu buffer."
  (interactive)
  (jcs-when-buffer-window diminish-buffer-menu-name (msgu-silent (buffer-menu))))

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
;; (@* "Comment" )
;;

(defun jcs-comment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)
  (if (and mark-active (/= (point) (mark)))
      (comment-region (region-beginning) (region-end))
    (comment-region (line-beginning-position) (line-end-position))))

(defun jcs-uncomment-region-or-line ()
  "If no region selected then just comment the line."
  (interactive)
  (if (and mark-active (/= (point) (mark)))
      (uncomment-region (region-beginning) (region-end))
    (uncomment-region (line-beginning-position) (line-end-position))))

;;
;; (@* "Dashboard" )
;;

(defun jcs-dashboard ()
  "Jump to dashboard buffer; if doesn't exists create one."
  (interactive)
  (switch-to-buffer dashboard-buffer-name)
  (unless (eq major-mode 'dashboard-mode) (dashboard-mode))
  (jcs-dashboard-refresh-buffer))

(defun jcs-dashboard-other-window ()
  "Switch to dashboard buffer on other window."
  (interactive)
  (jcs-with-other-window (jcs-dashboard)))

(defun jcs-dashboard-refresh-buffer ()
  "Refresh dashboard buffer."
  (interactive)
  (jcs-when-buffer-window dashboard-buffer-name
    (jcs-with-dashboard-last-path
      (msgu-silent
        (jcs-save-window-excursion (dashboard-refresh-buffer))))))

(jcs-advice-add 'dashboard-remove-item-under :around
  (jcs-with-dashboard-last-path (apply arg0 args)))

(defun jcs-dashboard--get-banner-path ()
  "Return banner path."
  (concat
   user-emacs-directory "banners/"
   (cond (elenv-graphic-p (if (jcs-light-theme-p) "sink/black.png" "sink/white.png"))
         (t "sink.txt"))))

;;
;; (@* "ElDoc" )
;;

(defun jcs-eldoc-message-now () "Show eldoc message now." (interactive))

(jcs-advice-add 'eldoc--message-command-p :override
  ;; One can also loop through `eldoc-message-commands' and empty it out
  (memq arg0
        '(jcs-eldoc-message-now
          mouse-set-point
          vsc-edit-real-space vsc-edit-smart-space vsc-edit-space
          vsc-edit-real-backspace vsc-edit-smart-backspace vsc-edit-backspace
          previous-line next-line
          vs-edit-previous-line vs-edit-next-line
          jcs-py-indent-up jcs-py-indent-down
          left-char right-char
          jcs-smart-forward-word jcs-smart-backward-word
          jcs-backward-word-capital jcs-forward-word-capital
          beginning-of-line end-of-line
          jcs-beginning-of-line jcs-end-of-line)))

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
      ((nxml-mode xml-mode web-mode html-mode) (sgml-pretty-print start end))
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
      (jcs-jump-to-buffer-windows
       (buffer-name flycheck-error-list-source-buffer)
       :success #'jcs-flycheck-mode
       :error #'jcs-maybe-kill-this-buffer)
    (call-interactively #'flycheck-mode)
    (if flycheck-mode
        (progn
          (save-window-excursion (call-interactively #'flycheck-list-errors))
          (with-selected-window (get-largest-window nil nil t)
            (switch-to-buffer flycheck-error-list-buffer)))
      (jcs-when-buffer-window flycheck-error-list-buffer
        (jcs-maybe-kill-this-buffer))))
  flycheck-mode)

;;
;; (@* "Tab Bar" )
;;

(defun jcs-toggle-tabbar-mode ()
  "Toggle tab bar."
  (interactive)
  (jcs-enable-disable-mode-if #'centaur-tabs-mode (not centaur-tabs-mode)))

;;
;; (@* "Tips" )
;;

(defconst jcs-pop-tooltip-buffer-name "*jcs:pop-tooltip*"
  "Buffer name for tooltip.")

(defun jcs-pop-tooltip--next ()
  "Hide tooltip after first post command."
  (posframe-hide jcs-pop-tooltip-buffer-name)
  (remove-hook 'post-command-hook #'jcs-pop-tooltip--next))

(defun jcs-pop-tooltip--post ()
  "Register for next post command."
  (add-hook 'post-command-hook #'jcs-pop-tooltip--next)
  (remove-hook 'post-command-hook #'jcs-pop-tooltip--post))

(cl-defun jcs-pop-tooltip (string &key point (timeout 300) (height 30))
  "Pop up an tooltip depends on the graphic used.

STRING is the content of the toolip. The location POINT. TIMEOUT for not forever
delay. HEIGHT of the tooltip that will display."
  (jcs-require '(asoc pos-tip popup))
  (let ((bg (asoc-get company-box-doc-frame-parameters 'background-color))
        (fg (asoc-get company-box-doc-frame-parameters 'foreground-color))
        (fringe-width 10))
    (if elenv-graphic-p
        (progn
          (with-current-buffer (get-buffer-create jcs-pop-tooltip-buffer-name)
            (let ((text-scale-mode-step 1.1)) (text-scale-set company-box-doc-text-scale-level)))
          (posframe-show jcs-pop-tooltip-buffer-name :string string :position point
                         :timeout timeout
                         :background-color bg :foreground-color fg
                         :internal-border-width 1
                         :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                         :left-fringe fringe-width :right-fringe fringe-width)
          (add-hook 'post-command-hook #'jcs-pop-tooltip--post))
      (popup-tip string :point point :around t :height height :scroll-bar t :margin t))
    t))

(defun jcs--describe-symbol-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (save-window-excursion
      (with-current-buffer (get-buffer-create "*Help*")
        (let (buffer-read-only) (erase-buffer))
        (msgu-silent (describe-symbol thing))
        (buffer-string)))))

(defun jcs-tip-describe-it ()
  "Describe symbol at point."
  (let ((desc (jcs--describe-symbol-string)))
    (if (or (string-empty-p desc) (string= (string-trim desc) "[back]"))
        (error "[ERROR] No description at point")
      (jcs-pop-tooltip desc :point (point)))))

(defun jcs-describe-thing-in-popup ()
  "Show current symbol info."
  (interactive)
  (jcs-funcall-fboundp #'company-abort)
  (if (jcs--lsp-connected-p)
      (progn (require 'lsp-ui)
             (or (ignore-errors (call-interactively #'lsp-ui-doc-glance))
                 (ignore-errors (call-interactively #'lsp-ui-doc-show))))
    (cond ((ignore-errors (jcs-tip-describe-it)))
          ((ignore-errors (preview-it)))
          (t (define-it-at-point)))
    ;; In case we are using region, cancel the select region.
    (deactivate-mark)))

(provide 'jcs-function)
;;; jcs-function.el ends here
