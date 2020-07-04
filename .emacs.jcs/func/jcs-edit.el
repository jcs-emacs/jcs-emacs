;;; jcs-edit.el --- When editing the file.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Undo / Redo

;;
;; NOTE: This is compatible with other text editor
;; or IDE. Most IDE/text editor have this undo/redo
;; system as default.
;;
(defvar jcs-use-undo-tree-key t
  "Using the undo tree key in stead of normal Emacs's undo key.
This variable must be use with `jcs-undo' and `jcs-redo' functions.")

;; NOTE: Active this will cause huge amount of performance,
;; consider this before active.
(defvar jcs-undo-tree-auto-show-diff nil
  "Show the difference code when undo tree minor mode is active.")

(defvar jcs--splits-windows nil
  "Flag to check if the window splits.")

;;;###autoload
(defun jcs-toggle-undo-tree-auto-show-diff ()
  "Toggle auto show diff functionality."
  (interactive)
  (if jcs-undo-tree-auto-show-diff
      (jcs-disable-undo-tree-auto-show-diff)
    (jcs-enable-undo-tree-auto-show-diff)))

;;;###autoload
(defun jcs-enable-undo-tree-auto-show-diff ()
  "Enable undo tree auto show diff effect."
  (interactive)
  (setq jcs-undo-tree-auto-show-diff t)
  (message "Enable undo tree auto show diff."))

;;;###autoload
(defun jcs-disable-undo-tree-auto-show-diff ()
  "Disable undo tree auto show diff effect."
  (interactive)
  (setq jcs-undo-tree-auto-show-diff nil)
  (message "Disable undo tree auto show diff."))


;;;###autoload
(defun jcs-toggle-undo-tree-key()
  "Toggle `jcs-use-undo-tree-key' boolean."
  (interactive)
  (if jcs-use-undo-tree-key
      (jcs-disable-undo-tree-key)
    (jcs-enable-undo-tree-key)))

;;;###autoload
(defun jcs-enable-undo-tree-key ()
  "Enable undo tree key.
This will replace usual Emacs' undo key."
  (interactive)
  (setq jcs-use-undo-tree-key t)
  (message "Enable undo tree key."))

;;;###autoload
(defun jcs-disable-undo-tree-key ()
  "Disable undo tree key.
This will no longer overwrite usual Emacs' undo key."
  (interactive)
  (setq jcs-use-undo-tree-key nil)
  (message "Disable undo tree key."))


(defun jcs-undo-kill-this-buffer ()
  "Kill the undo tree buffer."
  (require 'undo-tree)
  (jcs-safe-jump-shown-to-buffer
   undo-tree-visualizer-buffer-name
   (lambda ()
     (save-selected-window (undo-tree-visualizer-quit))
     (when jcs--splits-windows
       (delete-window)
       (setq jcs--splits-windows nil)))))

(defun jcs-undo-tree-visualize (&optional cbf)
  "Call `undo-tree-visualize' only in window that has higher height.
CBF : Current buffer file name."
  (let ((jcs--no-advice-other-window t)
        (win-len (jcs-count-windows)) (win-index 0)
        (target-window nil)
        (rel-cbf (if cbf cbf (buffer-name)))
        (current-window (selected-window)))
    (when (< win-len 2)
      (jcs-balance-split-window-horizontally)
      (setq jcs--splits-windows t))
    (save-selected-window
      (other-window 1)
      (jcs-walk-through-all-windows-once
       (lambda ()
         (unless target-window
           (when (and
                  (not (equal (selected-window) current-window))
                  (not (jcs-buffer-name-this jcs--lsp-lv-buffer-name))
                  (not (jcs-frame-util-p))
                  (jcs-window-is-larger-in-height-p))
             (setq target-window (selected-window))))))
      (unless target-window
        (other-window 1)
        (jcs-walk-through-all-windows-once
         (lambda ()
           (unless target-window
             (when (and
                    (not (equal (selected-window) current-window))
                    (not (jcs-buffer-name-this jcs--lsp-lv-buffer-name))
                    (not (jcs-frame-util-p)))
               (setq target-window (selected-window)))))))
      (select-window target-window)
      ;; NOTE: We need to go back two windows in order to make the
      ;; `undo-tree-visualize' buffer to display in the next window.
      (progn (other-window -2) (other-window 1))
      (let ((bf-before-switched (buffer-name)))
        (switch-to-buffer rel-cbf)
        (save-selected-window (undo-tree-visualize))
        (switch-to-buffer bf-before-switched)))))


(defun jcs--undo-or-redo (ud)
  "Do undo or redo base on UD.
If UD is non-nil, do undo.  If UD is nil, do redo."
  (require 'undo-tree)
  (jcs--lsp-ui-doc--hide-frame)
  (if (not jcs-use-undo-tree-key)
      (call-interactively #'undo)  ; In Emacs, undo/redo is the same thing.
    (save-selected-window
      (let ((record-window (selected-window))
            (jumped-to-utv (jcs-jump-shown-to-buffer undo-tree-visualizer-buffer-name t)))
        ;; NOTE: If we do jumped to the `undo-tree-visualizer-buffer-name'
        ;; buffer, then we use `undo-tree-visualize-redo' instead of
        ;; `undo-tree-redo'. Because directly called `undo-tree-visualize-redo'
        ;; key is way faster than `undo-tree-redo' key.
        (if jumped-to-utv
            (if ud (undo-tree-visualize-undo) (undo-tree-visualize-redo))
          (select-window record-window)
          (if ud (undo-tree-undo) (undo-tree-redo))
          (jcs-undo-tree-visualize))
        ;; STUDY: weird that they use word
        ;; toggle, instead of just set it.
        ;;
        ;; Why not?
        ;;   => `undo-tree-visualizer-show-diff'
        ;; or
        ;;   => `undo-tree-visualizer-hide-diff'
        (when jcs-undo-tree-auto-show-diff (undo-tree-visualizer-toggle-diff))))))

;;;###autoload
(defun jcs-undo ()
  "Undo key."
  (interactive)
  (jcs--undo-or-redo t))

;;;###autoload
(defun jcs-redo ()
  "Redo key."
  (interactive)
  (jcs--undo-or-redo nil))

;;----------------------------------------------------------------------------
;; Backspace

;;;###autoload
(defun jcs-real-backspace ()
  "Just backspace a char."
  (interactive)
  (jcs-electric-backspace))

;;;###autoload
(defun jcs-smart-backspace ()
  "Smart backspace."
  (interactive)
  (if (and (jcs-is-infront-first-char-at-line-p)
           (not (jcs-is-beginning-of-line-p))
           (not (use-region-p)))
      (jcs-backward-delete-spaces-by-tab-width)
    (jcs-real-backspace)))

;;----------------------------------------------------------------------------
;; Delete

;;;###autoload
(defun jcs-real-delete ()
  "Just delete a char."
  (interactive)
  (jcs-electric-delete))

;;;###autoload
(defun jcs-smart-delete ()
  "Smart backspace."
  (interactive)
  (if (and (jcs-is-infront-first-char-at-line-p (1+ (point)))
           (not (jcs-is-end-of-line-p)))
      (jcs-forward-delete-spaces-by-tab-width)
    (jcs-real-delete)))

;;----------------------------------------------------------------------------
;; Return

;;;###autoload
(defun jcs-ctrl-return-key ()
  "Global Ctrl-Return key."
  (interactive)
  ;;;
  ;; Priority
  ;;
  ;; ATTENTION: all the function in the priority function
  ;; list must all have error handling. Or else this the
  ;; priority chain will break.
  ;;
  ;; 1. `project-abbrev-complete-word'
  ;; 2. `yas-expand'
  ;; 3. `goto-address-at-point'
  ;;
  (unless (ignore-errors (call-interactively #'project-abbrev-complete-word))
    (unless (ignore-errors (call-interactively #'jcs-yas-expand))
      (if (or (jcs-is-current-point-face 'link)
              (and (jcs-is-end-of-symbol-p)
                   (jcs-is-current-point-face 'link (1- (point)))))
          (call-interactively #'goto-address-at-point)
        (cl-case major-mode
          ('org-mode (call-interactively #'org-todo))
          (t (call-interactively (key-binding (kbd "RET")))))))))

;;----------------------------------------------------------------------------
;; Space

;;;###autoload
(defun jcs-real-space ()
  "Just insert a space."
  (interactive)
  (insert " "))

;;;###autoload
(defun jcs-smart-space ()
  "Smart way of inserting space."
  (interactive)
  (if (jcs-current-line-empty-p)
      (let ((pt (point)))
        (indent-for-tab-command)
        (when (= pt (point)) (jcs-real-space)))
    (if (or (jcs-is-infront-first-char-at-line-p)
            (jcs-is-beginning-of-line-p))
        (jcs-insert-spaces-by-tab-width)
      (jcs-real-space))))

;;----------------------------------------------------------------------------
;; Yank

;;;###autoload
(defun jcs-smart-yank ()
  "Yank and then indent region."
  (interactive)
  (jcs-delete-region)
  (let ((reg-beg (point)))
    (call-interactively #'yank)
    (ignore-errors (indent-region reg-beg (point)))))

;;----------------------------------------------------------------------------
;; Tab

;;;###autoload
(defun jcs-tab-key ()
  "Global TAB key."
  (interactive)
  (unless (ignore-errors (call-interactively #'jcs-yas-expand))
    (if (company--active-p)
        (call-interactively #'company-complete-selection)
      (if (jcs-current-line-empty-p)
          (let ((pt (point)))
            (indent-for-tab-command)
            (when (= pt (point)) (jcs-insert-spaces-by-tab-width)))
        (jcs-insert-spaces-by-tab-width)))))

;;----------------------------------------------------------------------------
;; Mark

(defvar-local jcs--marking-whole-buffer-p nil
  "Marking the whole buffer now?")

(defvar-local jcs--marking-whole-buffer--curosr-pos -1
  "Record down the cursor position.")

;;;###autoload
(defun jcs-mark-whole-buffer ()
  "Mark the whole buffer."
  (interactive)
  (call-interactively #'mark-whole-buffer)
  (setq jcs--marking-whole-buffer--curosr-pos (point))
  (setq jcs--marking-whole-buffer-p t))

;;----------------------------------------------------------------------------
;; Overwrite (Insert toggle)

;;;###autoload
(defun jcs-overwrite-mode ()
  "Wrap insert key with cursor changed."
  (interactive)

  ;; Toggle overwrite mode
  (call-interactively #'overwrite-mode)

  ;;;
  ;; Cursor Type
  ;;   -> box
  ;;   -> hollow
  ;;   -> bar
  ;;   -> hbar

  (if overwrite-mode
      (setq-local cursor-type 'hbar)
    (setq-local cursor-type 'box)))

;;----------------------------------------------------------------------------
;; Kill Line

;;;###autoload
(defun jcs-kill-whole-line ()
  "Deletes a line, but does not put it in the `kill-ring'."
  (interactive)
  ;; SOURCE: http://ergoemacs.org/emacs/emacs_kill-ring.html
  (let (kill-ring)
    (if (use-region-p)
        (jcs-delete-region)
      (let (;; Record down the column before killing the whole line.
            (before-column-num (current-column)))

        ;; Do kill the whole line!
        (delete-region (line-beginning-position)
                       (if (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))
                           (line-end-position)
                         (1+ (line-end-position))))

        ;; Goto the same column as before we do the killing the whole line
        ;; operations above.
        (move-to-column before-column-num)))))

;;;###autoload
(defun jcs-backward-kill-line (arg)
  "Kill ARG lines backward, but does not put it in the `kill-ring'."
  (interactive "p")
  (kill-line (- 1 arg))
  (setq kill-ring (cdr kill-ring)))

;;;###autoload
(defun jcs-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region (line-beginning-position) (point)))

;;;###autoload
(defun jcs-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;;;###autoload
(defun jcs-backward-delete-word (arg)
  "Backward deleteing ARG words."
  (interactive "p")
  (if (use-region-p)
      (jcs-delete-region)
    (jcs-delete-word (- arg))))

;;;###autoload
(defun jcs-forward-delete-word (arg)
  "Forward deleteing ARG words."
  (interactive "p")
  (if (use-region-p)
      (jcs-delete-region)
    (jcs-delete-word (+ arg))))

;;;###autoload
(defun jcs-smart-backward-delete-word ()
  "Backward deleteing ARG words in the smart way."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (let ((start-pt -1) (end-pt (point)) (start-ln-end-pt -1))
      (save-excursion
        (jcs-smart-backward-word)
        (setq start-pt (point))
        (setq start-ln-end-pt (line-end-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq start-pt start-ln-end-pt))
      (delete-region start-pt end-pt))))

;;;###autoload
(defun jcs-smart-forward-delete-word ()
  "Forward deleteing ARG words in the smart way."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (let ((start-pt (point)) (end-pt -1) (end-ln-start-pt -1))
      (save-excursion
        (jcs-smart-forward-word)
        (setq end-pt (point))
        (setq end-ln-start-pt (line-beginning-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq end-pt end-ln-start-pt))
      (delete-region start-pt end-pt))))

;;;###autoload
(defun jcs-backward-kill-word-capital ()
  "Backward delete the word unitl the word is capital."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (let ((start-pt -1) (end-pt (point)) (start-ln-end-pt -1))
      (save-excursion
        (jcs-backward-word-capital)
        (setq start-pt (point))
        (setq start-ln-end-pt (line-end-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq start-pt start-ln-end-pt))
      (delete-region start-pt end-pt))))

;;;###autoload
(defun jcs-forward-kill-word-capital ()
  "Forward delete the word unitl the word is capital."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (let ((start-pt (point)) (end-pt -1) (end-ln-start-pt -1))
      (save-excursion
        (jcs-forward-word-capital)
        (setq end-pt (point))
        (setq end-ln-start-pt (line-beginning-position)))
      (unless (= (line-number-at-pos start-pt) (line-number-at-pos end-pt))
        (setq end-pt end-ln-start-pt))
      (delete-region start-pt end-pt))))

(defun jcs-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

;;;###autoload
(defun jcs-duplicate-line ()
  "Duplicate the line."
  (interactive)
  (let ((cur-col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (forward-line 1)
    (yank)
    (move-to-column cur-col)))

;;----------------------------------------------------------------------------
;; Indent moving UP or DOWN.

(defun jcs-can-do-smart-indent-p ()
  "Check smart indent conditions."
  (and (not mark-active)
       (jcs-buffer-name-or-buffer-file-name)
       (not buffer-read-only)))

;;;###autoload
(defun jcs-smart-indent-up ()
  "Indent line after move up one line."
  (interactive)
  (jcs-previous-line)
  (when (jcs-can-do-smart-indent-p)
    (indent-for-tab-command)))

;;;###autoload
(defun jcs-smart-indent-up-by-mode ()
  "Like `jcs-smart-indent-up' but indent by mode."
  (interactive)
  (jcs-previous-line)
  (when (jcs-can-do-smart-indent-p)
    (indent-according-to-mode)))

;;;###autoload
(defun jcs-smart-indent-down ()
  "Indent line after move down one line."
  (interactive)
  (jcs-next-line)
  (when (jcs-can-do-smart-indent-p)
    (indent-for-tab-command)))

;;;###autoload
(defun jcs-smart-indent-down-by-mode ()
  "Like `jcs-smart-indent-down' but indent by mode."
  (interactive)
  (jcs-next-line)
  (when (jcs-can-do-smart-indent-p)
    (indent-according-to-mode)))

;;----------------------------------------------------------------------------
;; Format File

;;;###autoload
(defun jcs-format-document ()
  "Format current document."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun jcs-format-region-or-document ()
  "Format the document if there are no region apply."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (jcs-format-document)))

;;;###autoload
(defun jcs-align-region-by-points (regexp pnt-min pnt-max)
  "Align current selected region with REGEXP, PNT-MIN and PNT-MAX."
  (interactive)
  (align pnt-min pnt-max)
  (align-regexp pnt-min pnt-max regexp 1 1 t))

;;;###autoload
(defun jcs-align-region (regexp)
  "Align current selected region REGEXP."
  (interactive)
  (jcs-align-region-by-points regexp (region-beginning) (region-end))
  ;; Deactive region no matter what.
  (deactivate-mark))

;;;###autoload
(defun jcs-align-document (regexp)
  "Align current document with REGEXP."
  (interactive)
  ;; URL: https://www.emacswiki.org/emacs/AlignCommands
  ;; align the whole doc.
  (jcs-align-region-by-points regexp (point-min) (point-max)))

;;;###autoload
(defun jcs-align-region-or-document ()
  "Either align the region or document depend on if there is region selected."
  (interactive)
  (save-excursion
    (let (;; NOTE: this is the most common one.
          ;; Compatible to all programming languages use equal sign to assign value.
          (align-regexp-string-code "\\(\\s-*\\)[=]")
          ;; NOTE: Default support `//' and `/**/' comment symbols.
          (align-regexp-string-comment "\\(\\s-*\\) /[/*]")
          (pnt-min nil)
          (pnt-max nil))

      ;; Code RegExp String
      (cond ((jcs-is-current-major-mode-p "nasm-mode")
             (setq align-regexp-string-code "\\(\\s-*\\)equ "))
            ((jcs-is-current-major-mode-p "go-mode")
             (setq align-regexp-string-code "\\(\\s-*\\) := ")))

      ;; Comment RegExp String
      (cond ((jcs-is-current-major-mode-p "nasm-mode")
             (setq align-regexp-string-comment "\\(\\s-*\\)               [;]")))

      (if (jcs-is-region-selected-p)
          ;; NOTE: Align region only.
          (progn
            ;; First get region info.
            (setq pnt-min (region-beginning))
            (setq pnt-max (region-end))

            ;; Swapn region here.
            (when (< (point) pnt-max)
              (push-mark-command nil)
              (goto-char pnt-max)

              ;; Update region info.
              (setq pnt-min (region-beginning))
              (setq pnt-max (region-end)))

            ;; Align code segment.
            (jcs-align-region align-regexp-string-code)

            (when (> (point) pnt-min) (setq pnt-max (point))))
        ;; NOTE: Align whole document.
        (jcs-align-document align-regexp-string-code)

        ;; NOTE: These assigns does nothing for now. Just in case we dont apply
        ;; weird value, assign default document info.
        (setq pnt-min (point-min))
        (setq pnt-max (point-max)))

      ;; Align comment segment.
      (jcs-align-region-by-points align-regexp-string-comment
                                  pnt-min
                                  pnt-max))))

;;;###autoload
(defun jcs-align-repeat (regexp)
  "Repeat alignment with respect to the given regular expression.
REGEXP : reqular expression use to align."
  (interactive "r\nsAlign regexp: ")
  (if (jcs-is-region-selected-p)
      (align-regexp (region-beginning) (region-end)
                    (concat "\\(\\s-*\\)" regexp) 1 1 t)
    (align-regexp (point-min) (point-max)
                  (concat "\\(\\s-*\\)" regexp) 1 1 t)))

;;;###autoload
(defun jcs-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (require 'flycheck)
  ;; Record all the enabled mode that you want to
  ;; remain enabled after revert the file.
  (let ((was-flycheck flycheck-mode) (was-readonly buffer-read-only))
    ;; Revert it!
    (revert-buffer :ignore-auto :noconfirm :preserve-modes)
    ;; Revert all the enabled mode.
    (if was-flycheck (flycheck-mode 1) (flycheck-mode -1))
    (if was-readonly (read-only-mode 1) (read-only-mode -1))))

;;;###autoload
(defun jcs-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation."
  (interactive)
  (save-window-excursion
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        ;; Revert only buffers containing files, which are not modified; do not try
        ;; to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buf))
                   (not (jcs-is-current-file-empty-p buf)))
          (if (file-readable-p filename)
              ;; If the file exists and is readable, revert the buffer.
              (with-current-buffer buf
                (jcs-revert-buffer-no-confirm))
            ;; Otherwise, kill the buffer.
            (let (kill-buffer-query-functions)
              (kill-buffer buf))))))))


;;;###autoload
(defun jcs-other-window-next (&optional cnt not-all-frame)
  "Move CNT to the next window with NOT-ALL-FRAME."
  (interactive)
  (unless (numberp cnt) (setq cnt 1))
  (if not-all-frame (other-window cnt nil) (other-window cnt t)))

;;;###autoload
(defun jcs-other-window-prev (&optional cnt not-all-frame)
  "Move CNT to the previous window with NOT-ALL-FRAME."
  (interactive)
  (unless (numberp cnt) (setq cnt -1))
  (if not-all-frame (other-window cnt nil) (other-window cnt t)))

;;;###autoload
(defun jcs-scroll-up-one-line (&optional n)
  "Scroll the text up N line."
  (interactive)
  (let ((rel-n (if n n 1)))
    (scroll-up rel-n)))

;;;###autoload
(defun jcs-scroll-down-one-line (&optional n)
  "Scroll the text down N line."
  (interactive)
  (let ((rel-n (if n n 1)))
    (scroll-down rel-n)))

;;;###autoload
(defun jcs-remove-trailing-lines-end-buffer ()
  "Delete trailing line at the end of the buffer, leave only one line."
  (interactive)
  (save-excursion
    (let ((rec-point (point)))
      (goto-char (point-max))
      (if (and (jcs-current-line-empty-p)
               (not (= (line-number-at-pos) 1)))
          (forward-line -1)
        (newline))
      (while (and (jcs-current-line-empty-p)
                  (< rec-point (point)))
        (jcs-kill-whole-line)
        (forward-line -1)))))

;;;###autoload
(defun jcs-delete-trailing-whitespace-except-current-line ()
  "Delete the trailing whitespace for whole document execpt the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (> (point-max) end)
        (delete-trailing-whitespace (1+ end) (point-max)))
      (when (< (point-min) begin)
        (delete-trailing-whitespace (point-min) (1- begin))))))

;;----------------------------------------------------------------------------
;; Move Current Line Up or Down

;;;###autoload
(defun jcs-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun jcs-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;----------------------------------------------------------------------------
;; Word Case

;;;###autoload
(defun jcs-upcase-word-or-region ()
  "Uppercase the word or region."
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (call-interactively #'upcase-word)))

;;;###autoload
(defun jcs-downcase-word-or-region ()
  "Lowercase the word or region."
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (call-interactively #'downcase-word)))

;;;###autoload
(defun jcs-capitalize-word-or-region ()
  "Capitalize the word or region."
  (interactive)
  (if (use-region-p)
      (capitalize-region (region-beginning) (region-end))
    (call-interactively #'capitalize-word)))

;;----------------------------------------------------------------------------
;; Line Ending

;;;###autoload
(defun jcs-remove-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

;;----------------------------------------------------------------------------
;; Tabify / Unabify

;;;###autoload
(defun jcs-untabify-buffer (&optional start end)
  "Untabify the current buffer with region START and END."
  (interactive)
  (jcs-save-excursion
   (lambda ()
     (let ((start-pt (if start start (point-min)))
           (end-pt (if end end (point-max))))
       (widen)
       (untabify start-pt end-pt)))))

;;;###autoload
(defun jcs-tabify-buffer (&optional start end)
  "Tabify the current buffer with region START and END."
  (interactive)
  (jcs-save-excursion
   (lambda ()
     (let ((start-pt (if start start (point-min)))
           (end-pt (if end end (point-max))))
       (widen)
       (tabify start-pt end-pt)))))

;;----------------------------------------------------------------------------
;; Save Buffer

(defun jcs-do-stuff-before-save (&rest _)
  "Do stuff before save command is executed."
  ;; NOTE: If company menu currently active, abort it.
  (company-abort))
(advice-add 'save-buffer :before #'jcs-do-stuff-before-save)

(defun jcs-do-stuff-after-save (&rest _)
  "Do stuff after save command is executed."
  (jcs-undo-kill-this-buffer)  ; Try to close `*undo-tree*' buffer.
  (jcs-update-line-number-each-window))
(advice-add 'save-buffer :after #'jcs-do-stuff-after-save)

(defun jcs-organize-save-buffer (tab-it)
  "Organize save buffer by TAB-IT."
  (let (deactivate-mark truncate-lines)
    (jcs-delete-trailing-whitespace-except-current-line)
    (jcs-remove-trailing-lines-end-buffer)
    (if tab-it (jcs-tabify-buffer) (jcs-untabify-buffer))
    (jcs-mute-apply #'jcs-remove-control-M)
    (jcs-save-buffer)))

;;;###autoload
(defun jcs-reverse-tab-untab-save-buffer ()
  (interactive)
  (cl-case (key-binding (kbd "C-s"))
    ('jcs-untabify-save-buffer (jcs-tabify-save-buffer))
    ('jcs-tabify-save-buffer (jcs-untabify-save-buffer))
    (t (user-error "[ERROR] Their are no default tab/untab save"))))

;;;###autoload
(defun jcs-untabify-save-buffer ()
  "Untabify the file and save the buffer."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "[WARNING] Can't save with invalid filename: %s" (buffer-name)))
   (buffer-read-only
    (user-error "[WARNING] Can't save read-only file: %s" buffer-read-only))
   (t
    (jcs-organize-save-buffer nil))))

;;;###autoload
(defun jcs-tabify-save-buffer ()
  "Tabify the file and save the buffer."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "[WARNING] Can't save with invalid filename: %s" (buffer-name)))
   (buffer-read-only
    (user-error "[WARNING] Can't save read-only file: %s" buffer-read-only))
   (t
    (jcs-organize-save-buffer t))))

;;;###autoload
(defun jcs-save-buffer ()
  "Save buffer wrapper."
  (interactive)
  (setq jcs-created-parent-dir-path nil)
  (let ((jcs--no-advice-other-window t)
        (modified (buffer-modified-p))
        (readable (file-readable-p (buffer-file-name)))
        (cur-frame (selected-frame)))
    ;; For some mode, broken save.
    (jcs-mute-apply (lambda () (save-excursion (save-buffer))))
    (select-frame-set-input-focus cur-frame)  ; For multi frames.
    ;; If wasn't readable, try to active LSP once if LSP is available.
    (unless readable (jcs--safe-lsp-active))
    (if (or modified (not readable))
        (message "Wrote file %s" (buffer-file-name))
      (message "(No changes need to be saved)"))))

;;;###autoload
(defun jcs-save-all-buffers ()
  "Save all buffers currently opened."
  (interactive)
  (save-window-excursion
    (dolist (buf (buffer-list))
      (switch-to-buffer buf)
      (ignore-errors (call-interactively (key-binding (kbd "C-s"))))))
  (message "All buffers are saved"))

;;----------------------------------------------------------------------------
;; Find file

(defun jcs-is-finding-file-p ()
  "Check if current minibuffer finding file."
  (jcs-minibuffer-do-stuff
   (lambda ()
     (string-match-p "Find file:" (buffer-string)))))

;;;###autoload
(defun jcs-same-file-other-window ()
  "This will allow us open the same file in another window."
  (interactive)
  (save-selected-window
    (let ((buf-name (buffer-name)))
      (other-window 1)
      (unless (jcs-window-is-larger-in-height-p) (other-window 1))
      (switch-to-buffer buf-name))))

(defun jcs-find-file-other-window (fp)
  "Find file FP in other window with check of larger window height."
  (find-file fp)
  (jcs-same-file-other-window)
  (bury-buffer))

;;----------------------------------------------------------------------------
;; Rename file

(defun jcs-is-renaming-p ()
  "Check if current minibuffer renaming."
  (jcs-minibuffer-do-stuff
   (lambda ()
     (string-match-p "New name:" (buffer-string)))))

;;;###autoload
(defun jcs-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  ;; SOURCE: https://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name
  ;; URL: http://www.whattheemacsd.com/
  (let ((name (buffer-name)) (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'."
                   name (file-name-nondirectory new-name)))))))

;;----------------------------------------------------------------------------
;; Kill Buffer

(defun jcs-switch-to-buffer (buffer-or-name &optional ow no-record force-same-window)
  "Switch to buffer wrarpper with other window (OW) option.
NO-RECORD and FORCE-SAME-WINDOW are the same as switch to buffer arguments."
  (if ow
      (switch-to-buffer-other-window buffer-or-name no-record)
    (switch-to-buffer buffer-or-name no-record force-same-window)))

(defun jcs-bury-diminished-buffer ()
  "Bury the diminished buffer."
  (when (and
         diminish-buffer-mode
         (jcs-is-contain-list-string-regexp (append jcs-bury-buffer-list diminish-buffer-list)
                                            (jcs-buffer-name-or-buffer-file-name)))
    (jcs-bury-buffer)))

;;;###autoload
(defun jcs-bury-buffer ()
  "Bury this buffer."
  (interactive)
  (let ((bn (jcs-buffer-name-or-buffer-file-name)))
    (bury-buffer)
    (when (or (jcs-is-current-major-mode-p "Buffer-menu-mode")
              (string= bn (jcs-buffer-name-or-buffer-file-name)))
      (jcs-switch-to-previous-buffer)))
  ;; If something that I doesn't want to see, bury it.
  ;; For instance, any `*helm-' buffers.
  (jcs-bury-diminished-buffer))

(defun jcs-advice-kill-this-buffer-around (fnc &rest args)
  "Advice around execute `kill-this-buffer' command with FNC and ARGS."
  (require 'undo-tree)
  (let ((target-kill-buffer (jcs-buffer-name-or-buffer-file-name))
        (undoing-buffer-name nil)
        (jumped-to-utv nil))
    (save-selected-window
      (setq jumped-to-utv (jcs-jump-shown-to-buffer undo-tree-visualizer-buffer-name t))
      (when jumped-to-utv
        (setq undoing-buffer-name (buffer-name undo-tree-visualizer-parent-buffer))))

    (apply fnc args)

    ;; If `undo-tree' visualizer exists, kill it too.
    (when jumped-to-utv
      (when (and undoing-buffer-name
                 (string-match-p undoing-buffer-name target-kill-buffer)
                 ;; Only close `undo-tree' when buffer is killed.
                 (not (string= target-kill-buffer (jcs-buffer-name-or-buffer-file-name))))
        (jcs-undo-kill-this-buffer)))))
(advice-add 'kill-this-buffer :around #'jcs-advice-kill-this-buffer-around)

;;;###autoload
(defun jcs-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (when jcs-created-parent-dir-path  ; Remove virtual parent directory.
    (let* ((topest-dir (nth 0 (f-split jcs-created-parent-dir-path)))
           (create-dir (s-replace jcs-created-parent-dir-path "" default-directory))
           (del-path (f-slash (concat create-dir topest-dir))))
      (delete-directory del-path)
      (message "Remove parent directory that were virtual => '%s'" del-path)))

  (kill-this-buffer)

  ;; Refresh buffer menu once.
  (jcs-buffer-menu-safe-refresh)

  ;; If still in the buffer menu, try switch to the
  ;; previous buffer
  (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
    (jcs-switch-to-previous-buffer)))

;;;###autoload
(defun jcs-maybe-kill-this-buffer (&optional ecp-same)
  "Kill buffer if the current buffer is the only shown in one window.
Otherwise just switch to the previous buffer to keep the buffer.
ECP-SAME : Exception for the same buffer."
  (interactive)
  (let ((is-killed nil))
    (if (jcs-buffer-shown-in-multiple-window-p (buffer-name) t)
        (jcs-bury-buffer)
      (jcs-kill-this-buffer)
      (setq is-killed t)

      ;; NOTE: After kill the buffer, if the buffer appear in multiple
      ;; windows then we do switch to previous buffer again. Hence, it will
      ;; not show repeated buffer at the same time in different windows.
      (when (and (jcs-buffer-shown-in-multiple-window-p (buffer-name) t)
                 (not ecp-same))
        (jcs-bury-buffer)

        ;; If is something from default Emacs's buffer, switch back to previous
        ;; buffer once again.
        ;;
        ;; This will solve if there is only one file opened, and switch to none
        ;; sense buffer issue.
        ;;
        ;; None sense buffer or Emacs's default buffer is
        ;;   -> *GNU Emacs*
        ;;   -> *scratch*
        ;;   , etc.
        (when (and (not (buffer-file-name))
                   (>= (jcs-valid-buffers-count) 2))
          (jcs-switch-to-next-valid-buffer))))
    ;; If something that I doesn't want to see, bury it.
    ;; For instance, any `*helm-' buffers.
    (jcs-bury-diminished-buffer)
    is-killed))

;;;###autoload
(defun jcs-reopen-this-buffer ()
  "Kill the current buffer and open it again."
  (interactive)
  (let ((current-bfn (buffer-file-name)))
    (when current-bfn
      (jcs-window-record-once)
      (jcs-kill-this-buffer)
      (jcs-window-restore-once)
      (message "Reopened file => '%s'" current-bfn))))

;;----------------------------------------------------------------------------
;; Delete Repeatedly

;;;###autoload
(defun jcs-backward-delete-current-char-repeat ()
  "Delete the current character repeatedly, util it meet the character is not \
the same as current char.  (Backward)"
  (interactive)
  (jcs-delete-char-repeat (jcs-get-current-char-string) t))

;;;###autoload
(defun jcs-forward-delete-current-char-repeat ()
  "Delete the current character repeatedly, util it meet the character is not \
the same as current char.  (Forward)"
  (interactive)
  (jcs-delete-char-repeat (jcs-get-current-char-string) nil))

;;;###autoload
(defun jcs-delete-char-repeat (char reverse)
  "Kill the character repeatedly forward.
CHAR : character to check to delete.
REVERSE : t forward, nil backward."
  (let ((do-kill-char nil))
    (save-excursion
      (if reverse (backward-char) (forward-char))

      (when (jcs-current-char-equal-p char)
        (setq do-kill-char t)))

    (when do-kill-char
      (if reverse (delete-char -1) (delete-char 1))
      (jcs-delete-char-repeat char reverse))))

;;----------------------------------------------------------------------------
;; Delete inside a Character.

(defun jcs-find-start-char (start-char preserve-point)
  "Find the START-CHAR with PRESERVE-POINT."
  (let ((inhibit-message t)
        (start-point nil))
    (jcs-move-to-backward-a-char-do-recursive start-char nil)

    ;; If failed search backward start character..
    (if jcs-search-trigger-backward-char
        (progn
          (setq jcs-search-trigger-backward-char nil)
          (goto-char preserve-point)
          (error "Does not find beginning character : %s" start-char))
      ;; Fixed column position.
      (forward-char 1))

    (setq start-point (point))

    ;; Returns found point.
    start-point))

(defun jcs-find-end-char (end-char preserve-point)
  "Find the END-CHAR with PRESERVE-POINT."
  (let ((inhibit-message t) (end-point nil))
    (jcs-move-to-forward-a-char-do-recursive end-char nil)

    ;; If failed search forward end character..
    (if jcs-search-trigger-forward-char
        (progn
          (setq jcs-search-trigger-forward-char nil)
          (goto-char preserve-point)
          (error "Does not find end character : %s" end-char))
      (forward-char -1))

    (setq end-point (point))

    ;; Returns found point.
    end-point))

(defun jcs-check-outside-nested-char-p (start-char end-char)
  "Check if outside the nested START-CHAR and END-CHAR."
  (save-excursion
    (ignore-errors
      (let ((preserve-point (point))
            (nested-level 0)
            ;; Is the same char or not?
            (same-char (string= start-char end-char))
            (same-char-start-flag nil))

        ;; Beginning of the buffer.
        (goto-char (point-min))

        ;; We count the nested level from the beginning of the buffer.
        (while (<= (point) preserve-point)
          (if same-char
              (when (jcs-current-char-equal-p start-char)
                (if same-char-start-flag
                    (progn
                      (setq nested-level (- nested-level 1))
                      (setq same-char-start-flag nil))
                  (setq nested-level (+ nested-level 1))
                  (setq same-char-start-flag t)))
            ;; If is the start char, we add up the nested level.
            (when (jcs-current-char-equal-p start-char)
              (setq nested-level (+ nested-level 1)))

            ;; If is the end char, we minus the nested level.
            (when (jcs-current-char-equal-p end-char)
              (setq nested-level (- nested-level 1))))

          (forward-char 1))

        ;; If nested level is lower than 0, meaning is not between the nested
        ;; START-CHAR and END-CHAR.
        (<= nested-level 0)))))

(defun jcs-delete-between-char (start-char end-char)
  "Delete everything between START-CHAR and the END-CHAR."
  (let* ((preserve-point (point))
         (start-point (jcs-find-start-char start-char preserve-point))
         (end-point nil))

    ;; NOTE: Back to preserve point before we search.
    (goto-char preserve-point)

    ;; Get end bound.
    (forward-char 1)
    (if (jcs-current-char-equal-p end-char)
        (progn
          (backward-char 1)
          (setq end-point (point)))
      (backward-char 1)
      (setq end-point (jcs-find-end-char end-char preserve-point)))

    (unless (string= start-char end-char)
      ;; NOTE: Start to solve the nested character issue.
      (goto-char preserve-point)
      (let ((nested-count 0)
            (break-search-nested nil)
            (nested-counter 0))
        (ignore-errors
          ;; Solve backward nested.
          (while (not break-search-nested)
            (goto-char start-point)
            (backward-char 1)

            (while (<= nested-counter nested-count)
              (jcs-find-end-char end-char preserve-point)
              (setq nested-counter (+ nested-counter 1)))

            (if (not (= end-point (point)))
                (progn
                  (setq nested-count (+ nested-count 1))
                  (goto-char start-point)
                  (backward-char 1)
                  (setq start-point (jcs-find-start-char start-char preserve-point)))
              (setq break-search-nested t))))

        ;; IMPORTANT: reset variables.
        (goto-char preserve-point)
        (setq nested-count 0)
        (setq break-search-nested nil)
        (setq nested-counter 0)

        (ignore-errors
          ;; Solve forward nested.
          (while (not break-search-nested)
            (goto-char end-point)

            (while (<= nested-counter nested-count)
              (jcs-find-start-char start-char preserve-point)
              (setq nested-counter (+ nested-counter 1)))

            (if (not (= start-point (point)))
                (progn
                  (setq nested-count (+ nested-count 1))
                  (goto-char end-point)
                  (setq end-point (jcs-find-end-char end-char preserve-point)))
              (setq break-search-nested t)))))

      ;; Go back to original position before do anything.
      (goto-char preserve-point))

    ;; Check if is inside the region.
    (if (and (>= preserve-point start-point)
             (<= preserve-point end-point)
             (or (string= start-char end-char)
                 (not (jcs-check-outside-nested-char-p start-char end-char))))
        ;; Delete the region.
        (delete-region start-point end-point)
      ;; Back to where you were.
      (goto-char preserve-point)
      (error "You are not between %s and %s" start-char end-char))))

;;;###autoload
(defun jcs-delete-inside-paren ()
  "Delete everything inside open parenthesis and close parenthesis."
  (interactive)
  (jcs-delete-between-char "(" ")"))

;;;###autoload
(defun jcs-delete-inside-sqr-paren ()
  "Delete everything between open square parenthesis and close square parenthesis."
  (interactive)
  (jcs-delete-between-char "[[]" "]"))

;;;###autoload
(defun jcs-delete-inside-curly-paren ()
  "Delete everything between open curly parenthesis and close curly parenthesis."
  (interactive)
  (jcs-delete-between-char "{" "}"))

;;;###autoload
(defun jcs-delete-inside-single-quot ()
  "Delete everything between single quotation mark."
  (interactive)
  (jcs-delete-between-char "'" "'"))

;;;###autoload
(defun jcs-delete-inside-double-quot ()
  "Delete everything between double quotation mark."
  (interactive)
  (jcs-delete-between-char "\"" "\""))

;;;###autoload
(defun jcs-delete-inside-greater-less-sign ()
  "Delete everything between greater than sign and less than sign."
  (interactive)
  (jcs-delete-between-char "<" ">"))

;;;###autoload
(defun jcs-delete-inside-less-greater-sign ()
  "Delete everything between less than sign and greater than sign."
  (interactive)
  (jcs-delete-between-char ">" "<"))

;;;###autoload
(defun jcs-delete-inside-back-quot ()
  "Delete everything between back quote."
  (interactive)
  (jcs-delete-between-char "`" "`"))

;;;###autoload
(defun jcs-delete-inside-tilde ()
  "Delete everything between back quote."
  (interactive)
  (jcs-delete-between-char "~" "~"))

;;;###autoload
(defun jcs-delete-inside-exclamation-mark ()
  "Delete everything between exclamation mark."
  (interactive)
  (jcs-delete-between-char "!" "!"))

;;;###autoload
(defun jcs-delete-inside-at-sign ()
  "Delete everything between at sign."
  (interactive)
  (jcs-delete-between-char "@" "@"))

;;;###autoload
(defun jcs-delete-inside-sharp-sign ()
  "Delete everything between sharp sign."
  (interactive)
  (jcs-delete-between-char "#" "#"))

;;;###autoload
(defun jcs-delete-inside-dollar-sign ()
  "Delete everything between dollar sign."
  (interactive)
  (jcs-delete-between-char "[$]" "[$]"))

;;;###autoload
(defun jcs-delete-inside-percent-sign ()
  "Delete everything between percent sign."
  (interactive)
  (jcs-delete-between-char "%" "%"))

;;;###autoload
(defun jcs-delete-inside-caret ()
  "Delete everything between caret."
  (interactive)
  (jcs-delete-between-char "[|^]" "[|^]"))

;;;###autoload
(defun jcs-delete-inside-and ()
  "Delete everything between and."
  (interactive)
  (jcs-delete-between-char "&" "&"))

;;;###autoload
(defun jcs-delete-inside-asterisk ()
  "Delete everything between asterisk."
  (interactive)
  (jcs-delete-between-char "*" "*"))

;;;###autoload
(defun jcs-delete-inside-dash ()
  "Delete everything between dash."
  (interactive)
  (jcs-delete-between-char "-" "-"))

;;;###autoload
(defun jcs-delete-inside-underscore ()
  "Delete everything between underscore."
  (interactive)
  (jcs-delete-between-char "_" "_"))

;;;###autoload
(defun jcs-delete-inside-equal ()
  "Delete everything between equal."
  (interactive)
  (jcs-delete-between-char "=" "="))

;;;###autoload
(defun jcs-delete-inside-plus ()
  "Delete everything between plus."
  (interactive)
  (jcs-delete-between-char "+" "+"))


;;;###autoload
(defun jcs-delete-inside-backslash ()
  "Delete everything between backslash."
  (interactive)
  (jcs-delete-between-char "[\\]" "[\\]"))

;;;###autoload
(defun jcs-delete-inside-or ()
  "Delete everything between or."
  (interactive)
  (jcs-delete-between-char "|" "|"))


;;;###autoload
(defun jcs-delete-inside-colon ()
  "Delete everything between colon."
  (interactive)
  (jcs-delete-between-char ":" ":"))

;;;###autoload
(defun jcs-delete-inside-semicolon ()
  "Delete everything between semicolon."
  (interactive)
  (jcs-delete-between-char ";" ";"))

;;;###autoload
(defun jcs-delete-inside-comma ()
  "Delete everything between comma."
  (interactive)
  (jcs-delete-between-char "," ","))

;;;###autoload
(defun jcs-delete-inside-period ()
  "Delete everything between period."
  (interactive)
  (jcs-delete-between-char "[.]" "[.]"))

;;;###autoload
(defun jcs-delete-inside-slash ()
  "Delete everything between slash."
  (interactive)
  (jcs-delete-between-char "/" "/"))

;;;###autoload
(defun jcs-delete-inside-question-mark ()
  "Delete everything between question mark."
  (interactive)
  (jcs-delete-between-char "?" "?"))

;;----------------------------------------------------------------------------
;; Electric Pair

(defun jcs-get-open-pair-char (c)
  "Get the open pairing character from C."
  (let ((pair-char nil))
    (cond ((string= c "\"") (setq pair-char '("\"")))
          ((string= c "'") (setq pair-char '("'" "`")))
          ((string= c ")") (setq pair-char '("(")))
          ((string= c "]") (setq pair-char '("[")))
          ((string= c "}") (setq pair-char '("{")))
          ((string= c "`") (setq pair-char '("`"))))
    pair-char))

(defun jcs-get-close-pair-char (c)
  "Get the list of close pairing character from C."
  (let ((pair-char nil))
    (cond ((string= c "\"") (setq pair-char '("\"")))
          ((string= c "'") (setq pair-char '("'")))
          ((string= c "(") (setq pair-char '(")")))
          ((string= c "[") (setq pair-char '("]")))
          ((string= c "{") (setq pair-char '("}")))
          ((string= c "`") (setq pair-char '("`" "'"))))
    pair-char))


(defun jcs-forward-delete-close-pair-char (cpc)
  "Forward delete close pair characters CPC."
  (when (and cpc
             (not (jcs-is-end-of-buffer-p)))
    (save-excursion
      (forward-char 1)
      (when (jcs-current-char-equal-p cpc)
        (backward-delete-char 1)))))

(defun jcs-backward-delete-open-pair-char (opc)
  "Backward delete open pair characters OPC."
  (when (and opc
             (not (jcs-is-beginning-of-buffer-p)))
    (save-excursion
      (when (jcs-current-char-equal-p opc)
        (backward-delete-char 1)))))

(defun jcs-forward-delete-close-pair-char-seq (cc)
  "Forward delete close pair characters in sequence.
CC : Current character at position."
  (save-excursion
    (cond ((string= cc "*")  ; Seq => /**/
           (when (jcs-current-char-equal-p "/")
             (save-excursion
               (forward-char 1)
               (when (jcs-current-char-equal-p "*")
                 (forward-char 1)
                 (when (jcs-current-char-equal-p "/")
                   ;; Found sequence, delete them!
                   (backward-delete-char 1)
                   (backward-delete-char 1)
                   (backward-delete-char 1)))))))))

(defun jcs-backward-delete-open-pair-char-seq (cc)
  "Backward delete open pair characters in sequence.
CC : Current character at position."
  (save-excursion
    (cond ((string= cc "*")  ; Seq => /**/
           (save-excursion
             (backward-char 1)
             (when (jcs-current-char-equal-p "/")
               (forward-char 1)
               (when (jcs-current-char-equal-p "*")
                 (forward-char 1)
                 (when (jcs-current-char-equal-p "/")
                   ;; Found sequence, delete them!
                   (backward-delete-char 1)
                   (backward-delete-char 1)
                   (backward-delete-char 1)))))))))

;;;###autoload
(defun jcs-electric-delete ()
  "Electric delete key."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (let ((cc "")
          (opc ""))
      (save-excursion
        (forward-char 1)
        (setq cc (jcs-get-current-char-string)))
      (setq opc (jcs-get-open-pair-char cc))
      (if (and (jcs-is-inside-string-p)
               (not (string= cc "\""))
               (not (string= cc "'")))
          (backward-delete-char -1)
        (backward-delete-char -1)
        (jcs-backward-delete-open-pair-char opc)
        (jcs-backward-delete-open-pair-char-seq cc)))))

;;;###autoload
(defun jcs-electric-backspace ()
  "Electric backspace key."
  (interactive)
  (if (use-region-p)
      (jcs-delete-region)
    (if (and (jcs-is-inside-string-p)
             (not (jcs-current-char-equal-p '("\"" "'"))))
        (jcs-own-delete-backward-char)
      (let* ((cc (jcs-get-current-char-string))
             (cpc (jcs-get-close-pair-char cc)))
        (jcs-own-delete-backward-char)
        (jcs-forward-delete-close-pair-char cpc)
        (jcs-forward-delete-close-pair-char-seq cc)))))

;;----------------------------------------------------------------------------
;; Isearch

;;;###autoload
(defun jcs-isearch-backward-symbol-at-point ()
  "Isearch backward symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-repeat-backward))

;;;###autoload
(defun jcs-isearch-project-backward-symbol-at-point ()
  "Isearch project backward symbol at point."
  (interactive)
  (isearch-project-forward-symbol-at-point))

;;;###autoload
(defun jcs-isearch-repeat-backward ()
  "Isearch backward repeating."
  (interactive)
  (if (not (advice-member-p 'isearch-project-advice-isearch-repeat-after 'isearch-repeat))
      (isearch-repeat-backward)
    (message "Exit 'isearch-project' cuz you are trying to use 'isearch'..")
    (sleep-for jcs-prompt-message-sleep-delay-time)
    (save-mark-and-excursion
      (isearch-abort))))

;;;###autoload
(defun jcs-isearch-repeat-forward ()
  "Isearch forward repeating."
  (interactive)
  (if (not (advice-member-p 'isearch-project-advice-isearch-repeat-after 'isearch-repeat))
      (isearch-repeat-forward)
    (message "Exit 'isearch-project' cuz you are trying to use 'isearch'..")
    (sleep-for jcs-prompt-message-sleep-delay-time)
    (save-mark-and-excursion
      (isearch-abort))))

;;;###autoload
(defun jcs-isearch-project-repeat-backward ()
  "Isearch project backward repeating."
  (interactive)
  (if (advice-member-p 'isearch-project-advice-isearch-repeat-after 'isearch-repeat)
      (isearch-repeat-backward)
    (message "Exit 'isearch' cuz you are trying to use 'isearch-project'..")
    (sleep-for jcs-prompt-message-sleep-delay-time)
    (save-mark-and-excursion
      (isearch-abort))))

;;;###autoload
(defun jcs-isearch-project-repeat-forward ()
  "Isearch project forward repeating."
  (interactive)
  (if (advice-member-p 'isearch-project-advice-isearch-repeat-after 'isearch-repeat)
      (isearch-repeat-forward)
    (message "Exit 'isearch' cuz you are trying to use 'isearch-project'..")
    (sleep-for jcs-prompt-message-sleep-delay-time)
    (save-mark-and-excursion
      (isearch-abort))))

;;----------------------------------------------------------------------------
;; Multiple Cursors

;;;###autoload
(defun jcs-mc/mark-previous-like-this-line ()
  "Smart marking previous line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-next-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-previous-like-this)))))

;;;###autoload
(defun jcs-mc/mark-next-like-this-line ()
  "Smart marking next line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-previous-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-next-like-this)))))

(defun jcs-mc/maybe-multiple-cursors-mode ()
  "Maybe enable `multiple-cursors-mode' depends on the cursor number."
  (if (> (mc/num-cursors) 1) (multiple-cursors-mode 1) (multiple-cursors-mode 0)))

(defun jcs-mc/to-furthest-cursor-before-point ()
  "Goto the furthest cursor before point."
  (when (mc/furthest-cursor-before-point) (goto-char (overlay-end (mc/furthest-cursor-before-point)))))

(defun jcs-mc/to-furthest-cursor-after-point ()
  "Goto furthest cursor after point."
  (when (mc/furthest-cursor-after-point) (goto-char (overlay-end (mc/furthest-cursor-after-point)))))

;;;###autoload
(defun jcs-mc/mark-previous-similar-this-line (&optional sdl)
  "Mark previous line similar to this line depends on string distance level (SDL)."
  (interactive)
  (require 'multiple-cursors)
  (unless sdl (setq sdl jcs-mc/string-distance-level))
  (save-excursion
    (let ((cur-line (thing-at-point 'line)) (sim-line nil)
          (cur-col (current-column))
          (break nil))
      (jcs-mc/to-furthest-cursor-before-point)
      (forward-line -1)
      (while (and (not break) (not (= (line-number-at-pos (point)) (line-number-at-pos (point-min)))))
        (setq sim-line (thing-at-point 'line))
        (when (and (< (string-distance sim-line cur-line) sdl)
                   (or (and (not (string= "\n" sim-line)) (not (string= "\n" cur-line)))
                       (and (string= "\n" sim-line) (string= "\n" cur-line))))
          (move-to-column cur-col)
          (mc/create-fake-cursor-at-point)
          (setq break t))
        (forward-line -1))
      (unless break (user-error "[INFO] no previous similar match"))))
  (jcs-mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun jcs-mc/mark-next-similar-this-line (&optional sdl)
  "Mark next line similar to this line depends on string distance level (SDL)."
  (interactive)
  (require 'multiple-cursors)
  (unless sdl (setq sdl jcs-mc/string-distance-level))
  (save-excursion
    (let ((cur-line (thing-at-point 'line)) (sim-line nil)
          (cur-col (current-column))
          (break nil))
      (jcs-mc/to-furthest-cursor-after-point)
      (forward-line 1)
      (while (and (not break) (not (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))))
        (setq sim-line (thing-at-point 'line))
        (when (and (< (string-distance sim-line cur-line) sdl)
                   (or (and (not (string= "\n" sim-line)) (not (string= "\n" cur-line)))
                       (and (string= "\n" sim-line) (string= "\n" cur-line))))
          (move-to-column cur-col)
          (mc/create-fake-cursor-at-point)
          (setq break t))
        (forward-line 1))
      (unless break (user-error "[INFO] no next similar match"))))
  (jcs-mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun jcs-mc/inc-string-distance-level ()
  "Increase the string distance level by 1."
  (interactive)
  (setq jcs-mc/string-distance-level (1+ jcs-mc/string-distance-level))
  (message "[INFO] Current string distance: %s" jcs-mc/string-distance-level))

;;;###autoload
(defun jcs-mc/dec-string-distance-level ()
  "Decrease the string distance level by 1."
  (interactive)
  (setq jcs-mc/string-distance-level (1- jcs-mc/string-distance-level))
  (message "[INFO] Current string distance: %s" jcs-mc/string-distance-level))

;;----------------------------------------------------------------------------
;; Folding / Unfolding

;;;###autoload
(defun jcs-close-all-nodes ()
  "Close all nodes in current file."
  (interactive)
  (require 'origami)
  (call-interactively #'origami-close-all-nodes))

;;;###autoload
(defun jcs-open-all-nodes ()
  "Open all nodes in current file."
  (interactive)
  (require 'origami)
  (call-interactively #'origami-open-all-nodes))

;;;###autoload
(defun jcs-close-node ()
  "Close the current scope of the node."
  (interactive)
  (require 'origami)
  (call-interactively #'origami-close-node))

;;;###autoload
(defun jcs-open-node ()
  "Open the current scope of the node."
  (interactive)
  (require 'origami)
  (call-interactively #'origami-open-node))

(provide 'jcs-edit)
;;; jcs-edit.el ends here
