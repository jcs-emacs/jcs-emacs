;;; jcs-edit.el --- When editing the file.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------
;; Undo / Redo
;;----------------------------------------------

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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

;;;###autoload
(defun jcs-toggle-undo-tree-auto-show-diff ()
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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

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

;;-----------------------------------------------------------
;;-----------------------------------------------------------

(defun jcs-undo-tree-visualize (&optional cbf)
  "Call `undo-tree-visualize' only in window that is full height (next window).
CBF : Current buffer file name."
  (let ((win-len (jcs-count-windows))
        (win-index 0)
        (found-valid nil)
        (rel-cbf (if cbf cbf (buffer-name))))
    (save-selected-window
      (while (and (< win-index win-len)
                  (not found-valid))
        (jcs-other-window-next)
        (when (jcs-window-is-larger-in-height-p)
          ;; NOTE: We need to go back two windows in
          ;; order to make the undo-tree-visualize
          ;; buffer to display in the next window.
          (jcs-other-window-prev)
          (jcs-other-window-prev)
          (jcs-other-window-next)
          (setq found-valid t))
        (setq win-index (1+ win-index)))
      (when found-valid
        (let ((bf-before-switched (buffer-name)))
          (switch-to-buffer rel-cbf)
          (save-selected-window
            (undo-tree-visualize))
          (switch-to-buffer bf-before-switched))))
    (unless found-valid
      (undo-tree-visualize))))


;;;###autoload
(defun jcs-undo ()
  "Undo key."
  (interactive)
  (require 'undo-tree)
  (if jcs-use-undo-tree-key
      (progn
        (save-selected-window
          (let ((jumped-to-utv
                 (ignore-errors
                   (jcs-jump-shown-to-buffer undo-tree-visualizer-buffer-name))))
            ;; NOTE: If we do jumped to the
            ;; `undo-tree-visualizer-buffer-name' buffer,
            ;; then we use `undo-tree-visualize-undo' instead
            ;; of `undo-tree-undo'. Because directly called
            ;; `undo-tree-visualize-undo' key is way faster than
            ;; `undo-tree-undo' key.
            (if jumped-to-utv
                (undo-tree-visualize-undo)
              (undo-tree-undo)
              (jcs-undo-tree-visualize))
            ;; STUDY: weird that they use word
            ;; toggle, instead of just set it.
            ;;
            ;; Why not?
            ;;   => `undo-tree-visualizer-show-diff'
            ;; or
            ;;   => `undo-tree-visualizer-hide-diff'
            (when jcs-undo-tree-auto-show-diff
              (undo-tree-visualizer-toggle-diff)))))
    (call-interactively #'undo)))

;;;###autoload
(defun jcs-redo ()
  "Redo key."
  (interactive)
  (require 'undo-tree)
  (if jcs-use-undo-tree-key
      (progn
        (save-selected-window
          (let ((jumped-to-utv
                 (ignore-errors
                   (jcs-jump-shown-to-buffer undo-tree-visualizer-buffer-name))))
            ;; NOTE: If we do jumped to the
            ;; `undo-tree-visualizer-buffer-name' buffer,
            ;; then we use `undo-tree-visualize-redo' instead
            ;; of `undo-tree-redo'. Because directly called
            ;; `undo-tree-visualize-redo' key is way faster than
            ;; `undo-tree-redo' key.
            (if jumped-to-utv
                (undo-tree-visualize-redo)
              (undo-tree-redo)
              (jcs-undo-tree-visualize))
            ;; STUDY: weird that they use word
            ;; toggle, instead of just set it.
            ;;
            ;; Why not?
            ;;   => `undo-tree-visualizer-show-diff'
            ;; or
            ;;   => `undo-tree-visualizer-hide-diff'
            (when jcs-undo-tree-auto-show-diff
              (undo-tree-visualizer-toggle-diff)))))
    ;; In Emacs, undo/redo is the same thing.
    (call-interactively #'undo)))

;;---------------------------------------------
;; Return
;;---------------------------------------------

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
      (call-interactively #'goto-address-at-point))))

;;----------------------------------------------
;; Tab
;;----------------------------------------------

;;;###autoload
(defun jcs-tab-key ()
  "Global TAB key."
  (interactive)
  (unless (ignore-errors (call-interactively #'jcs-yas-expand))
    (unless (ignore-errors (call-interactively #'dabbrev-expand))
      (jcs-insert-spaces-by-tab-width))))

;;----------------------------------------------
;; Mark
;;----------------------------------------------

(defvar-local jcs-marking-whole-buffer nil
  "Marking the whole buffer now?")

(defvar-local jcs-marking-whole-buffer-cmd-count 0
  "Count up the command used after marking whole buffer.")

;;;###autoload
(defun jcs-mark-whole-buffer ()
  "Mark the whole buffer."
  (interactive)
  (call-interactively #'mark-whole-buffer)
  (setq-local jcs-marking-whole-buffer-cmd-count 0)
  (setq-local jcs-marking-whole-buffer t))

;;----------------------------------------------
;; Overwrite (Insert toggle)
;;----------------------------------------------

;;;###autoload
(defun jcs-overwrite-mode ()
  "Wrap <insert> key with cursor changes."
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

;;---------------------------------------------
;;-- Source --
;;      Deletion: http://ergoemacs.org/emacs/emacs_kill-ring.html
;;---------------------------------------------

;;;###autoload
(defun jcs-kill-whole-line ()
  "Deletes a line, but does not put it in the `kill-ring'."
  (interactive)
  (let ((kill-ring))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (let (;; Record down the column before
            ;; killing the whole line.
            (before-column-num (current-column)))

        ;; Do kill the whole line!
        (move-beginning-of-line 1)
        (kill-line 1)

        ;; Goto the same column as before we do the killing
        ;; the whole line operations above.
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
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

;;;###autoload
(defun jcs-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

;;;###autoload
(defun jcs-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (jcs-delete-word (- arg))))

;;;###autoload
(defun jcs-forward-delete-word (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (jcs-delete-word (+ arg))))

(defun jcs-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

;;;###autoload
(defun jcs-duplicate-line ()
  "Duplicate the line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;;---------------------------------------------
;; Indent moving UP or DOWN.
;;---------------------------------------------

(defun jcs-can-do-smart-indent-p ()
  "Check smart indent conditions."
  (and (not mark-active)
       (jcs-buffer-name-or-buffer-file-name)
       (not buffer-read-only)))

;;;###autoload
(defun jcs-smart-indent-up ()
  "Indent line after move up one line.
This function uses `indent-for-tab-command'."
  (interactive)
  (if (jcs-can-do-smart-indent-p)
      (progn
        (jcs-previous-line)
        (indent-for-tab-command))
    (jcs-previous-line)))

;;;###autoload
(defun jcs-smart-indent-up-by-mode ()
  "Indent line after move up one line.
Use `indent-according-to-mode' instead `indent-for-tab-command'."
  (interactive)
  (if (jcs-can-do-smart-indent-p)
      (progn
        (jcs-previous-line)
        (indent-according-to-mode))
    (jcs-previous-line)))

;;;###autoload
(defun jcs-smart-indent-down ()
  "Indent line after move down one line.
This function uses `indent-for-tab-command'."
  (interactive)
  (if (jcs-can-do-smart-indent-p)
      (progn
        (jcs-next-line)
        (indent-for-tab-command))
    (jcs-next-line)))

;;;###autoload
(defun jcs-smart-indent-down-by-mode ()
  "Indent line after move down one line.
Use `indent-according-to-mode' instead `indent-for-tab-command'."
  (interactive)
  (if (jcs-can-do-smart-indent-p)
      (progn
        (jcs-next-line)
        (indent-according-to-mode))
    (jcs-next-line)))


;;==============================
;;      Format File
;;------------------------

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
      (call-interactively #'indent-region)
    (call-interactively #'jcs-format-document)))

;;;###autoload
(defun jcs-align-region-by-points (regexp pnt-min pnt-max)
  "Align current selected region.

REGEXP : reqular expression use to align.
PNT-MIN: point min.
PNT-MAX: point max."
  (interactive)
  (align pnt-min pnt-max)
  (align-regexp pnt-min pnt-max regexp 1 1 t))

;;;###autoload
(defun jcs-align-region (regexp)
  "Align current selected region.
REGEXP : reqular expression use to align."
  (interactive)
  (jcs-align-region-by-points regexp (region-beginning) (region-end))
  ;; Deactive region no matter what.
  (deactivate-mark))

;;;###autoload
(defun jcs-align-document (regexp)
  "Align current document.
REGEXP : reqular expression use to align."
  (interactive)
  ;; URL: https://www.emacswiki.org/emacs/AlignCommands
  ;; align the whole doc.
  (jcs-align-region-by-points regexp (point-min) (point-max)))

;;;###autoload
(defun jcs-align-region-or-document ()
  "Either align the region or document depend on if there is \
region selected?"
  (interactive)
  (save-excursion
    (let (;; NOTE: this is the most common one.
          ;; Compatible to all programming languages use equal
          ;; sign to assign value.
          (align-regexp-string-code "\\(\\s-*\\)[=]")
          ;; NOTE: Default support `//' and `/**/'
          ;; comment symbols.
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

            (when (> (point) pnt-min)
              (setq pnt-max (point))))
        ;; NOTE: Align whole document.
        (jcs-align-document align-regexp-string-code)

        ;; NOTE: These assigns does nothing for now.
        ;; Just in case we dont apply weird value, assign
        ;; default document info.
        (setq pnt-min (point-min))
        (setq pnt-max (point-max)))

      ;; Align comment segment.
      (jcs-align-region-by-points align-regexp-string-comment
                                  pnt-min
                                  pnt-max)
      )))

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
  (let ((was-flycheck flycheck-mode)
        (was-readonly buffer-read-only))

    (revert-buffer :ignore-auto :noconfirm :preserve-modes)

    ;; Revert all the enabled mode.
    (when was-flycheck
      (flycheck-mode 1))
    (when was-readonly
      (read-only-mode 1))))

;;;###autoload
(defun jcs-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted.
They will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  ;; SOURCE: https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors
  (save-excursion
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buf)))
          (if (file-readable-p filename)
              ;; If the file exists and is readable, revert the buffer.
              (with-current-buffer buf
                (jcs-revert-buffer-no-confirm))
            ;; Otherwise, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buf)
              ;;(message "Killed non-existing/unreadable file buffer: %s" filename)
              )))))
    ;;(message "Finished reverting buffers containing unmodified files.")
    ))

;;;###autoload
(defun jcs-other-window-next (&optional cnt not-all-frame)
  "Move to the next window.
CNT : Move count.
NOT-ALL-FRAME : Default boundaries is all frame, limit to curent frame."
  (interactive)
  (when (or (not cnt)
            (not (numberp cnt)))
    (setq cnt 1))
  ;; find next window and jump to that window.
  (if not-all-frame
      (other-window cnt nil)
    (other-window cnt t))
  (select-frame-set-input-focus (selected-frame))
  ;; Update the selected window if speedbar is active.
  (jcs-update-speedbar-record-after-select-new-window))

;;;###autoload
(defun jcs-other-window-prev (&optional cnt not-all-frame)
  "Move to the previous window.
CNT : Move count.
NOT-ALL-FRAME : Default boundaries is all frame, limit to curent frame."
  (interactive)
  (when (or (not cnt)
            (not (numberp cnt)))
    (setq cnt -1))
  ;; find previous window and jump to that window.
  (if not-all-frame
      (other-window cnt nil)
    (other-window cnt t))
  (select-frame-set-input-focus (selected-frame))
  ;; Update the selected window if speedbar is active.
  (jcs-update-speedbar-record-after-select-new-window))

;;;###autoload
(defun jcs-scroll-up-one-line (&optional n)
  "Scroll the text up one line.
N : line to scroll."
  (interactive)
  (let ((rel-n (if n n 1)))
    (scroll-up rel-n)))

;;;###autoload
(defun jcs-scroll-down-one-line (&optional n)
  "Scroll the text down one line.
N : line to scroll."
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
      (unless (= (line-number-at-pos) 1)
        (forward-line -1))
      (while (and (jcs-current-line-empty-p)
                  (< rec-point (point)))
        (jcs-kill-whole-line)
        (forward-line -1)))))

;;;###autoload
(defun jcs-delete-trailing-whitespace-except-current-line ()
  "Delete the trailing whitespace for whole document execpt \
the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (> (point-max) end)
        (delete-trailing-whitespace (1+ end) (point-max)))
      (when (< (point-min) begin)
        (delete-trailing-whitespace (point-min) (1- begin))))))

;;----------------------------------------------
;; Move Current Line Up or Down
;;----------------------------------------------

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

;;=================================
;;        Word Case
;;-------------------------

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

;;=================================
;;     Tabify / Unabify
;;-------------------------

;;;###autoload
(defun jcs-untabify-buffer (&optional start end)
  "Untabify the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((start-pt (if start start (point-min)))
            (end-pt (if end end (point-max))))
        (widen)
        (untabify start-pt end-pt)))))

;;;###autoload
(defun jcs-tabify-buffer (&optional start end)
  "Tabify the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((start-pt (if start start (point-min)))
            (end-pt (if end end (point-max))))
        (widen)
        (tabify start-pt end-pt)))))

;;=================================
;; Save Buffer
;;-------------------------

(defun jcs-do-stuff-before-save (&rest _)
  "Do stuff before save command executed.
ARG : Match with `save-buffer' command."
  ;; NOTE: If company menu currently active, abort it.
  (company-abort))
(advice-add 'save-buffer :before #'jcs-do-stuff-before-save)

(defun jcs-do-stuff-after-save (&rest _)
  "Do stuff after save command executed.
ARG : Match with `save-buffer' command."
  ;; NOTE: Is we found `*undo-tree*' buffer, we try to close it.
  (save-selected-window
    (when (ignore-errors (jcs-jump-shown-to-buffer "*undo-tree*"))
      (jcs-maybe-kill-this-buffer t)))
  (jcs-update-line-number-each-window))
(advice-add 'save-buffer :after #'jcs-do-stuff-after-save)

;;;###autoload
(defun jcs-untabify-save-buffer ()
  "Untabify the file and save the buffer."
  (interactive)
  (let (deactivate-mark
        truncate-lines)
    (jcs-delete-trailing-whitespace-except-current-line)
    (jcs-remove-trailing-lines-end-buffer)
    (jcs-untabify-buffer)
    (save-buffer)))

;;;###autoload
(defun jcs-tabify-save-buffer ()
  "Tabify the file and save the buffer."
  (interactive)
  (let (deactivate-mark
        truncate-lines)
    (jcs-delete-trailing-whitespace-except-current-line)
    (jcs-remove-trailing-lines-end-buffer)
    (jcs-tabify-buffer)
    (save-buffer)))

;;=================================
;; Find file
;;-------------------------

;;;###autoload
(defun jcs-same-file-other-window ()
  "This will allow us open the same file in another window."
  (interactive)
  (save-selected-window
    (let ((buf-name (buffer-name)))
      (other-window 1)
      (switch-to-buffer buf-name))))

;;----------------------------------------------
;; Rename file.
;;----------------------------------------------

;;;###autoload
(defun jcs-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  ;; SOURCE: https://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name
  ;; URL: http://www.whattheemacsd.com/
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
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


;;----------------------------------------------
;; Kill Buffer
;;----------------------------------------------

(defun jcs-advice-kill-this-buffer-around (orig-fun &rest args)
  "Advice around execute `kill-this-buffer' command."
  (let ((target-kill-buffer (jcs-buffer-name-or-buffer-file-name))
        (undoing-buffer-name nil)
        (jumped-to-utv nil))
    (save-selected-window
      (setq jumped-to-utv
            (ignore-errors
              (jcs-jump-shown-to-buffer undo-tree-visualizer-buffer-name)))
      (when jumped-to-utv
        (setq undoing-buffer-name (buffer-name undo-tree-visualizer-parent-buffer))))

    (apply orig-fun args)

    ;; If `undo-tree' visualizer exists, kill it too.
    (when jumped-to-utv
      (when (and undoing-buffer-name
                 (string-match-p undoing-buffer-name target-kill-buffer)
                 ;; Only close `undo-tree' when buffer is killed.
                 (not (string= target-kill-buffer (jcs-buffer-name-or-buffer-file-name))))
        (save-selected-window
          (jcs-jump-shown-to-buffer undo-tree-visualizer-buffer-name)
          ;; NOTE: This prompt error, but does not matter.
          ;; Just force to quite it!
          (ignore-errors (undo-tree-visualizer-quit)))))))
(advice-add 'kill-this-buffer :around #'jcs-advice-kill-this-buffer-around)

;;;###autoload
(defun jcs-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (kill-this-buffer)

  ;; Refresh buffer menu once.
  (jcs-buffer-menu-refresh-buffer)

  ;; If still in the buffer menu, try switch to the
  ;; previous buffer
  (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
    (previous-buffer)))

;;;###autoload
(defun jcs-maybe-kill-this-buffer (&optional ecp-same)
  "Kill the buffer if this file is the only file. Otherwise just
switch to the previous buffer.
ECP-SAME : Exception for the same buffer."
  (interactive)
  (let ((is-killed nil))
    (if (or (>= (jcs-buffer-showns (buffer-name)) 2)
            ;; NOTE: If you don't want `*Buffer-List*'
            ;; window open in at least two window and get killed
            ;; at the same time. Enable the line under.
            ;;(jcs-is-current-major-mode-p "Buffer-menu-mode")
            )
        (jcs-switch-to-previous-buffer)
      (jcs-kill-this-buffer)
      (setq is-killed t)

      ;; NOTE: After kill the buffer, if the buffer
      ;; appear in multiple windows then we do switch to
      ;; previous buffer again. Hence, it will not show
      ;; repeated buffer at the same time in different windows.
      (when (and (>= (jcs-buffer-showns (buffer-name)) 2)
                 (not ecp-same))
        (jcs-switch-to-previous-buffer)

        ;; If is something from default Emacs's buffer,
        ;; switch back to previous buffer once again.
        ;;
        ;; This will solve if there is only one file opened,
        ;; and switch to none sense buffer issue.
        ;;
        ;; None sense buffer or Emacs's default buffer is
        ;;   -> *GNU Emacs*
        ;;   -> *scratch*
        ;;   , etc.
        (when (and (not (buffer-file-name))
                   (not (= (jcs-valid-buffers-in-buffer-list) 0)))
          (jcs-switch-to-next-buffer-not-nil))))
    is-killed))

;;;###autoload
(defun jcs-reopen-this-buffer ()
  "Kill the current buffer and open it again."
  (interactive)
  (save-selected-window
    (let ((buf-name (buffer-file-name))
          (win-cnt-lst '())
          (first-vs-ln-lst '())
          (record-pt-lst '())
          (win-cnt 0)
          (win-id-cnt 0))
      (when buf-name
        ;; Record down all the window information with the same
        ;; buffer opened.
        (jcs-walk-through-all-windows-once
         (lambda ()
           (when (string= (buffer-file-name) buf-name)
             (push win-cnt win-cnt-lst)
             (push (point) record-pt-lst)
             (push (jcs-first-visible-line-in-window) first-vs-ln-lst))
           (setq win-cnt (+ win-cnt 1))))

        ;; Reverse the order to have the information order corresponding
        ;; to the window order correctly.
        (setq record-pt-lst (reverse record-pt-lst))
        (setq first-vs-ln-lst (reverse first-vs-ln-lst))

        (jcs-kill-this-buffer)

        (setq win-cnt 0)

        ;; Restore the window information after, including
        ;; opening the same buffer.
        (jcs-walk-through-all-windows-once
         (lambda ()
           (when (jcs-is-contain-list-integer win-cnt-lst win-cnt)
             (find-file buf-name)
             (jcs-make-first-visible-line-to (nth win-id-cnt first-vs-ln-lst))
             (goto-char (nth win-id-cnt record-pt-lst))
             (setq win-id-cnt (+ win-id-cnt 1)))
           (setq win-cnt (+ win-cnt 1))))

        (message "Reopened file => '%s'" buf-name)))))

;;----------------------------------------------
;; Search/Kill word capital.
;;----------------------------------------------

(defvar jcs-first-char-is-char nil
  "Check the first character a character.")
(defvar jcs-check-first-char nil
  "Boolean to check the first character")
(defvar jcs-found-first-char nil
  "Found the first character in search?")

;;;###autoload
(defun jcs-backward-kill-word-capital ()
  "Backward delete the word unitl the word is capital."
  (interactive)

  (unless jcs-check-first-char
    ;; check the first character a character
    (when (jcs-word-p (jcs-get-current-char-byte))
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  ;; found the first character!
  (when (jcs-word-p (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (not jcs-found-first-char)
      (if jcs-first-char-is-char
          (backward-delete-char 1)
        (backward-delete-char 1)
        (jcs-backward-kill-word-capital))
    (if (not (jcs-word-p (jcs-get-current-char-byte)))
        (progn
          ;; NOTE: Here is end of the recursive
          ;; function loop...
          )
      (if (jcs-uppercase-p (jcs-get-current-char-byte))
          ;; NOTE: Here is end of the recursive
          ;; function loop...
          (backward-delete-char 1)
        (backward-delete-char 1)
        (jcs-backward-kill-word-capital))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;;###autoload
(defun jcs-forward-kill-word-capital ()
  "Forward delete the word unitl the word is capital."
  (interactive)

  (unless jcs-check-first-char
    (backward-delete-char -1)
    ;; check the first character a character
    (when (jcs-word-p (jcs-get-current-char-byte))
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  (forward-char 1)

  ;; found the first character!
  (when (jcs-word-p (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (not jcs-found-first-char)
      (if jcs-first-char-is-char
          (backward-delete-char 1)
        (backward-delete-char 1)
        (jcs-forward-kill-word-capital))
    (if (or (not (jcs-word-p (jcs-get-current-char-byte)))
            (jcs-is-end-of-line-p))
        ;; NOTE: Here is end of the recursive function loop...
        (backward-delete-char 1)
      (if (jcs-uppercase-p (jcs-get-current-char-byte))
          ;; NOTE: Here is end of the recursive function loop...
          (forward-char -1)
        (backward-delete-char 1)
        (jcs-forward-kill-word-capital))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;;###autoload
(defun jcs-backward-capital-char ()
  "Backward search capital character and set the cursor
to the point."
  (interactive)

  (unless jcs-check-first-char
    ;; check the first character a character
    (when (jcs-word-p (jcs-get-current-char-byte))
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  ;; found the first character!
  (when (jcs-word-p (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (not jcs-found-first-char)
      (if jcs-first-char-is-char
          (backward-char 1)
        (backward-char 1)
        (jcs-backward-capital-char))
    (if (not (jcs-word-p (jcs-get-current-char-byte)))
        (progn
          ;; NOTE: Here is end of the recursive
          ;; function loop...
          )
      (if (jcs-uppercase-p (jcs-get-current-char-byte))
          ;; NOTE: Here is end of the recursive
          ;; function loop...
          (backward-char 1)
        (backward-char 1)
        (jcs-backward-capital-char))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;;###autoload
(defun jcs-forward-capital-char ()
  "Forward search capital character and set the cursor to
the point."
  (interactive)
  ;; If the point is at the first character, we will get the error.
  ;; So move forward a character then check.
  (when (= (point-min) (point))
    (forward-char 1))

  (unless jcs-check-first-char
    ;; check the first character a character
    (when (jcs-word-p (jcs-get-current-char-byte))
      (forward-char 1)
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  ;; found the first character!
  (when (jcs-word-p (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (not jcs-found-first-char)
      (progn
        (forward-char 1)
        (jcs-forward-capital-char))
    (if (not (jcs-word-p (jcs-get-current-char-byte)))
        ;; NOTE: Here is end of the recursive
        ;; function loop...
        (backward-char 1)
      (if (jcs-uppercase-p (jcs-get-current-char-byte))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            )
        (forward-char 1)
        (jcs-forward-capital-char))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;----------------------------------------------
;; Delete Repeatedly
;;----------------------------------------------

;;;###autoload
(defun jcs-backward-delete-current-char-repeat ()
  "Delete the current character repeatedly, util it meet the \
character is not the same as current char.  (Backward)"
  (interactive)
  (jcs-delete-char-repeat (jcs-get-current-char-string) t))

;;;###autoload
(defun jcs-forward-delete-current-char-repeat ()
  "Delete the current character repeatedly, util it meet the \
character is not the same as current char.  (Forward)"
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

;;----------------------------------------------
;; Delete inside a Character.
;;----------------------------------------------

(defun jcs-find-start-char (start-char preserve-point)
  "Find the starting character."
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
  "Find the ending character."
  (let ((inhibit-message t)
        (end-point nil))
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

        ;; If nested level is lower than 0, meaning is not between
        ;; the nested START-CHAR and END-CHAR.
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

;;----------------------------------------------
;; Electric Pair
;;----------------------------------------------

(defun jcs-get-open-pair-char (c)
  "Get the open pairing character from C."
  (let ((pair-char nil))
    (cond ((string= c "\"") (setq pair-char "\""))
          ((string= c "'") (setq pair-char "'"))
          ((string= c ")") (setq pair-char "("))
          ((string= c "]") (setq pair-char "["))
          ((string= c "}") (setq pair-char "{"))
          ((string= c "`") (setq pair-char "`")))
    pair-char))

(defun jcs-get-close-pair-char (c)
  "Get the close pairing character from C."
  (let ((pair-char nil))
    (cond ((string= c "\"") (setq pair-char "\""))
          ((string= c "'") (setq pair-char "'"))
          ((string= c "(") (setq pair-char ")"))
          ((string= c "[") (setq pair-char "]"))
          ((string= c "{") (setq pair-char "}"))
          ((string= c "`") (setq pair-char "`")))
    pair-char))


(defun jcs-process-close-pair-char (cpc)
  "Process the close pair character.
CPC : close pair character."
  (when (and cpc
             (not (jcs-is-end-of-buffer-p)))
    (save-excursion
      (forward-char 1)
      (when (jcs-current-char-equal-p cpc)
        (backward-delete-char 1)))))

(defun jcs-process-close-pair-char-seq (cc)
  "Process close pair character sequence.
CC : current character before character deletion occured."
  (save-excursion
    (cond (;; Seq => /**/
           (string= cc "*")
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

;;;###autoload
(defun jcs-electric-backspace ()
  "Electric backspace key."
  (interactive)
  (let* ((cc (jcs-get-current-char-string))
         (cpc (jcs-get-close-pair-char cc)))
    (jcs-own-delete-backward-char)
    (jcs-process-close-pair-char cpc)
    (jcs-process-close-pair-char-seq cc)))

;;----------------------------------------------
;; Isearch
;;----------------------------------------------

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

;;----------------------------------------------
;; Multiple Cursors
;;----------------------------------------------

;;;###autoload
(defun jcs-mc/mark-previous-like-this ()
  "Smart marking previous line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-next-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-previous-like-this)))))

;;;###autoload
(defun jcs-mc/mark-next-like-this ()
  "Smart marking next line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-previous-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-next-like-this)))))

;;----------------------------------------------
;; Folding / Unfolding
;;----------------------------------------------

;;;###autoload
(defun jcs-close-all-nodes ()
  (interactive)
  (require 'origami)
  (call-interactively #'origami-close-all-nodes))

;;;###autoload
(defun jcs-open-all-nodes ()
  (interactive)
  (require 'origami)
  (call-interactively #'origami-open-all-nodes))


(provide 'jcs-edit)
;;; jcs-edit.el ends here
