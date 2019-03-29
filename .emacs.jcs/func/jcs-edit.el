;;; jcs-edit.el --- When editing the file.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------
;; Undo/Redo
;;----------------------------------------------

(require 'undo-tree)

;;
;; NOTE(jenchieh): This is compatible with other
;; text editor or IDE. Most IDE/text editor have this
;; undo/redo system as default.
;;
(defvar jcs-use-undo-tree-key t
  "Using the undo tree key in stead of normal Emacs's undo key.
This variable must be use with `jcs-undo' and `jcs-redo' functions.")

;; NOTE(jenchieh): Active this will cause huge amount of
;; performance, consider this before active.
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

;;;###autoload
(defun jcs-undo ()
  "Undo key."
  (interactive)
  (if jcs-use-undo-tree-key
      (progn
        (undo-tree-undo)
        (save-selected-window
          (undo-tree-visualize)
          ;; STUDY(jenchieh): weird that they use word
          ;; toggle, instead of just set it.
          ;;
          ;; Why not?
          ;;   => `undo-tree-visualizer-show-diff'
          ;; or
          ;;   => `undo-tree-visualizer-hide-diff'
          (when jcs-undo-tree-auto-show-diff
            (undo-tree-visualizer-toggle-diff))))
    (call-interactively #'undo)))

;;;###autoload
(defun jcs-redo ()
  "Redo key."
  (interactive)
  (if jcs-use-undo-tree-key
      (progn
        (undo-tree-redo)
        (save-selected-window
          (undo-tree-visualize)
          ;; STUDY(jenchieh): weird that they use word
          ;; toggle, instead of just set it.
          ;;
          ;; Why not?
          ;;   => `undo-tree-visualizer-show-diff'
          ;; or
          ;;   => `undo-tree-visualizer-hide-diff'
          (when jcs-undo-tree-auto-show-diff
            (undo-tree-visualizer-toggle-diff))))
    ;; In Emacs, undo/redo is the same thing.
    (call-interactively #'redo)))

;;----------------------------------------------
;; Tab
;;----------------------------------------------

;;;###autoload
(defun jcs-tab-key ()
  "TAB key for JayCeS usage."
  (interactive)
  (if (or (jcs-current-whitespace-or-tab-p)
          (jcs-is-beginning-of-line-p)
          (jcs-is-end-of-line-p))
      (progn
        (jcs-insert-spaces-by-tab-width))
    (progn
      ;; NOTE(jenchieh): Default tab function put here..
      (call-interactively #'dabbrev-expand))))

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
      (progn
        (let (;; Record down the column before
              ;; killing the whole line.
              (before-column-num (current-column)))

          ;; Do kill the whole line!
          (move-beginning-of-line 1)
          (kill-line 1)

          ;; Goto the same column as before we do the killing
          ;; the whole line operations above.
          (move-to-column before-column-num))))))

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
;; After moving UP one line, do identation.
;;---------------------------------------------

;;;###autoload
(defun jcs-smart-indent-up ()
  "Indent line after move up one line.
This function uses `indent-for-tab-command'."
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name))
      (progn
        (previous-line 1)
        (indent-for-tab-command))
    (previous-line 1)))

;;;###autoload
(defun jcs-smart-indent-up-by-mode ()
  "Indent line after move up one line.
Use `indent-according-to-mode' instead `indent-for-tab-command'."
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name))
      (progn
        (previous-line 1)
        (indent-according-to-mode))
    (previous-line 1)))


;;---------------------------------------------
;; After moving DOWN one line, do identation.
;;---------------------------------------------

;;;###autoload
(defun jcs-smart-indent-down ()
  "Indent line after move down one line.
This function uses `indent-for-tab-command'."
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name))
      (progn
        (next-line 1)
        (indent-for-tab-command))
    (next-line 1)))

;;;###autoload
(defun jcs-smart-indent-down-by-mode ()
  "Indent line after move down one line.
Use `indent-according-to-mode' instead `indent-for-tab-command'."
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name))
      (progn
        (next-line 1)
        (indent-according-to-mode))
    (next-line 1)))

;;;###autoload
(defun jcs-smart-select-home ()
  "Set the curosr beginning of the line from the current \
frame, so it does not goto the beginning of the line first."
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (jcs-back-to-indentation-or-beginning))

;;;###autoload
(defun jcs-smart-select-end ()
  "Set the cursor to the  end of the line from the current frame."
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (end-of-line))


;;========================================
;;      JCS Format File
;;----------------------------------

;;;###autoload
(defun jcs-format-document ()
  "Format current document."
  (interactive)
  ;; indent the whole doc.
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun jcs-format-region-or-document ()
  "Format the document if there are no region apply."
  (interactive)

  (if (use-region-p)
      (progn
        (call-interactively #'indent-region))
    (progn
      (call-interactively #'jcs-format-document))))

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
URL(jenchieh): https://www.emacswiki.org/emacs/AlignCommands

REGEXP : reqular expression use to align."
  (interactive)

  ;; align the whole doc.
  (jcs-align-region-by-points regexp (point-min) (point-max)))

;;;###autoload
(defun jcs-align-region-or-document ()
  "Either align the region or document depend on if there is \
region selected?"
  (interactive)

  (save-excursion
    (let (;; NOTE(jenchieh): this is the most common one.
          ;; Compatible to all programming languages use equal
          ;; sign to assign value.
          (align-regexp-string-code "\\(\\s-*\\)[=]")
          ;; NOTE(jenchihe): Default support `//' and `/**/'
          ;; comment symbols.
          (align-regexp-string-comment "\\(\\s-*\\) /[/*]")
          (pnt-min nil)
          (pnt-max nil))

      ;; Code RegExp String
      (cond ((or (jcs-is-current-major-mode-p "nasm-mode"))
             (progn
               (setq align-regexp-string-code "\\(\\s-*\\)equ ")
               ))
            ((or (jcs-is-current-major-mode-p "go-mode"))
             (progn
               (setq align-regexp-string-code "\\(\\s-*\\) := ")
               ))
            )

      ;; Comment RegExp String
      (cond ((or (jcs-is-current-major-mode-p "nasm-mode"))
             (progn
               (setq align-regexp-string-comment "\\(\\s-*\\)               [;]")
               ))
            )

      (if (jcs-is-region-selected-p)
          ;; NOTE(jenchieh): Align region only.
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
        ;; NOTE(jenchieh): Align whole document.
        (progn
          (jcs-align-document align-regexp-string-code)

          ;; NOTE(jenchieh): These assigns does nothing for now.
          ;; Just in case we dont apply weird value, assign
          ;; default document info.
          (setq pnt-min (point-min))
          (setq pnt-max (point-max))))

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
      (progn
        (align-regexp (region-beginning) (region-end)
                      (concat "\\(\\s-*\\)" regexp) 1 1 t))
    (progn
      (align-regexp (point-min) (point-max)
                    (concat "\\(\\s-*\\)" regexp) 1 1 t))))

;;;###autoload
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  ;; SOURCE(jenchieh):
  ;; 1) http://emacs.stackexchange.com/questions/169/how-do-i-reload-a-file-in-a-buffer
  ;; 2) http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;;;###autoload
(defun jcs-other-window-next ()
  "Cycle through window and frame. (next window/frame)"
  (interactive)
  ;; SOURCE: http://emacs.stackexchange.com/questions/628/cycle-between-windows-in-all-frames
  ;; find next window and jump to that window.
  (other-window 1 t)
  (select-frame-set-input-focus (selected-frame))

  ;; Update the selected window if speedbar is active.
  (when (and (sr-speedbar-exist-p)
             (not (jcs-is-current-major-mode-p "speedbar-mode")))
             (setq jcs-sr-speedbar-record-selected-window (selected-window))))

;;;###autoload
(defun jcs-other-window-prev ()
  "Cycle through window and frame.(previous window/frame)"
  (interactive)
  ;; find previous window and jump to that window.
  (other-window -1 t)
  (select-frame-set-input-focus (selected-frame))

  ;; Update the selected window if speedbar is active.
  (when (and (sr-speedbar-exist-p)
             (not (jcs-is-current-major-mode-p "speedbar-mode")))
    (setq jcs-sr-speedbar-record-selected-window (selected-window))))

;;;###autoload
(defun jcs-scroll-up-one-line ()
  "Scroll the text up one line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun jcs-scroll-down-one-line ()
  "Scroll the text down one line."
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun jcs-delete-trailing-whitespace-except-current-line ()
  "Delete the trailing whitespace for whole document execpt \
the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (1+ end) (point-max))
          (delete-trailing-whitespace))))))

;;----------------------------------------------
;; Move Current Line Up or Down
;;
;; SOURCE(jenchieh): http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
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
;; Save Buffer
;;-------------------------

(defun jcs-do-stuff-after-save ()
  "Do stuff after save command executed."
  ;; NOTE(jenchieh): Is we found `*undo-tree*' buffer, we
  ;; try to close it.
  (let ((prev-frame (selected-frame)))
    (save-selected-window
      (when (or (ignore-errors (jcs-jump-shown-to-buffer "*undo-tree*")))
        (jcs-maybe-kill-this-buffer t)))
    (select-frame-set-input-focus prev-frame)
    (jcs-update-line-number-each-window)))
(advice-add 'save-buffer :after #'jcs-do-stuff-after-save)

;;;###autoload
(defun jcs-untabify-save-buffer ()
  "Save buffer / Utabify the document / Delete all trailing
whitespaces."
  (interactive)
  (jcs-delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

;;;###autoload
(defun jcs-tabify-save-buffer ()
  " Save buffer / Tabify the document / Delete all trailing
whitespaces. NOTE(JenChieh): Makefile does not support space,
so we must convert spaces to tab."
  (interactive)
  (jcs-delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (tabify (point-min) (point-max))))
  (save-buffer))

;;=================================
;; Find file
;;-------------------------

;;;###autoload
(defun jcs-find-file-other-window ()
  "This will allow us open the same file in another window."
  (interactive)
  (when (buffer-file-name)
    (find-file-other-window buffer-file-name)
    (jcs-other-window-prev)))

;;;###autoload
(defun jcs-smart-find-file-in-project-in-another-window ()
  "This will open the file in another window using 'find-file-
project.el' plugin."
  (interactive)
  (if (ignore-errors (find-file-in-project t))
      (progn
        ;; Reach here mean success using 'find-file-in-
        ;; project.el' plugin.
        )
    (ido-find-file-other-window)))

;;;###autoload
(defun jcs-smart-find-file-in-project ()
  "This will open the file in current window using 'find-
file-project.el' plugin."
  (interactive)
  (if (ignore-errors (find-file-in-project))
      (progn
        ;; Reach here mean success using 'find-file-in-
        ;; project.el' plugin.
        )
    (ido-find-file)))


;;----------------------------------------------
;; Rename file.
;;----------------------------------------------

;;;###autoload
(defun jcs-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  ;; SOURCE(jenchieh): https://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name
  ;; URL(jenchieh): http://www.whattheemacsd.com/
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

;;;###autoload
(defun jcs-erase-message-buffer ()
  "Erase the *Message* buffer."
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
      (save-selected-window
        (ignore-errors
          (jcs-jump-shown-to-buffer "*Buffer List*"))
        (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
          ;; NOTE(jenchieh): Refresh buffer menu once.
          (jcs-buffer-menu))))))

;;;###autoload
(defun jcs-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (kill-this-buffer)

  (save-selected-window
    (ignore-errors
      (jcs-jump-shown-to-buffer "*Buffer List*"))
    (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
      ;; NOTE(jenchieh): Refresh buffer menu once.
      (jcs-buffer-menu)))

  ;; If still in the buffer menu, try switch to the
  ;; previous buffer
  (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
    (jcs-switch-to-previous-buffer)))

;;;###autoload
(defun jcs-maybe-kill-this-buffer (&optional ecp-same)
  "Kill the buffer if this file is the only file. Otherwise just
switch to the previous buffer.
ECP-SAME : Exception for the same buffer."
  (interactive)
  (let ((is-killed nil))
    (if (or (>= (jcs-buffer-showns (buffer-name)) 2)
            ;; NOTE(jenchieh): If you don't want `*Buffer-List*'
            ;; window open in at least two window and get killed
            ;; at the same time. Enable the line under.
            ;;(jcs-is-current-major-mode-p "Buffer-menu-mode")
            )
        (jcs-switch-to-previous-buffer)
      (progn
        (jcs-kill-this-buffer)
        (setq is-killed t)
        ;; NOTE(jenchieh): After kill the buffer, if the buffer
        ;; appear in multiple windows then we do switch to
        ;; previous buffer again. Hence, it will not show
        ;; repeated buffer at the same time in different windows.
        (when (and (>= (jcs-buffer-showns (buffer-name)) 2)
                   (not ecp-same))
          (jcs-switch-to-previous-buffer))))
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

        (message "Reopened buffer => '%s'" buf-name)))))

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

  (when (eq jcs-check-first-char nil)
    ;; check the first character a character
    (when (wordp (jcs-get-current-char-byte))
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  ;; found the first character!
  (when (wordp (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (eq jcs-found-first-char nil)
      (progn
        (if (eq jcs-first-char-is-char t)
            (progn
              (backward-delete-char 1))
          (progn
            (backward-delete-char 1)
            (jcs-backward-kill-word-capital))))
    (progn
      (if (not (wordp (jcs-get-current-char-byte)))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            )
        (progn
          (if (uppercasep (jcs-get-current-char-byte))
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                (backward-delete-char 1))
            (progn
              (backward-delete-char 1)
              (jcs-backward-kill-word-capital)))))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;;###autoload
(defun jcs-forward-kill-word-capital ()
  "Forward delete the word unitl the word is capital."
  (interactive)

  (when (eq jcs-check-first-char nil)
    (backward-delete-char -1)
    ;; check the first character a character
    (when (wordp (jcs-get-current-char-byte))
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  (forward-char 1)

  ;; found the first character!
  (when (wordp (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (eq jcs-found-first-char nil)
      (progn
        (if (eq jcs-first-char-is-char t)
            (progn
              (backward-delete-char 1))
          (progn
            (backward-delete-char 1)
            (jcs-forward-kill-word-capital))))
    (progn
      (if (or (not (wordp (jcs-get-current-char-byte)))
              (jcs-is-end-of-line-p))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            (backward-delete-char 1))
        (progn
          (if (uppercasep (jcs-get-current-char-byte))
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                (forward-char -1))
            (progn
              (backward-delete-char 1)
              (jcs-forward-kill-word-capital)))))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;;###autoload
(defun jcs-backward-capital-char ()
  "Backward search capital character and set the cursor
to the point."
  (interactive)

  (when (eq jcs-check-first-char nil)
    ;; check the first character a character
    (when (wordp (jcs-get-current-char-byte))
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  ;; found the first character!
  (when (wordp (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (eq jcs-found-first-char nil)
      (progn
        (if (eq jcs-first-char-is-char t)
            (progn
              (backward-char 1))
          (progn
            (backward-char 1)
            (jcs-backward-capital-char))))
    (progn
      (if (not (wordp (jcs-get-current-char-byte)))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            )
        (progn
          (if (uppercasep (jcs-get-current-char-byte))
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                (backward-char 1))
            (progn
              (backward-char 1)
              (jcs-backward-capital-char)))))))

  ;; reset triggers
  (setq jcs-first-char-is-char nil)
  (setq jcs-check-first-char nil)
  (setq jcs-found-first-char nil))

;;;###autoload
(defun jcs-forward-capital-char ()
  "Forward search capital character and set the cursor to
the point."
  (interactive)

  (save-excursion
    ;; Get the first 'Beginning of buffer's Point'.
    (beginning-of-buffer)
    (setq beginningBufferPoint (point)))

  ;; If the point is at the first character, we will get the error.
  ;; So move forward a character then check.
  (when (= beginningBufferPoint (point))
    (forward-char 1))

  (when (eq jcs-check-first-char nil)
    ;; check the first character a character
    (when (wordp (jcs-get-current-char-byte))
      (forward-char 1)
      (setq jcs-first-char-is-char t))

    ;; check the first character mission complete.
    (setq jcs-check-first-char t))

  ;; found the first character!
  (when (wordp (jcs-get-current-char-byte))
    (setq jcs-found-first-char t))

  (if (eq jcs-found-first-char nil)
      (progn
        (forward-char 1)
        (jcs-forward-capital-char))
    (progn
      (if (not (wordp (jcs-get-current-char-byte)))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            (backward-char 1))
        (progn
          (if (uppercasep (jcs-get-current-char-byte))
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                )
            (progn
              (forward-char 1)
              (jcs-forward-capital-char)))))))

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
  (let ((temp-cur-char (jcs-get-current-char-string)))
    (jcs-delete-char-repeat temp-cur-char t)))

;;;###autoload
(defun jcs-forward-delete-current-char-repeat ()
  "Delete the current character repeatedly, util it meet the \
character is not the same as current char.  (Forward)"
  (interactive)
  (let ((temp-cur-char (jcs-get-current-char-string)))
    (jcs-delete-char-repeat temp-cur-char nil)))

;;;###autoload
(defun jcs-delete-char-repeat (char reverse)
  "Kill the character repeatedly forward.
CHAR : character to check to delete.
REVERSE : t forward, nil backward."
  (let ((do-kill-char nil))
    (save-excursion
      (if (equal reverse t)
          (backward-char)
        (forward-char))

      (when (jcs-current-char-equal-p char)
        (setq do-kill-char t)))

    (when (equal do-kill-char t)
      (if (equal reverse t)
          (delete-char -1)
        (delete-char 1))
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
    (if (= jcs-search-trigger-backward-char 1)
        (progn
          (setq jcs-search-trigger-backward-char 0)
          (goto-char preserve-point)
          (error "Does not find beginning character : %s" start-char))
      (progn
        ;; Fixed column position.
        (forward-char 1)))
    (setq start-point (point))

    ;; Returns found point.
    start-point))

(defun jcs-find-end-char (end-char preserve-point)
  "Find the ending character."
  (let ((inhibit-message t)
        (end-point nil))
    (jcs-move-to-forward-a-char-do-recursive end-char nil)

    ;; If failed search forward end character..
    (when (= jcs-search-trigger-forward-char 1)
      (setq jcs-search-trigger-forward-char 0)
      (goto-char preserve-point)
      (error "Does not find end character : %s" end-char))
    (setq end-point (point))

    ;; Returns found point.
    end-point))

(defun jcs-check-outside-nested-char-p (start-char end-char)
  "Check if outside the nested START-CHAR and END-CHAR?"
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
              (progn
                (when (jcs-current-char-equal-p start-char)
                  (if same-char-start-flag
                      (progn
                        (setq nested-level (- nested-level 1))
                        (setq same-char-start-flag nil))
                    (progn
                      (setq nested-level (+ nested-level 1))
                      (setq same-char-start-flag t)))))
            (progn
              ;; If is the start char, we add up the nested level.
              (when (jcs-current-char-equal-p start-char)
                (setq nested-level (+ nested-level 1)))

              ;; If is the end char, we minus the nested level.
              (when (jcs-current-char-equal-p end-char)
                (setq nested-level (- nested-level 1)))))

          (forward-char 1))

        ;; If nested level is lower than 0, meaning is not between
        ;; the nested START-CHAR and END-CHAR.
        (<= nested-level 0)))))

(defun jcs-delete-between-char (start-char end-char)
  "Delete everything between START-CHAR and the END-CHAR."
  (let ((preserve-point (point))
        (start-point nil)
        (end-point nil))
    ;; Get start bound.
    (setq start-point (jcs-find-start-char start-char preserve-point))

    ;; NOTE(jenchieh): Back to preserve point before we search.
    (goto-char preserve-point)

    ;; Get end bound.
    (forward-char 1)
    (if (jcs-current-char-equal-p end-char)
        (progn
          (backward-char 1)
          (setq end-point (point)))
      (progn
        (backward-char 1)
        (setq end-point (jcs-find-end-char end-char preserve-point))))

    ;; NOTE(jenchieh): Start to solve the nested character issue.
    (goto-char preserve-point)
    (let ((nested-count 0)
          (break-search-nested nil))
      (ignore-errors
        ;; Solve backward nested.
        (while (equal break-search-nested nil)
          (goto-char start-point)
          (backward-char 1)

          (let ((nested-counter 0))
            (while (<= nested-counter nested-count)
              (jcs-find-end-char end-char preserve-point)
              (setq nested-counter (+ nested-counter 1))))

          (if (not (= end-point (point)))
              (progn
                (setq nested-count (+ nested-count 1))
                (goto-char start-point)
                (backward-char 1)
                (setq start-point (jcs-find-start-char start-char preserve-point)))
            (setq break-search-nested t))))

      ;; IMPORTANT(jenchieh): reset variables.
      (goto-char preserve-point)
      (setq nested-count 0)
      (setq break-search-nested nil)

      (ignore-errors
        ;; Solve forward nested.
        (while (equal break-search-nested nil)
          (goto-char end-point)

          (let ((nested-counter 0))
            (while (<= nested-counter nested-count)
              (jcs-find-start-char start-char preserve-point)
              (setq nested-counter (+ nested-counter 1))))

          (if (not (= start-point (point)))
              (progn
                (setq nested-count (+ nested-count 1))
                (goto-char end-point)
                (setq end-point (jcs-find-end-char end-char preserve-point)))
            (setq break-search-nested t)))))

    ;; Go back to original position before do anything.
    (goto-char preserve-point)

    ;; Check if is inside the region.
    (if (and (>= preserve-point start-point)
             (<= preserve-point end-point)
             (not (jcs-check-outside-nested-char-p start-char end-char)))
        (progn
          ;; Delete the region.
          (delete-region start-point end-point))
      (progn
        ;; Back to where you were.
        (goto-char preserve-point)
        (error "You are not between %s and %s" start-char end-char)))))

;;;###autoload
(defun jcs-delete-inside-paren ()
  "Delete everything inside open parenthesis and close parenthesis."
  (interactive)
  (jcs-delete-between-char "(" ")"))

;;;###autoload
(defun jcs-delete-inside-sqrParen ()
  "Delete everything between open square parenthesis and close square parenthesis."
  (interactive)
  (jcs-delete-between-char "[[]" "]"))

;;;###autoload
(defun jcs-delete-inside-curlyParen ()
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
          ((string= c "}") (setq pair-char "{")))
    pair-char))

(defun jcs-get-close-pair-char (c)
  "Get the close pairing character from C."
  (let ((pair-char nil))
    (cond ((string= c "\"") (setq pair-char "\""))
          ((string= c "'") (setq pair-char "'"))
          ((string= c "(") (setq pair-char ")"))
          ((string= c "[") (setq pair-char "]"))
          ((string= c "{") (setq pair-char "}")))
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

;;;###autoload
(defun jcs-electric-backspace ()
  "Electric backspace key."
  (interactive)
  (let* ((cc (jcs-get-current-char-string))
         (cpc (jcs-get-close-pair-char cc)))
    (backward-delete-char 1)
    (jcs-process-close-pair-char cpc)))


(provide 'jcs-edit)
;;; jcs-edit.el ends here
