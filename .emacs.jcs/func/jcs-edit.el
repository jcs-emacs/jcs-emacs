;; ========================================================================
;; $File: jcs-edit.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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
        (save-selected-window
          (if (or (ignore-errors (jcs-jump-shown-to-buffer "*undo-tree*")))
              ;; We do found `*undo-tree*' buffer shown in
              ;; one of the window.
              (progn
                (undo-tree-visualize-undo))
            ;; Error occurs
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
                  (undo-tree-visualizer-toggle-diff))
                (global-linum-mode t))))))
    (call-interactively 'undo)))

;;;###autoload
(defun jcs-redo ()
  "Undo key."
  (interactive)
  (if jcs-use-undo-tree-key
      (progn
        (save-selected-window
          (if (or (ignore-errors (jcs-jump-shown-to-buffer "*undo-tree*")))
              ;; We do found `*undo-tree*' buffer shown in
              ;; one of the window.
              (progn
                (undo-tree-visualize-redo))
            ;; Error occurs
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
                  (undo-tree-visualizer-toggle-diff))
                (global-linum-mode t))))))
    ;; In Emacs, undo/redo is the same thing.
    (call-interactively 'redo)))

;;----------------------------------------------
;; Tab
;;----------------------------------------------

;;;###autoload
(defun jcs-tab-key ()
  "TAB key for JayCeS usage."
  (interactive)
  (if (or (jcs-current-char-equal-p " ")
          (jcs-current-char-equal-p "\t")
          (jcs-is-beginning-of-line-p)
          (jcs-is-end-of-line-p))
      (progn
        (jcs-insert-spaces-by-tab-width))
    (progn
      ;; NOTE(jenchieh): Default tab function put here..
      (call-interactively 'dabbrev-expand))))

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

;;;###autoload
(defun duplicate-line ()
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
  "Set it goto the beginning of the buffer from the current \
frame, so it do not goto the beginning of the line."
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (back-to-indentation-or-beginning))

;;;###autoload
(defun jcs-smart-select-end ()
  "TODO(jecnhieh): comment this..."
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
        (call-interactively 'indent-region))
    (progn
      (call-interactively 'jcs-format-document))))

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
  "Revert buffer without confirmation.

SOURCE(jenchieh):
1) http://emacs.stackexchange.com/questions/169/how-do-i-reload-a-file-in-a-buffer
2) http://www.emacswiki.org/emacs-en/download/misc-cmds.el"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;;;###autoload
(defun jcs-other-window-next()
  "Cycle through window and frame. (next window/frame)

SOURCE: http://emacs.stackexchange.com/questions/628/cycle-between-windows-in-all-frames"
  (interactive)

  ;; find nexr window and jump to that window.
  (other-window 1 t)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun jcs-other-window-prev()
  "Cycle through window and frame.(previous window/frame)"
  (interactive)

  ;; find previous window and jump to that window.
  (other-window -1 t)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun scroll-up-one-line()
  "Scroll the text up one line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun scroll-down-one-line()
  "Scroll the text down one line."
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun delete-trailing-whitespace-except-current-line ()
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
;; JenChieh Save Buffer
;;-------------------------

;;;###autoload
(defun jcs-untabify-save-buffer ()
  "Save buffer / Utabify the document / Delete all trailing
whitespaces."
  (interactive)
  (delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer)

  ;; NOTE(jenchieh): Is we found `*undo-tree*' buffer, we
  ;; try to close it.
  (save-selected-window
    (when (or (ignore-errors (jcs-jump-shown-to-buffer "*undo-tree*")))
      (jcs-maybe-kill-this-buffer))))

;;;###autoload
(defun jcs-tabify-save-buffer ()
  " Save buffer / Tabify the document / Delete all trailing
whitespaces. NOTE(JenChieh): Makefile does not support space,
so we must convert spaces to tab."
  (interactive)
  (delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (tabify (point-min) (point-max))))
  (save-buffer)

  ;; NOTE(jenchieh): Is we found `*undo-tree*' buffer, we
  ;; try to close it.
  (save-selected-window
    (when (or (ignore-errors (jcs-jump-shown-to-buffer "*undo-tree*")))
      (jcs-maybe-kill-this-buffer))))

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


;;================================================
;; * Run Program *
;; Setup the run file key and function.
;;----------------------------------------

;;;###autoload
(defun find-project-directory-recursive-run ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p jcs-runscript) t
    (cd "../")
    (find-project-directory-recursive-run)))

;;;###autoload
(defun find-project-directory-run ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked
      (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive-run)
    (setq last-compilation-directory default-directory)))

;;;###autoload
(defun run-without-asking()
  "Run the current build program. - JenChieh"
  (interactive)
  (if (find-project-directory-run)
      (compile jcs-runscript)))

;;================================================
;; * Open TODO file *
;;----------------------------------------

;;;###autoload
(defun open-todo-without-asking()
  "Open the TODO list from this project. - JenChieh"
  (interactive)
  (ffip-find-files jcs-todo-file t))

;;================================================
;; * Open LOG file *
;;----------------------------------------

;;;###autoload
(defun open-update-log-without-asking()
  "Open the Update Log from this project. - JenChieh"
  (interactive)
  (ffip-find-files jcs-update-log-file t))

;;---------------------------------------------
;; Source: https://www.emacswiki.org/emacs/BackToIndentationOrBeginning
;;---------------------------------------------

;;;###autoload
(defun back-to-indentation-or-beginning ()
  "Toggle between first character and beginning of line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

;;;###autoload
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation

If you rather it go to beginning-of-line
first and to indentation on the next hit use
this version instead."
  (interactive)
  (if (bolp)
      (beginning-of-line)
    (back-to-indentation)))

;;;###autoload
(defun jcs-back-to-indentation ()
  "back to identation by checking first character in the line."
  (interactive)
  (beginning-of-line)

  (if (not (jcs-current-line-totally-empty-p))
      (forward-char 1))

  (while (jcs-current-whitespacep)
    (forward-char 1))

  (backward-char 1))


;;----------------------------------------------
;; Rename file.
;;----------------------------------------------

;; SOURCE(jenchieh): https://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name
;; URL: http://www.whattheemacsd.com/
;;;###autoload
(defun jcs-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
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
;; Edit
;;----------------------------------------------

;;;###autoload
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;###autoload
(defun visible-buffers (buffers)
  "given a list of buffers, return buffers which are currently
visible"
  (remove nil
          (mapcar
           '(lambda (buf)
              (if (get-buffer-window-list buf) buf))
           buffers)
          ))

;;;###autoload
(defun not-visible-buffers (buffers)
  "given a list of buffers, return buffers which are not currently
visible"
  (remove nil
          (mapcar
           '(lambda (buf)
              (unless (get-buffer-window-list buf) buf))
           buffers)
          ))

;;;###autoload
(defun buffer-in-window-list ()
  "TOPIC(jenchieh): Show all open buffers in Emacs
SOURCE(jenchieh): http://stackoverflow.com/questions/12186713/show-all-open-buffers-in-emacs
"
  (let (buffers)
    (walk-windows
     (lambda (window)
       (push (window-buffer window) buffers)) t t)
    buffers))

(defun jcs-in-window-list (buf)
  "Check if buffer open in window list.

buf : buffer name. (string)

True: return name.
False: return nil."
  (get-buffer-window-list buf))

;;----------------------------------------------
;; Kill Buffer
;;----------------------------------------------

;;;###autoload
(defun jcs-kill-this-buffer ()
  "Kill this buffer."
  (interactive)

  (kill-this-buffer)

  (save-selected-window
    (ignore-errors
      (jcs-jump-shown-to-window "*Buffer List*"))
    (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
      (jcs-buffer-menu)))

  ;; If still in the buffer menu, try switch to the
  ;; previous buffer
  (when (jcs-is-current-major-mode-p "Buffer-menu-mode")
    (switch-to-previous-buffer)))

;;;###autoload
(defun jcs-maybe-kill-this-buffer ()
  "Kill the buffer if this file is the only file. Otherwise just
switch to the previous buffer.

SOURCE(jenchieh): https://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2915#2915
"
  (interactive)

  (ignore-errors
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))
    (setq current-file-buffer (get-buffer BaseFileNameWithExtension)))

  ;; NOTE(jenchieh): new line in common lisp.
  ;;(terpri)

  (let ((displayed-frame-count 0))
    (dolist (buf (buffer-in-window-list))
      (ignore-errors
        (if (eq buf current-file-buffer)
            ;; increment plus 1
            (setq displayed-frame-count (+ displayed-frame-count 1)))))
    (if (or (>= displayed-frame-count 2)
            ;; NOTE(jenchieh): If you don't want `*Buffer-List*'
            ;; window open in at least two window and get killed
            ;; at the same time. Enable the line under.
            ;;(jcs-is-current-major-mode-p "Buffer-menu-mode")
            )
        (switch-to-previous-buffer)
      (jcs-kill-this-buffer))))

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
