;; This is the start of jcs-edit.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-edit.el             -*- Emacs-Lisp -*-

;; Mode for editing JayCeS code

;; Created:    <Fri Oct 21 13:51:49 EST 2016>
;; Time-stamp: <2016-10-21 10:21:39>
;; Author:     Jen-Chieh Shen <jcs090218@gmail.com>
;; Version:    0.1
;; Keywords:   JayCeS, languages, os, operating system

;; Copyright (C) 2016 Jen-Chieh Shen

;; jcs-function is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jcs-function is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; When editing the file.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


;;---------------------------------------------
;;-- Source --
;;      Deletion: http://ergoemacs.org/emacs/emacs_kill-ring.html
;;---------------------------------------------
;;;###autoload
(defun jcs-kill-whole-line ()
  "Deletes a line, but does not put it in the kill-ring. (kinda)"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (setq kill-ring (cdr kill-ring)))

;;---------------------------------------------
;;
;;---------------------------------------------
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

;;---------------------------------------------
;;
;;---------------------------------------------
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
(defun duplicate-line ()
  "Duplicate the line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;;;###autoload
(defun jcs-previous-line ()
  "Calling `previous-line' does not execute. Just use this without
remember Emacs Lisp function."
  (interactive)
  (previous-line 1))

;;;###autoload
(defun jcs-next-line ()
  "Calling `next-line' does not execute. Just use this without
remember Emacs Lisp function."
  (interactive)
  (next-line 1))

;;---------------------------------------------
;; After moving UP one line, do identation.
;;---------------------------------------------
;;;###autoload
(defun jcs-smart-indent-up ()
  ""
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name)
           ;;(nth 4 (syntax-ppss))    ;; check if whole line in comment
           )
      (progn
        (previous-line 1)
        (indent-for-tab-command)
        )
    ;; else
    (previous-line 1))
  )

;;;###autoload
(defun jcs-smart-indent-up-by-mode ()
  ""
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name)
           ;;(nth 4 (syntax-ppss))    ;; check if whole line in comment
           )
      (progn
        (previous-line 1)
        (indent-according-to-mode)
        )
    ;; else
    (previous-line 1))
  )


;;---------------------------------------------
;; After moving DOWN one line, do identation.
;;---------------------------------------------
;;;###autoload
(defun jcs-smart-indent-down ()
  ""
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name)
           ;;(nth 4 (syntax-ppss))    ;; check if whole line in comment
           )
      (progn
        (next-line 1)
        (indent-for-tab-command)
        )
    ;; else
    (next-line 1))
  )

;;;###autoload
(defun jcs-smart-indent-down-by-mode ()
  ""
  (interactive)
  (if (and (not mark-active)
           (buffer-file-name)
           ;;(nth 4 (syntax-ppss))    ;; check if whole line in comment
           )
      (progn
        (next-line 1)
        (indent-according-to-mode)
        )
    ;; else
    (next-line 1))
  )

;;---------------------------------------------
;; Set it goto the beginning of the buffer from
;; the current frame, so it do not goto the
;; beginning of the line.
;;---------------------------------------------
;;;###autoload
(defun jcs-smart-select-home ()
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (back-to-indentation-or-beginning)
  )

;;---------------------------------------------
;;
;;---------------------------------------------
;;;###autoload
(defun jcs-smart-select-end ()
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (end-of-line)
  )


;;========================================
;;      JCS Format File
;;----------------------------------

;;;###autoload
(defun jcs-format-document ()
  "Format current document."
  (interactive)

  ;; indent the whole doc.
  (indent-region (point-min) (point-max))
  )

;;;###autoload
(defun jcs-format-region-or-document ()
  "Format the document if there are no region apply."
  (interactive)

  (if (use-region-p)
      (progn
        (call-interactively 'indent-region))
    (progn
      (call-interactively 'jcs-format-document))
    )
  )

;;;###autoload
(defun jcs-align-document ()
  "Align current document."
  (interactive)

  ;; align the whole doc.
  (align (point-min) (point-max))
  )

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
  (select-frame-set-input-focus (selected-frame))
  )

;;;###autoload
(defun jcs-other-window-prev()
  "Cycle through window and frame.(previous window/frame)"
  (interactive)

  ;; find previous window and jump to that window.
  (other-window -1 t)
  (select-frame-set-input-focus (selected-frame))
  )

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


;;---------------------------------------------
;; Delete the trailing whitespace for whole doc-
;; ument execpt the current line.
;;---------------------------------------------
(defun delete-trailing-whitespace-except-current-line ()
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


;;=================================
;; JenChieh Save Buffer
;;-------------------------

;; autoload
(defun jcs-save-buffer ()
  "Save buffer / Utabify the document / Delete all trailing
whitespaces."
  (interactive)
  (delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

;; autoload
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
  (save-buffer))

;; autoload
(defun jcs-find-file-other-window ()
  "This will allow us open the same file in another window."
  (interactive)
  (if (buffer-file-name)
      (progn
        (find-file-other-window buffer-file-name)
        (jcs-other-window-prev)
        )
    )
  )

;; autoload
(defun jcs-smart-find-file-in-project-in-another-window ()
  "This will open the file in another window using 'find-file-
project.el' plugin."
  (interactive)

  (if (ignore-errors (find-file-in-project t))
      (progn
        ;; Reach here mean success using 'find-file-in-
        ;; project.el' plugin.
        )
    (ido-find-file-other-window))
  )

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
    (ido-find-file))
  )


;;================================================
;; * Run Program *
;; Setup the run file key and function.
;;----------------------------------------

;;---------------------------------------------
;; Find the run file base on the operating
;; system u r on.
;;---------------------------------------------
(defun find-project-directory-recursive-run ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p jenchieh-runscript) t
    (cd "../")
    (find-project-directory-recursive-run)))

;;---------------------------------------------
;; Find the directory in order.
;;---------------------------------------------
(defun find-project-directory-run ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive-run)
    (setq last-compilation-directory default-directory)))

;;---------------------------------------------
;; Main function call by running the program.
;;---------------------------------------------
(defun run-without-asking()
  "Run the current build program. - JenChieh"
  (interactive)
  (if (find-project-directory-run) (compile jenchieh-runscript))
  (other-window 1))


;;---------------------------------------------
;; Source:
;; -> https://www.emacswiki.org/emacs/BackToIndentationOrBeginning
;;---------------------------------------------
(defun back-to-indentation-or-beginning ()
  "Toggle between first character and beginning of line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
;;---------------------------------------------
;; If you rather it go to beginning-of-line
;; first and to indentation on the next hit use
;; this version instead.
;;---------------------------------------------
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (beginning-of-line)
    (back-to-indentation)))

(defun jcs-back-to-indentation ()
  "back to identation by checking first character in the line."
  (interactive)
  (beginning-of-line)

  (if (not (current-line-totally-empty-p))
      (forward-char 1))

  (while (current-whitespacep)
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


;;;###autoload
(defun jcs-maybe-kill-this-buffer ()
  "Kill the buffer if this file is the only file. Otherwise just
switch to the previous buffer.

SOURCE(jenchieh): https://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2915#2915
"
  (interactive)

  (ignore-errors
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))
    (setq current-file-buffer (get-buffer BaseFileNameWithExtension))
    )

  ;; NOTE(jenchieh): new line in common lisp.
  ;;(terpri)

  (let (
        (displayed-frame-count 0)
        )
    (dolist (buf  (buffer-in-window-list))
      (ignore-errors
        (if (eq buf current-file-buffer)
            ;; increment plus 1
            (setq displayed-frame-count (+ displayed-frame-count 1))
          )
        )
      )

    (if (>= displayed-frame-count 2)
        (switch-to-previous-buffer)
      (kill-this-buffer)
      )
    )
  )

;;----------------------------------------------
;; Search/Kill word capital.
;;----------------------------------------------

;; check the first character a character.
(setq first-char-is-char nil)

;; boolean to check the first character
(setq check-first-char nil)

;; found the first character in search?
(setq found-first-char nil)

;;;###autoload
(defun jcs-backward-kill-word-capital ()
  "Backward delete the word unitl the word is capital."
  (interactive)

  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))

  (if (eq check-first-char nil)
      (progn
        ;; check the first character a character
        (if (wordp current-char-char)
            (setq first-char-is-char t))

        ;; check the first character mission complete.
        (setq check-first-char t)
        )
    )

  ;; found the first character!
  (if (wordp current-char-char)
      (setq found-first-char t))

  (if (eq found-first-char nil)
      (progn
        (if (eq first-char-is-char t)
            (progn
              (backward-delete-char 1))
          (progn
            (backward-delete-char 1)
            (jcs-backward-kill-word-capital)
            ))

        )
    (progn
      (if (not (wordp current-char-char))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            )
        (progn
          (if (uppercasep current-char-char)
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                (backward-delete-char 1))
            (progn
              (backward-delete-char 1)
              (jcs-backward-kill-word-capital)))
          )
        )
      )
    )

  ;; reset triggers
  (setq first-char-is-char nil)
  (setq check-first-char nil)
  (setq found-first-char nil)
  )

;;;###autoload
(defun jcs-forward-kill-word-capital ()
  "Forward delete the word unitl the word is capital."
  (interactive)

  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))

  (if (eq check-first-char nil)
      (progn
        ;; check the first character a character
        (if (wordp current-char-char)
            (progn
              (forward-char 1)
              (setq first-char-is-char t)))

        ;; check the first character mission complete.
        (setq check-first-char t)
        )
    )

  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))

  ;; found the first character!
  (if (wordp current-char-char)
      (setq found-first-char t))

  (if (eq found-first-char nil)
      (progn
        (delete-forward-char 1)
        (jcs-forward-kill-word-capital)
        )

    (progn
      (if (not (wordp current-char-char))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            (delete-backward-char 1)
            )
        (progn
          (if (uppercasep current-char-char)
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                )
            (progn
              (delete-forward-char 1)
              (jcs-forward-kill-word-capital)
              ))))))

  ;; reset triggers
  (setq first-char-is-char nil)
  (setq check-first-char nil)
  (setq found-first-char nil)
  )

;;;###autoload
(defun jcs-backward-capital-char ()
  "Backward search capital character and set the cursor
to the point."
  (interactive)

  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))

  (if (eq check-first-char nil)
      (progn
        ;; check the first character a character
        (if (wordp current-char-char)
            (setq first-char-is-char t))

        ;; check the first character mission complete.
        (setq check-first-char t)
        )
    )

  ;; found the first character!
  (if (wordp current-char-char)
      (setq found-first-char t))

  (if (eq found-first-char nil)
      (progn
        (if (eq first-char-is-char t)
            (progn
              (backward-char 1))
          (progn
            (backward-char 1)
            (jcs-backward-capital-char)
            ))

        )
    (progn
      (if (not (wordp current-char-char))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            )
        (progn
          (if (uppercasep current-char-char)
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                (backward-char 1))
            (progn
              (backward-char 1)
              (jcs-backward-capital-char)))
          )
        )
      )
    )

  ;; reset triggers
  (setq first-char-is-char nil)
  (setq check-first-char nil)
  (setq found-first-char nil)
  )

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
  (if (= beginningBufferPoint (point))
      (forward-char 1))

  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))

  (if (eq check-first-char nil)
      (progn
        ;; check the first character a character
        (if (wordp current-char-char)
            (progn
              (forward-char 1)
              (setq first-char-is-char t)))

        ;; check the first character mission complete.
        (setq check-first-char t)
        ))

  (setq current-char (char-before))
  (setq current-char-string (string current-char))
  (setq current-char-char (string-to-char current-char-string))

  ;; found the first character!
  (if (wordp current-char-char)
      (setq found-first-char t))

  (if (eq found-first-char nil)
      (progn
        (forward-char 1)
        (jcs-forward-capital-char))

    (progn
      (if (not (wordp current-char-char))
          (progn
            ;; NOTE: Here is end of the recursive
            ;; function loop...
            (backward-char 1)
            )
        (progn
          (if (uppercasep current-char-char)
              (progn
                ;; NOTE: Here is end of the recursive
                ;; function loop...
                )
            (progn
              (forward-char 1)
              (jcs-forward-capital-char)
              ))))))

  ;; reset triggers
  (setq first-char-is-char nil)
  (setq check-first-char nil)
  (setq found-first-char nil)
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-edit.el file
