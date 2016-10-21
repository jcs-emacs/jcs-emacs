;; This is the start of jcs-function.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-function.el             -*- Emacs-Lisp -*-

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
;; JenChieh self function defines.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;---------------------------------------------
;; JenChieh Window Split Setting for Dual Monitor
;;---------------------------------------------
(defun jcs-new-window()
  "Setup dual monitor."
  (interactive)

  ;; open a new frame
  (make-frame-command)
  )

;;---------------------------------------------
;; Create a new frame and
;;---------------------------------------------
;; @param frame: frame we just created.
;;---------------------------------------------
(defun jcs-aftermake-frame-functions-hook(frame)
  "Resetting the new frame just created."
  (interactive)

  (select-frame frame)

  ;; split the winodw after create the new window
  (split-window-horizontally)
  )
(add-hook 'after-make-frame-functions 'jcs-aftermake-frame-functions-hook)

;;---------------------------------------------
;;-- Source --
;;      Deletion: http://ergoemacs.org/emacs/emacs_kill-ring.html
;;---------------------------------------------
(defun my-delete-line()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

;;---------------------------------------------
;;
;;---------------------------------------------
(defun my-delete-word (arg)
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
(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun duplicate-line ()
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

;;---------------------------------------------
;; After moving DOWN one line, do identation.
;;---------------------------------------------
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

;; jcs smart select

;;---------------------------------------------
;; Set it goto the beginning of the buffer from
;; the current frame, so it do not goto the
;; beginning of the line.
;;---------------------------------------------
(defun jcs-smart-select-home ()
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (back-to-indentation-or-beginning)
  )

;;---------------------------------------------
;;
;;---------------------------------------------
(defun jcs-smart-select-end ()
  (interactive)
  (if (not mark-active)
      (push-mark nil nil 1))
  (end-of-line)
  )


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

;;---------------------------------------------
;; Save buffer / Utabify the document / Delete
;; all trailing whitespaces.
;;---------------------------------------------
(defun jcs-save-buffer ()
  "Save the buffer after untabifying it. JenChieh"
  (interactive)
  (delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

;;---------------------------------------------
;; Save buffer / Tabify the document / Delete
;; all trailing whitespaces.
;;
;; NOTE(JenChieh): Makefile does not support
;; space, so we must convert spaces to tab.
;;---------------------------------------------
(defun jcs-tabify-save-buffer ()
  "Save the buffer after tabifying it. JenChieh"
  (interactive)
  (delete-trailing-whitespace-except-current-line)
  (save-excursion
    (save-restriction
      (widen)
      (tabify (point-min) (point-max))))
  (save-buffer))


;;---------------------------------------------
;; This will allow us open the same file
;; in another window!
;;---------------------------------------------
(defun jcs-find-file-other-window ()
  "Find the file that corresponds to this one. JenChieh"
  (interactive)
  (if (buffer-file-name)
      (progn
        (find-file-other-window buffer-file-name)
        (jcs-other-window-prev)
        )
    )
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


;;---------------------------------------------
;; Make the time stamp base on the format
;; provided.
;;
;; Source:
;; -> https://www.emacswiki.org/emacs/InsertingTodaysDate
;;---------------------------------------------
(defun jcs-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;;---------------------------------------------
;; Make the data base on the format provided.
;;---------------------------------------------
(defun jcs-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;---------------------------------------------
;; Make the time base on the format provided.
;;---------------------------------------------
(defun jcs-time()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))


;;===================================
;;      Toggle Shell window
;;---------------------------

;;---------------------------------------------
;; Main function call toggling the shell
;; window.
;;---------------------------------------------
(defun jcs-toggle-shell-window ()
  "Toggle Shell Command prompt."
  (interactive)

  ;; local variable trigger/boolean
  ;; TOGGLE SOURCE: http://ergoemacs.org/emacs/elisp_toggle_command.html
  (if (get 'jcs-toggle-shell-window 'state)
      (progn
        (jcs-hide-shell-window)
        (put 'jcs-toggle-shell-window 'state nil))
    (progn
      (jcs-show-shell-window)
      (put 'jcs-toggle-shell-window 'state t)))
  )

;;---------------------------------------------
;; Show the shell window.
;;---------------------------------------------
(defun jcs-show-shell-window()
  "Shell Command prompt."
  (interactive)

  (when (= (length (window-list)) 2)
    (split-window-below)
    (switch-to-buffer-other-window "*shell*")
    (shell)
    )
  )

;;---------------------------------------------
;; Hide the shell window.
;;---------------------------------------------
(defun jcs-hide-shell-window ()
  "Kill process prompt."
  (interactive)

  ;; kill the process before closing the frame.
  (if (get-buffer-process "*shell*")
      (progn
        (kill-process)
        (erase-buffer)
        )
    )

  ;; kill the frame.
  (delete-window)
  )

;;========================================
;;      JCS Format File
;;----------------------------------

;;---------------------------------------------
;; Re-format the whole document.
;;---------------------------------------------
(defun jcs-format-document ()
  "Format current document."
  (interactive)

  ;; indent the whole doc.
  (indent-region (point-min) (point-max))
  )


;;---------------------------------------------
;; SOURCE(jenchieh):
;; 1) http://emacs.stackexchange.com/questions/169/how-do-i-reload-a-file-in-a-buffer
;; 2) http://www.emacswiki.org/emacs-en/download/misc-cmds.el
;;---------------------------------------------
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;;---------------------------------------------
;; Cycle through window and frame. (next
;; window/frame)
;;
;; SOURCE: http://emacs.stackexchange.com/questions/628/cycle-between-windows-in-all-frames
;;---------------------------------------------
(defun jcs-other-window-next()
  ""
  (interactive)

  ;; find nexr window and jump to that window.
  (other-window 1 t)
  (select-frame-set-input-focus (selected-frame))
  )

;;---------------------------------------------
;; Cycle through window and frame.(previous
;; window/frame)
;;---------------------------------------------
(defun jcs-other-window-prev()
  ""
  (interactive)

  ;; find previous window and jump to that window.
  (other-window -1 t)
  (select-frame-set-input-focus (selected-frame))
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-function.el file
