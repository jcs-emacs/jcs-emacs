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
(defun jcs-aftermake-frame-functions-hook (frame)
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
(defun jcs-kill-whole-line ()
  "Deletes a line, but does not put it in the kill-ring. (kinda)"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (setq kill-ring (cdr kill-ring)))

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
;; Insert year only.
;;---------------------------------------------
(defun jcs-year-only ()
  (interactive)
  (insert (format-time-string "%Y")))

;;---------------------------------------------
;; Make the time base on the format provided.
;;---------------------------------------------
(defun jcs-time()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

;;===================================
;;      Toggle C/C++ mode
;;---------------------------

;;---------------------------------------------
;; Toggle between c and c++ mode.
;;---------------------------------------------
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)

  (if (equal major-mode 'c-mode)
      (progn
        (c++-mode))
    (progn
      (c-mode)))
  )

;;===================================
;;      Toggle Shell window
;;---------------------------

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun jcs-format-document ()
  "Format current document."
  (interactive)

  ;; indent the whole doc.
  (indent-region (point-min) (point-max))
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

;;;###autoload
(defun jcs-smart-context-line-break ()
  "Comment block."
  (interactive)

  ;; check if inside the comment block.
  (if (nth 4 (syntax-ppss))
      (progn

        (if (looking-back "/\\*\\s-*.*")
            (progn

              ;; ----------------------------------------
              ;; Old version
              ;; --------------------------
              ;; (insert "\n* ")
              ;; (indent-for-tab-command)

              ;; (insert "\n*/")
              ;; (indent-for-tab-command)

              ;; ;; back one line up
              ;; (previous-line 1)
              ;; ----------------------------------------


              ;; ----------------------------------------
              ;; New version
              ;; --------------------------
              (if (looking-back "/* ")
                  (progn
                    (backward-char)
                    (insert "*")
                    (insert "\n* ")
                    (indent-for-tab-command)

                    (insert "\n")
                    (indent-for-tab-command)

                    ;; back one line up
                    (previous-line 1)

                    ;; goto the end of line
                    (end-of-line)
                    )
                (progn
                  (insert "\n* ")
                  (indent-for-tab-command)

                  (insert "\n")
                  (indent-for-tab-command)

                  ;; back one line up
                  (previous-line 1)

                  ;; goto the end of line
                  (end-of-line)
                  )
                )
              ;; ----------------------------------------
              )
          (progn
            (insert "\n")

            (if (nth 4 (syntax-ppss))
                (progn
                  (insert "* ")
                  (indent-for-tab-command)
                  )
              )
            )
          )
        )
    ;; else insert new line
    (progn
      (newline-and-indent)
      ))
  )

;;;###autoload
(defun jcs-c-comment-pair ()
  "Auto pair c style comment block"
  (interactive)

  (insert "*")

  (if (nth 4 (syntax-ppss))

      (if (looking-back "/*")
          (progn
            (insert "  */")

            ;; backward char until the center
            ;; /* _ */   <- fall here.
            (backward-char)
            (backward-char)
            (backward-char)
            )
        )
    )
  )

;;;###autoload
(defun jcs-top-level ()
  "Teminate the current command. - Canceling Action."
  (interactive)

  ;; white
  (jcs-command-mode)

  (top-level)
  )

;;----------------------------------------------
;; JayCeS Helm
;;----------------------------------------------

;;;###autoload
(defun jcs-helm-before-initialize-hook ()
  "Do the helm mx and change theme"
  (interactive)

  ;; green
  (jcs-insert-mode)
  )

;;;###autoload
(defun jcs-helm-gtags-to-def-dec ()
  "Goto the declaration / definition depends on the cursor position."
  (interactive)

  ;; Update TAG file. Default is update only current file, You
  ;; can update all files with C-u prefix.
  (helm-gtags-update-tags)

  ;; goto definition or declaration.
  (helm-gtags-find-tag-from-here)
  )

;;;###autoload
(defun jcs-helm-gtags-to-def-dec-other-window ()
  "Goto the declaration / definition depends on the cursor position,
in other window."
  (interactive)

  ;; Update TAG file. Default is update only current file, You
  ;; can update all files with C-u prefix.
  (helm-gtags-update-tags)

  ;; NOTE(jenchieh): this will make it jump to next window.
  ;; Is stupid, but work.
  (ignore-errors (helm-gtags-find-tag-other-window nil))

  ;; goto definition or declaration.
  (helm-gtags-find-tag-from-here)
  )

;;;###autoload
(defun jcs-helm-find-files ()
  "Find the file with Helm"
  (interactive)

  (put 'jcs-helm-execute-persistent-action 'state nil)

  (helm-find-files nil)
  )

;;;###autoload
(defun jcs-helm-find-files-other-window ()
  "Find the file with Helm and open another window."
  (interactive)

  ;; set the flag, so when next time run 'jcs-helm-execute-
  ;; persistent-action', he will know what to do instead of
  ;; normal 'helm-execute-persistent-action' action.
  (put 'jcs-helm-execute-persistent-action 'state t)

  (helm-find-files nil)
  )

;;;###autoload
(defun jcs-helm-execute-persistent-action ()
  "Rewrap 'helm-execute-presistent-action' function to my
own preferences."
  (interactive)

  (if (get 'jcs-helm-execute-persistent-action 'state)
      (progn
        ;; switch the buffer to another window
        (helm-ff-run-switch-other-window)
        (put 'jcs-helm-execute-persistent-action 'state nil)
        )
    ;; NOTE(jenchieh): no longer needed.
    ;;(helm-execute-persistent-action)
    )
  )

;;;###autoload
(defun toggle-comment-on-line ()
  "comment or uncomment current line.
SOURCE: http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun jcs-comment-uncomment-region-or-line ()
  "Comment line or region, if there are region select then just
comment region. Otherwise comment line."
  (interactive)

  ;; check if there are region select
  (if (and mark-active
           (/= (point) (mark)))
      (progn
        (if (nth 4 (syntax-ppss))
            (progn
              (uncomment-region (region-beginning) (region-end))
              )
          (comment-region (region-beginning) (region-end))
          )
        )
    ;; else we just comment on single line.
    (toggle-comment-on-line)
    )
  )

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-function.el file
