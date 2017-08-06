;; This is the start of jcs-shell.el file
;;------------------------------------------------------------------------------------------------------

;; jcs-shell.el             -*- Emacs-Lisp -*-

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


;;===================================
;;      Toggle Shell window
;;---------------------------
;; TAG: shell, terminal

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

    ;; active truncate line as default for shell window.
    (toggle-truncate-lines)
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


;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-shell.el file
