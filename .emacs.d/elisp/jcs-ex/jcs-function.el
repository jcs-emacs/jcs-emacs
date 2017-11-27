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
;; Toggle between c and c++ mode.
;;---------------------------------------------
;;;###autoload
(defun jcs-toggle-cc-mode ()
  "Toggle c/c++ mode."
  (interactive)

  (if (equal major-mode 'c-mode)
      (progn
        (c++-mode))
    (progn
      (c-mode)))
  )

;;;###autoload
(defun jcs-is-buffer-open-in-two-or-more-window ()
  "Check if one buffer/file open in two window at a time."
  (interactive)

  (ignore-errors
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))
    (setq current-file-buffer (get-buffer BaseFileNameWithExtension))
    )


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

    ;; return this statement.
    (>= displayed-frame-count 2)
    )
  )

;;;###autoload
(defun jcs-reload-emacs-once ()
  "Reload emacs file one turn."
  (interactive)

  (setq frame-displayed-in-two-window 0)

  (if (jcs-is-buffer-open-in-two-or-more-window)
      (setq frame-displayed-in-two-window 1)
    )

  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))
  (ignore-errors (delete-window))

  (load-file "~/.emacs")

  ;; TODO(jayces): Bug if multiple window displayed.
  (if (= frame-displayed-in-two-window 0)
      (switch-to-previous-buffer)
    )
  )

;;;###autoload
(defun jcs-reload-emacs ()
  "Reload '.emacs' file properly fixed toggle issue."
  (interactive)

  ;; NOTE(jayces): kill window will fixed horizontal split issue.
  ;; Load twice will fixed the toggle issue.

  (jcs-reload-emacs-once)
  (jcs-reload-emacs-once)
  )

;;;###autoload
(defun jcs-top-level ()
  "Teminate the current command. - Canceling Action."
  (interactive)

  (top-level))

;;;
;; TOPIC: Toggle Window Split
;; URL: https://www.emacswiki.org/emacs/ToggleWindowSplit
;;;###autoload
(defun jcs-toggle-window-split ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))


;;----------------------------------------------
;; Sublimity Mode
;;----------------------------------------------
;;;###autoload
(defun jcs-toggle-sublimity-mode ()
  "Toggle sublimity mode and reactive `global-linum-mode'."
  (interactive)

  (call-interactively 'sublimity-mode)
  (global-linum-mode 1)
  )


;;----------------------------------------------
;; wgrep
;;----------------------------------------------
;;;###autoload
(defun jcs-ag-project-regexp ()
  "Use `wgrep' to replace the word in the entire project.

TOPIC: Is there a way to use query-replace from grep/ack/ag output modes?
URL: https://emacs.stackexchange.com/questions/212/is-there-a-way-to-use-query-replace-from-grep-ack-ag-output-modes
"
  (interactive)

  ;; open search result menu.
  (call-interactively 'ag-project-regexp)

  (other-window 1)

  ;; make result menu editable.
  (call-interactively 'wgrep-change-to-wgrep-mode)
  )

;;----------------------------------------------
;; JayCeS Helm
;;----------------------------------------------

;;;###autoload
(defun jcs-helm-before-initialize-hook ()
  "Do the helm mx and change theme"
  (interactive)

  ;; NOTE(jenchieh): Change theme so we know which mode
  ;; we are in visually.
  (jcs-dark-blue-theme)
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



;;; Utilities
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-util.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-window.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-shell.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-trans-window.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-minimap.el")

;;; Editing
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-edit.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-comment.el")

;;; Navigation
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-nav.el")

;;; For Specific Mode
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-oop-func.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-cc-func.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-web-func.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-python-func.el")
(load-file "~/.emacs.d/elisp/jcs-ex/ex-func/jcs-re-builder-func.el")

;;------------------------------------------------------------------------------------------------------
;; This is the end of jcs-function.el file
