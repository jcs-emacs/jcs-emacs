;; ========================================================================
;; $File: jcs-function.el $
;; $Date: 2016-10-21 10:21:39 $
;; $Revision: $
;; $Creator: Jen-Chieh Shen $
;; $Notice: See LICENSE.txt for modification and distribution information
;;                   Copyright © 2016 by Shen, Jen-Chieh $
;; ========================================================================


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; JenChieh self function defines.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;----------------------------------------------
;; Buffer
;;----------------------------------------------

;;;###autoload
(defun jcs-is-buffer-open-in-two-or-more-window ()
  "Check if one buffer/file open in two window at a time."
  (interactive)

  (ignore-errors
    (setq BaseFileNameWithExtension (file-name-nondirectory buffer-file-name))
    (setq current-file-buffer (get-buffer BaseFileNameWithExtension)))

  (let ((displayed-frame-count 0))
    (dolist (buf  (buffer-in-window-list))
      (ignore-errors
        (if (eq buf current-file-buffer)
            ;; increment plus 1
            (setq displayed-frame-count (+ displayed-frame-count 1))
          )))

    ;; return this statement.
    (>= displayed-frame-count 2)))

;;;###autoload
(defun jcs-reload-emacs-once ()
  "Reload emacs file once."
  (interactive)
  (save-match-data
    (save-window-excursion
      (save-selected-window
        (save-restriction
          (load-file "~/.emacs"))))))

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
  (global-linum-mode 1))

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
  (call-interactively 'wgrep-change-to-wgrep-mode))

;;----------------------------------------------
;; Helm
;;----------------------------------------------

;;;###autoload
(defun jcs-helm-before-initialize-hook ()
  "Do the helm mx and change theme"
  (interactive)

  ;; NOTE(jenchieh): Change theme so we know which mode
  ;; we are in visually.
  (jcs-dark-blue-theme))

;;;###autoload
(defun jcs-helm-gtags-to-def-dec ()
  "Goto the declaration / definition depends on the cursor position."
  (interactive)

  (ignore-errors
    ;; Update TAG file. Default is update only current file, You
    ;; can update all files with C-u prefix.
    (helm-gtags-update-tags)

    ;; goto definition or declaration.
    (helm-gtags-find-tag-from-here))

  (jcs-reload-active-mode))

;;;###autoload
(defun jcs-helm-gtags-to-def-dec-other-window ()
  "Goto the declaration / definition depends on the cursor position,
in other window."
  (interactive)

  (ignore-errors
    ;; Update TAG file. Default is update only current file, You
    ;; can update all files with C-u prefix.
    (helm-gtags-update-tags)

    ;; NOTE(jenchieh): this will make it jump to next window.
    ;; Is stupid, but work.
    (ignore-errors (helm-gtags-find-tag-other-window nil))

    ;; goto definition or declaration.
    (helm-gtags-find-tag-from-here))

  (jcs-reload-active-mode))

;;;###autoload
(defun jcs-helm-find-files ()
  "Find the file with Helm"
  (interactive)

  (put 'jcs-helm-execute-persistent-action 'state nil)

  (helm-find-files nil))

;;;###autoload
(defun jcs-helm-find-files-other-window ()
  "Find the file with Helm and open another window."
  (interactive)

  ;; set the flag, so when next time run 'jcs-helm-execute-
  ;; persistent-action', he will know what to do instead of
  ;; normal 'helm-execute-persistent-action' action.
  (put 'jcs-helm-execute-persistent-action 'state t)

  (helm-find-files nil))

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
    ))

;;---------------------------------------------
;; Truncate Lines
;;---------------------------------------------

;;;###autoload
(defun jcs-enable-truncate-lines ()
  "Enable truncate lines."
  (interactive)
  (setq truncate-lines t))

;;;###autoload
(defun jcs-disable-truncate-lines ()
  "Disable truncate lines."
  (interactive)
  (setq truncate-lines nil))

;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Load files.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Utilities
(load-file "~/.emacs.jcs/func/jcs-util.el")
(load-file "~/.emacs.jcs/func/jcs-window.el")
(load-file "~/.emacs.jcs/func/jcs-shell.el")
(load-file "~/.emacs.jcs/func/jcs-trans-window.el")
(load-file "~/.emacs.jcs/func/jcs-minimap.el")

;;; Editing
(load-file "~/.emacs.jcs/func/jcs-buffer-menu.el")
(load-file "~/.emacs.jcs/func/jcs-edit.el")
(load-file "~/.emacs.jcs/func/jcs-comment.el")

;;; Navigation
(load-file "~/.emacs.jcs/func/jcs-nav.el")

;;; For Specific Mode
(load-file "~/.emacs.jcs/func/jcs-txt-func.el")
(load-file "~/.emacs.jcs/func/jcs-preproc-func.el")
(load-file "~/.emacs.jcs/func/jcs-oop-func.el")
(load-file "~/.emacs.jcs/func/jcs-cc-func.el")
(load-file "~/.emacs.jcs/func/jcs-cs-func.el")
(load-file "~/.emacs.jcs/func/jcs-cmake-func.el")
(load-file "~/.emacs.jcs/func/jcs-java-func.el")
(load-file "~/.emacs.jcs/func/jcs-lua-func.el")
(load-file "~/.emacs.jcs/func/jcs-nasm-func.el")
(load-file "~/.emacs.jcs/func/jcs-python-func.el")
(load-file "~/.emacs.jcs/func/jcs-web-func.el")
(load-file "~/.emacs.jcs/func/jcs-re-builder-func.el")
