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
    (dolist (buf  (jcs-buffer-in-window-list))
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
          (load-file "~/.emacs")))))

  ;; Split window horizontally if full width.
  (when (and (window-full-width-p)
             (= (length (window-list)) 1))
    (jcs-balance-split-window-horizontally))

  ;; Restore to what ever state it was.
  ;;
  ;; NOTE(jenchieh): we need these two lines because we need it
  ;; for solving after reloading Emacs, there are some space at
  ;; the bottom. Which is weird and I have no idea why...
  (toggle-frame-maximized)
  (toggle-frame-maximized)

  ;; When frame not maximize we make sure it maximized.
  (unless (jcs-is-frame-maximize-p)
    (toggle-frame-maximized)))

;;;###autoload
(defun jcs-top-level ()
  "Teminate the current command. - Canceling Action."
  (interactive)
  (top-level))

;;----------------------------------------------
;; Sublimity Mode
;;----------------------------------------------

;;;###autoload
(defun jcs-toggle-sublimity-mode ()
  "Toggle sublimity mode and reactive `global-linum-mode'."
  (interactive)

  (call-interactively #'sublimity-mode)
  (global-linum-mode 1))

;;----------------------------------------------
;; wgrep
;;----------------------------------------------

;;;###autoload
(defun jcs-ag-project-regexp ()
  "Use `wgrep' to replace the word in the entire project."
  ;; TOPIC: Is there a way to use query-replace from grep/ack/ag output modes?
  ;; URL: https://emacs.stackexchange.com/questions/212/is-there-a-way-to-use-query-replace-from-grep-ack-ag-output-modes
  (interactive)

  ;; open search result menu.
  (call-interactively #'ag-project-regexp)

  (other-window 1)

  ;; make result menu editable.
  (call-interactively #'wgrep-change-to-wgrep-mode))

;;---------------------------------------------
;; Truncate Lines
;;---------------------------------------------

;;;###autoload
(defun jcs-enable-truncate-lines ()
  "Enable truncate lines."
  (interactive)
  (when (equal truncate-lines nil)
    (toggle-truncate-lines)))

;;;###autoload
(defun jcs-disable-truncate-lines ()
  "Disable truncate lines."
  (interactive)
  (when (equal truncate-lines t)
    (toggle-truncate-lines)))

;;---------------------------------------------
;; Text Scale
;;---------------------------------------------

;;;###autoload
(defun jcs-text-scale-increase ()
  "Scale the text up."
  (interactive)
  (call-interactively #'text-scale-increase)

  ;; Renable `linum-mode'.
  (call-interactively #'linum-mode)
  (call-interactively #'linum-mode))

;;;###autoload
(defun jcs-text-scale-decrease ()
  "Scale the text down."
  (interactive)
  (call-interactively #'text-scale-decrease)

  ;; Renable `linum-mode'.
  (call-interactively #'linum-mode)
  (call-interactively #'linum-mode))

;;---------------------------------------------
;; Return
;;---------------------------------------------

;;;###autoload
(defun jcs-ctrl-return-key ()
  "JayCeS default return key."
  (interactive)
  ;;;
  ;; Priority
  ;;
  ;; ATTENTION(jenchieh): all the function in the priority
  ;; function list must all have error handling. Or else this
  ;; the priority chain will break.
  ;;
  ;; 1. `project-abbrev-complete-word'
  ;; 2. `goto-address-at-point'
  ;;
  (unless (or (ignore-errors (call-interactively #'project-abbrev-complete-word)))
    (call-interactively #'goto-address-at-point)))


;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Load files.
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; Utilities
(load-file "~/.emacs.jcs/func/jcs-math.el")
(load-file "~/.emacs.jcs/func/jcs-util.el")
(load-file "~/.emacs.jcs/func/jcs-frame.el")
(load-file "~/.emacs.jcs/func/jcs-window.el")
(load-file "~/.emacs.jcs/func/jcs-shell.el")
(load-file "~/.emacs.jcs/func/jcs-minimap.el")
(load-file "~/.emacs.jcs/func/jcs-helm-func.el")

;;; Editing
(load-file "~/.emacs.jcs/func/jcs-buffer-menu.el")
(load-file "~/.emacs.jcs/func/jcs-edit.el")
(load-file "~/.emacs.jcs/func/jcs-comment.el")
(load-file "~/.emacs.jcs/func/jcs-vs-func.el")

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
(load-file "~/.emacs.jcs/func/jcs-sh-func.el")
(load-file "~/.emacs.jcs/func/jcs-web-func.el")
(load-file "~/.emacs.jcs/func/jcs-re-builder-func.el")
