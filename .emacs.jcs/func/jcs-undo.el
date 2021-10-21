;;; jcs-undo.el --- Undo/Redo module  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'undo-tree)

;;
;; NOTE: This is compatible with other text editor or IDE. Most IDE/text
;; editor have this undo/redo system as default.
;;
(defvar jcs-use-undo-tree-key t
  "Using the undo tree key in stead of normal Emacs's undo key.
This variable must be use with `jcs-undo' and `jcs-redo' functions.")

;; NOTE: Active this will cause huge amount of performance, consider this
;; before active.
(defvar jcs-undo-tree-auto-show-diff nil
  "Show the difference code when undo tree minor mode is active.")

(defvar jcs--undo-splits-windows nil
  "Flag to check if the window splits.")

(defun jcs-toggle-undo-tree-auto-show-diff ()
  "Toggle auto show diff functionality."
  (interactive)
  (if jcs-undo-tree-auto-show-diff
      (jcs-disable-undo-tree-auto-show-diff)
    (jcs-enable-undo-tree-auto-show-diff)))

(defun jcs-enable-undo-tree-auto-show-diff ()
  "Enable undo tree auto show diff effect."
  (interactive)
  (setq jcs-undo-tree-auto-show-diff t)
  (message "Enable undo tree auto show diff"))

(defun jcs-disable-undo-tree-auto-show-diff ()
  "Disable undo tree auto show diff effect."
  (interactive)
  (setq jcs-undo-tree-auto-show-diff nil)
  (message "Disable undo tree auto show diff"))


(defun jcs-toggle-undo-tree-key()
  "Toggle `jcs-use-undo-tree-key' boolean."
  (interactive)
  (if jcs-use-undo-tree-key (jcs-disable-undo-tree-key) (jcs-enable-undo-tree-key)))

(defun jcs-enable-undo-tree-key ()
  "Enable undo tree key.
This will replace usual Emacs' undo key."
  (interactive)
  (setq jcs-use-undo-tree-key t)
  (message "Enable undo tree key"))

(defun jcs-disable-undo-tree-key ()
  "Disable undo tree key.
This will no longer overwrite usual Emacs' undo key."
  (interactive)
  (setq jcs-use-undo-tree-key nil)
  (message "Disable undo tree key"))


(defun jcs--undo-tree-visualizer-quit--advice-after (&rest _)
  "Advice execute after `undo-tree-visualizer-quit' function."
  (when jcs--undo-splits-windows
    (delete-window)
    (setq jcs--undo-splits-windows nil)
    (switch-to-buffer undo-tree-visualizer-parent-buffer)))
(advice-add 'undo-tree-visualizer-quit :after #'jcs--undo-tree-visualizer-quit--advice-after)

(defun jcs-undo-kill-this-buffer ()
  "Kill the undo tree buffer."
  (interactive)
  (jcs-safe-jump-shown-to-buffer
   undo-tree-visualizer-buffer-name
   :type 'strict
   :success (lambda () (bury-buffer))))

(defun jcs-undo-tree-visualize ()
  "Call `undo-tree-visualize' only in window that has higher height."
  (let ((win-len (jcs-count-windows)) (current-window (selected-window))
        target-window)
    (when (< win-len 2)
      (jcs-balance-split-window-horizontally)
      (setq jcs--undo-splits-windows t))
    (save-selected-window
      (other-window 1)
      (jcs-walk-windows
       (lambda ()
         (unless target-window
           (when (and
                  (not (eq (selected-window) current-window))
                  (not (jcs-frame-util-p))
                  (jcs-window-is-larger-in-height-p))
             (setq target-window (selected-window)))))))
    (save-window-excursion (undo-tree-visualize))
    (with-selected-window target-window
      (switch-to-buffer undo-tree-visualizer-buffer-name)
      (jcs-recenter-top-bottom 'middle)
      (fill-page-if-unfill))))

(defun jcs--undo-tree-visualizer--do-diff ()
  "Do show/hide diff for `undo-tree'."
  ;; STUDY: weird that they use word toggle, instead of just set it.
  ;;
  ;; Why not?
  ;;   => `undo-tree-visualizer-show-diff'
  ;; or
  ;;   => `undo-tree-visualizer-hide-diff'
  (when jcs-undo-tree-auto-show-diff (undo-tree-visualizer-toggle-diff)))

(defun jcs--undo-or-redo (ud)
  "Do undo or redo base on UD.
If UD is non-nil, do undo.  If UD is nil, do redo."
  (jcs--lsp-ui-doc--hide-frame)
  (if (not jcs-use-undo-tree-key)
      (call-interactively #'undo)  ; In Emacs, undo/redo is the same thing.
    ;; NOTE: If we do jumped to the `undo-tree-visualizer-buffer-name'
    ;; buffer, then we use `undo-tree-visualize-redo' instead of
    ;; `undo-tree-redo'. Because directly called `undo-tree-visualize-redo'
    ;; key is way faster than `undo-tree-redo' key.
    (jcs-safe-jump-shown-to-buffer
     undo-tree-visualizer-buffer-name :type 'strict
     :success
     (lambda ()
       (if ud (undo-tree-visualize-undo) (undo-tree-visualize-redo))
       (jcs--undo-tree-visualizer--do-diff))
     :error
     (lambda ()
       (save-selected-window (if ud (undo-tree-undo) (undo-tree-redo)))
       (jcs-undo-tree-visualize)
       (jcs--undo-tree-visualizer--do-diff)))))

(provide 'jcs-undo)
;;; jcs-undo.el ends here
