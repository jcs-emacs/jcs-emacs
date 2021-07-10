;;; jcs-lsp.el --- LSP function related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'lsp-dart)
(require 'lsp-docker)
(require 'lsp-grammarly)
(require 'lsp-haskell)
(require 'lsp-java)
(require 'lsp-latex)
(require 'lsp-ltex)
(require 'lsp-mssql)
(require 'lsp-pascal)
(require 'lsp-pyright)
(require 'lsp-sourcekit)

;;
;; (@* "lsp" )
;;

(defun jcs--lsp-lv-buffer-alive-p ()
  "Check if ` *LV*' buffer alive."
  (get-buffer jcs--lsp-lv-buffer-name))

(defun jcs--lsp--execute-command--advice-around (fnc &rest args)
  "Advice execute around `lsp--execute-command'."
  (let ((jcs--lsp--executing-command t))
    (apply fnc args)))

(advice-add 'lsp--execute-command :around #'jcs--lsp--execute-command--advice-around)

;; Enable or Disable for LSP.

(defun jcs--lsp--stuff-on-enabled ()
  "Do stuff when lsp is enabled."
  (jcs-re-enable-mode 'company-fuzzy-mode)
  (lsp-origami-mode 1))

(defun jcs--lsp--stuff-on-disabled ()
  "Do stuff when lsp is disabled."
  (lsp-origami-mode -1))

(defun jcs--lsp-managed-mode-hook ()
  "LSP managed mode hook."
  (if (and lsp-mode lsp-managed-mode) (jcs--lsp--stuff-on-enabled) (jcs--lsp--stuff-on-disabled)))

(defun jcs--lsp-mode-hook ()
  "LSP mode hook."
  (if lsp-mode (jcs--lsp--stuff-on-enabled) (jcs--lsp--stuff-on-disabled)))

(add-hook 'lsp-managed-mode-hook 'jcs--lsp-managed-mode-hook)
(add-hook 'lsp-mode-hook 'jcs--lsp-mode-hook)

;;
;; (@* "lsp-ui" )
;;

(defun jcs--lsp-ui-mode--enabled-p ()
  "Check if `lsp-ui-mode' enabled."
  (and (boundp 'lsp-ui-mode) lsp-ui-mode))

(defun jcs--lsp-ui-doc-stop-timer ()
  "Safe way to stop lsp UI document."
  (when (and (boundp 'lsp-ui-doc--timer) (timerp lsp-ui-doc--timer))
    (cancel-timer lsp-ui-doc--timer)))

(defun jcs--lsp-ui-doc-show-safely ()
  "Safe way to show lsp UI document."
  (if (jcs--lsp-ui-mode--enabled-p)
      (let (lsp-ui-doc--bounds) (lsp-ui-doc--make-request))
    (jcs--lsp-ui-doc-stop-timer)
    (jcs--lsp-ui-doc--hide-frame)))

(defun jcs--lsp-ui-doc-resize ()
  "Reset the size of the UI document frame."
  (setq lsp-ui-doc-max-width (round (* (frame-width) 0.4))
        lsp-ui-doc-max-height (window-height)))

;;
;; (@* "Registry" )
;;

(defun jcs-lsp--focus-in-hook ()
  "When window is focus."
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'focus-in-hook 'jcs-lsp--focus-in-hook)

(defun jcs-lsp--focus-out-hook ()
  "When window is not focus."
  (jcs--lsp-ui-doc-stop-timer)
  (jcs--lsp-ui-doc--hide-frame))
(add-hook 'focus-out-hook 'jcs-lsp--focus-out-hook)

(defun jcs-lsp--other-window--advice-before (&rest _args)
  "Advice execute before `other-window' command."
  (unless jcs-walking-through-windows-p
    (jcs--lsp-ui-doc-stop-timer)))
(advice-add 'other-window :before #'jcs-lsp--other-window--advice-before)

(defun jcs-lsp--other-window--advice-after (&rest _args)
  "Advice execute after `other-window' command."
  (unless jcs-walking-through-windows-p
    (jcs--lsp-signature-maybe-stop)
    (jcs--lsp-ui-doc-show-safely)))
(advice-add 'other-window :after #'jcs-lsp--other-window--advice-after)

(defvar jcs--lsp-lv-was-alive nil
  "Record ` *LV*' buffer was alive.")

(defvar jcs--lsp-lv-recording nil
  "Check if we are recording ")

(defun jcs-lsp--window-size-change-functions (&rest _)
  "When window changed size."
  (jcs--lsp-ui-doc-resize)
  (when (and (boundp 'lsp-mode) lsp-mode)
    (if (jcs--lsp-lv-buffer-alive-p)
        (setq jcs--lsp-lv-was-alive t)
      (if jcs--lsp-lv-was-alive
          (progn
            (when (jcs--lsp-current-last-signature-buffer)
              (let ((pt (point))) (jcs-window-restore-once) (goto-char pt)))
            (setq jcs--lsp-lv-was-alive nil))
        (let ((jcs--lsp-lv-recording t)) (jcs-window-record-once)))))
  (jcs--lsp-ui-doc-show-safely))
(add-hook 'window-size-change-functions 'jcs-lsp--window-size-change-functions)

(provide 'jcs-lsp)
;;; jcs-lsp.el ends here
