;;; jcs-lsp.el --- LSP function related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "lsp-mode" )
;;

;; Enable or Disable for LSP.

(defun jcs--lsp--stuff-on-enabled ()
  "Do stuff when lsp is enabled."
  (jcs-re-enable-mode 'company-fuzzy-mode)
  ;; enable semantic meaning
  (setq-local company-fuzzy-passthrough-backends '(company-capf)))

(defun jcs--lsp--stuff-on-disabled ()
  "Do stuff when lsp is disabled."
  (setq-local company-fuzzy-passthrough-backends nil))

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
  "Return non-nil if lsp-ui is enabled."
  (and (boundp 'lsp-ui-mode) lsp-ui-mode))

(defun jcs--lsp-ui-doc-stop-timer ()
  "Safe way to stop lsp UI document."
  (when (and (boundp 'lsp-ui-doc--timer) (timerp lsp-ui-doc--timer))
    (cancel-timer lsp-ui-doc--timer)))

(defun jcs--lsp-ui-doc--inhibit-frame ()
  "Stop ui-doc frame from being pop up."
  (jcs--lsp-ui-doc-stop-timer)
  (jcs--lsp-ui-doc--hide-frame))

(defun jcs--lsp-ui-doc-show-safely ()
  "Safe way to show lsp UI document."
  (if (jcs--lsp-ui-mode--enabled-p)
      (let (lsp-ui-doc--bounds) (lsp-ui-doc--make-request))
    (jcs--lsp-ui-doc--inhibit-frame)))

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
  (jcs--lsp-ui-doc--inhibit-frame))
(add-hook 'focus-out-hook 'jcs-lsp--focus-out-hook)

(defun jcs-lsp--other-window--advice-before (&rest _args)
  "Advice execute before `other-window' command."
  (jcs--lsp-ui-doc--inhibit-frame))
(advice-add 'other-window :before #'jcs-lsp--other-window--advice-before)

(defun jcs-lsp--window-size-change-functions (&rest _)
  "When window changed size."
  (jcs--lsp-ui-doc-resize))
(add-hook 'window-size-change-functions 'jcs-lsp--window-size-change-functions)

(provide 'jcs-lsp)
;;; jcs-lsp.el ends here
