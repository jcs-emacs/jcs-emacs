;;; jcs-lsp.el --- LSP function related  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; (@* "lsp-mode" )
;;

(defun jcs--lsp--stuff-on-enabled ()
  "Do stuff when lsp is enabled."
  (jcs-re-enable-mode 'company-fuzzy-mode)
  ;; enable semantic meaning
  (setq-local company-fuzzy-passthrough-backends '(company-capf)))

(defun jcs--lsp--stuff-on-disabled ()
  "Do stuff when lsp is disabled."
  (setq-local company-fuzzy-passthrough-backends nil))

(jcs-add-hook 'lsp-managed-mode-hook
  (jcs-lsp--run-lighter-timer)
  (if (and lsp-mode lsp-managed-mode) (jcs--lsp--stuff-on-enabled)
    (jcs--lsp--stuff-on-disabled)))

(jcs-add-hook 'lsp-mode-hook
  (if lsp-mode (jcs--lsp--stuff-on-enabled) (jcs--lsp--stuff-on-disabled)))

;;; Lighter

(diminish 'lsp-mode '(:eval (jcs-lsp--lighter)))

;; NOTE: Borrow from lsp-mode directly
(defun jcs--lsp--workspace-print (workspace)
  "Visual representation WORKSPACE."
  (let* ((proc (lsp--workspace-cmd-proc workspace))
         (status (lsp--workspace-status workspace))
         (server-id (-> workspace lsp--workspace-client lsp--client-server-id symbol-name)))
    (if (eq 'initialized status)
        (format "%s" server-id)
      (format "%s/%s" server-id status))))

(defvar-local jcs-lsp--shorten-lighter nil
  "If true, show full lighter.")

(defvar-local jcs-lsp--lighter-timer nil
  "Timer to shotern the lighter display.")

(defun jcs-lsp--lighter ()
  "Lighter for `lsp-mode'."
  (concat
   " LSP"
   (when lsp--buffer-workspaces
     (format "<%s>"
             (if jcs-lsp--shorten-lighter "/"
               (mapconcat #'jcs--lsp--workspace-print lsp--buffer-workspaces ", "))))))

(defun jcs-lsp--run-lighter-timer ()
  "Start lighter timer."
  (when jcs-lsp-lighter-delay
    (jcs-safe-kill-timer jcs-lsp--lighter-timer)
    (setq jcs-lsp--shorten-lighter nil
          jcs-lsp--lighter-timer
          (run-with-timer jcs-lsp-lighter-delay nil
                          (lambda (buffer)
                            (jcs-with-current-buffer buffer
                              (setq jcs-lsp--shorten-lighter t)
                              (force-mode-line-update)))
                          (current-buffer)))))

;;
;; (@* "lsp-ui" )
;;

(defun jcs--lsp-ui-mode--enabled-p ()
  "Return non-nil if lsp-ui is enabled."
  (and (boundp 'lsp-ui-mode) lsp-ui-mode))

(defun jcs--lsp-ui-doc-stop-timer ()
  "Safe way to stop lsp UI document."
  (when (boundp 'lsp-ui-doc--timer)
    (jcs-safe-kill-timer lsp-ui-doc--timer)))

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

(defun jcs-lsp--focus-in ()
  "When window is focus."
  (jcs--lsp-ui-doc-show-safely))

(defun jcs-lsp--focus-out ()
  "When window is not focus."
  (jcs--lsp-ui-doc--inhibit-frame))

(add-function
 :after after-focus-change-function
 (lambda () (if (frame-focus-state) (jcs-lsp--focus-in) (jcs-lsp--focus-out))))

(jcs-advice-add 'other-window :before (jcs--lsp-ui-doc--inhibit-frame))

(jcs-add-hook 'window-size-change-functions (jcs--lsp-ui-doc-resize))

(jcs-add-hook 'window-state-change-hook (jcs-lsp--run-lighter-timer))

(provide 'jcs-lsp)
;;; jcs-lsp.el ends here
