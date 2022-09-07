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
  (if (and lsp-mode lsp-managed-mode) (jcs--lsp--stuff-on-enabled)
    (jcs--lsp--stuff-on-disabled)))

(jcs-add-hook 'lsp-mode-hook
  (if lsp-mode (jcs--lsp--stuff-on-enabled) (jcs--lsp--stuff-on-disabled)))

;;
;; (@* "Registry" )
;;

(jcs-advice-add 'lsp--message :around (msgu-inhibit-log (apply arg0 args)))

(provide 'jcs-lsp)
;;; jcs-lsp.el ends here
