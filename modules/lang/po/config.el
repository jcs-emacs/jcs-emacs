;;; lang/po/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-po-template "po" "default.txt"
  "Template for PO.")

;;
;; (@* "Hook" )
;;

(use-package po-mode
  :init
  (setq po-mode-map (make-sparse-keymap)))

(jcs-advice-add 'po-mode :before
  ;; File Header
  (setq po-default-file-header
        (if (jcs-current-file-empty-p)
            (with-temp-buffer
              (jcs-insert-header-if-empty #'jcs-insert-po-template)
              (buffer-string))
          "")))

(jcs-add-hook 'po-mode-hook
  (run-hooks 'prog-mode-hook)
  (setq buffer-read-only nil))
