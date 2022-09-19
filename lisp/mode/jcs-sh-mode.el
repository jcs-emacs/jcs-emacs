;;; jcs-sh-mode.el --- Shell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sh-script)

(defvar-local jcs-sh--buffer-eol nil
  "Record of buffer's line endings type.")

;; Ask the saved line endings SOURCE for this shell script.
(file-header-defsrc jcs-ask-line-endings-for-this-sh-script
    (format "Line Endings Type for file `%s`: " (jcs-buffer-name-or-buffer-file-name))
  (list (format "=> file: (%s)" (show-eol-get-current-system))
        (format "=> system: (%s)" jcs-system-type)
        "Windows (dos)" "macOS (mac)" "Linux (unix)")
  (setq jcs-sh--buffer-eol
        (cond ((string-match-p "file:" source) (show-eol-get-current-system))
              ((string-match-p "system:" source) jcs-system-type)
              (t (pcase source
                   ("Windows (dos)" 'dos)
                   ("macOS (mac)" 'mac)
                   ("Linux (unix)" 'unix)))))
  (set-buffer-file-coding-system jcs-sh--buffer-eol))

(defun jcs-sh--before-save ()
  "Run execution before saving."
  (if jcs-sh--buffer-eol (set-buffer-file-coding-system jcs-sh--buffer-eol)
    (call-interactively #'jcs-ask-line-endings-for-this-sh-script)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-sh-template "sh" "default.txt"
  "Header for Shell header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'sh-mode-hook
  (company-fuzzy-backend-add 'company-shell)

  (modify-syntax-entry ?_ "w")

  (add-hook 'before-save-hook #'jcs-sh--before-save nil t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sh"
                                "[.]linux"
                                "[.]macosx")
                              'jcs-insert-sh-template))

(provide 'jcs-sh-mode)
;;; jcs-sh-mode.el ends here
