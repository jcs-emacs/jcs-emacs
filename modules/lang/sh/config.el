;;; lang/sh/config.el  -*- lexical-binding: t; -*-

(require 'sh-script)
(require 'fish-mode)

;;
;; (@* "Line Endings" )
;;

(defvar-local jcs-sh--buffer-eol nil
  "Record of buffer's line endings type.")

;; Ask the saved line endings SOURCE for this shell script.
(file-header-defsrc jcs-ask-line-endings-for-this-sh-script
    (format "Line Endings Type for file `%s`: " (jcs-buffer-name-or-buffer-file-name))
  (list (format "=> file: (%s)" (show-eol-get-current-system))
        (format "=> system: (%s)" elenv-system-type)
        "Windows (dos)" "macOS (mac)" "Linux (unix)")
  (setq jcs-sh--buffer-eol
        (cond ((string-match-p "file:" source) (show-eol-get-current-system))
              ((string-match-p "system:" source) elenv-system-type)
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

(jcs-add-hook '(sh-mode-hook fish-mode-hook)
  (modify-syntax-entry ?_ "w")

  (company-fuzzy-backend-add 'company-shell)

  (add-hook 'before-save-hook #'jcs-sh--before-save nil t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]sh"
                                "[.]fish")
                              'jcs-insert-sh-template))
