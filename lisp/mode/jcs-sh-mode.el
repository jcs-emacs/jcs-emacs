;;; jcs-sh-mode.el --- Shell mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sh-script)

(defvar-local jcs-sh--buffer-eol nil
  "Record of buffer's line endings type.")

(defun jcs-ask-line-endings-for-this-sh-script (type)
  "Ask the saved line endings TYPE for this shell script."
  (interactive
   (list
    (completing-read
     (format "Line Endings Type for file `%s`: " (jcs-buffer-name-or-buffer-file-name))
     (progn
       (require 'show-eol)
       (list (format "=> file: (%s)" (show-eol--get-current-system))
             (format "=> system: (%s)" jcs-system-type)
             "Windows (dos)" "macOS (mac)" "Linux (unix)")))))
  (setq jcs-sh--buffer-eol
        (cond ((string-match-p "file:" type) (show-eol--get-current-system))
              ((string-match-p "system:" type) jcs-system-type)
              (t (pcase type
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

(defun jcs-insert-sh-template ()
  "Header for Shell header file."
  (jcs--file-header--insert "sh" "default.txt"))

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
