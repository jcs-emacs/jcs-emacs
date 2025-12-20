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
    (format "Line Endings for file `%s`: " (jcs-buffer-name-or-buffer-file-name))
  (list (cons (format "=> file: (%s)" (show-eol-get-current-system))
              (show-eol-get-eol-mark-by-system))
        (cons (format "=> system: (%s)" elenv-system-type)
              (cl-case elenv-system-type
                (`unix "Linux LF")
                (`mac  "macOS CR")
                (`dos  "Windows CRLF")
                (t     "Unkown")))
        '("Linux (unix)"  . "Linux LF")
        '("macOS (mac)"   . "macOS CR")
        '("Windows (dos)" . "Windows CRLF"))
  (setq jcs-sh--buffer-eol
        (pcase index
          (0 (show-eol-get-current-system))
          (1 elenv-system-type)
          (_ (pcase source
               ("Linux (unix)"  'unix)
               ("macOS (mac)"   'mac)
               ("Windows (dos)" 'dos)))))
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

(jcs-add-hook '( sh-mode-hook)
  (company-fuzzy-backend-add-before 'company-shell 'company-dabbrev)

  (add-hook 'before-save-hook #'jcs-sh--before-save nil t))

(jcs-add-hook '( sh-mode-hook
                 elvish-mode-hook fish-mode-hook nushell-mode-hook)
  ;; File Header
  (jcs-insert-header-if-valid '("[.]sh"
                                "[.]elv"
                                "[.]fish"
                                "[.]nu")
                              'jcs-insert-sh-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-shell
  :hook (sh-set-shell . flymake-shell-load))
