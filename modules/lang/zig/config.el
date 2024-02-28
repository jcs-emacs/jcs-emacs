;;; lang/zig/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Config" )
;;

(use-package zig-mode
  :init
  (setq zig-format-on-save nil))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-zig-template "zig" "default.txt"
  "Header for Zig header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'zig-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]zig")
                              'jcs-insert-zig-template)

  (jcs-key-local
    `(((kbd "<up>")   . vs-edit-previous-line)
      ((kbd "<down>") . vs-edit-next-line))))
