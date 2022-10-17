;;; jcs-zig-mode.el --- Zig mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'zig-mode)

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

(provide 'jcs-zig-mode)
;;; jcs-zig-mode.el ends here
