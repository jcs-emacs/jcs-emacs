;;; jcs-swift-mode.el --- Swift mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'swift-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-swift-template "swift" "default.txt"
  "Header for Swift header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'swift-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]swift")
                              'jcs-insert-swift-template)

  (jcs-key-local
    `(((kbd "M-k") . jcs-maybe-kill-this-buffer))))

(provide 'jcs-swift-mode)
;;; jcs-swift-mode.el ends here
