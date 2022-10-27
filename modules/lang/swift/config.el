;;; lang/swift/config.el  -*- lexical-binding: t; -*-

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

  (company-fuzzy-backend-add 'company-sourcekit)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]swift")
                              'jcs-insert-swift-template)

  (jcs-key-local
    `(((kbd "M-k") . jcs-maybe-kill-this-buffer))))

;;
;; (@* "Extensions" )
;;

(leaf flycheck-swift
  :hook (flycheck-mode-hook . flycheck-swift-setup))
