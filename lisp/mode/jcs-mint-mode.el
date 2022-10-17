;;; jcs-mint-mode.el --- Mint mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'mint-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-mint-template "mint" "default.txt"
  "Mint file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'mint-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]mint")
                              'jcs-insert-mint-template))

(provide 'jcs-mint-mode)
;;; jcs-mint-mode.el ends here
