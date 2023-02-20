;;; lang/mint/config.el  -*- lexical-binding: t; -*-

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
