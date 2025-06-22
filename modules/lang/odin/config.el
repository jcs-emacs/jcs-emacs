;;; lang/odin/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-odin-template "odin" "default.txt"
  "Odin file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'odin-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]odin")
                              'jcs-insert-odin-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-odin :hook (flycheck-mode . flycheck-odin-setup))
