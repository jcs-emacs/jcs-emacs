;;; lang/nim/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-nim-template "nim" "default.txt"
  "Nim file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'nim-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]nim")
                              'jcs-insert-nim-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-nim
  :hook (flycheck-mode . (lambda (&rest _) (require 'flycheck-nim))))

(use-package flycheck-nimsuggest
  :hook (nimsuggest-mode . flycheck-nimsuggest-setup))
