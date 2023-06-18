;;; lang/clojure/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-clojure-template "clojure" "default.txt"
  "Header for Clojure header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'clojure-mode-hook
  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  ;; File Header
  (jcs-insert-header-if-valid '("[.]clj")
                              'jcs-insert-clojure-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-clojure
  :hook (flycheck-mode . flycheck-clojure-setup))
