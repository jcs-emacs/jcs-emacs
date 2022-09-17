;;; jcs-clojure-mode.el --- Clojure mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'clojure-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-clojure-template "clojure" "default.txt"
  "Header for Clojure header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'clojure-mode-hook
  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]clj")
                              'jcs-insert-clojure-template))

(provide 'jcs-clojure-mode)
;;; jcs-clojure-mode.el ends here
