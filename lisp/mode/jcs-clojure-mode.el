;;; jcs-clojure-mode.el --- Clojure mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'clojure-mode)

;;
;; (@* "Templates" )
;;

(defun jcs-insert-clojure-template ()
  "Header for Clojure header file."
  (jcs--file-header--insert "clojure" "default.txt"))

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
