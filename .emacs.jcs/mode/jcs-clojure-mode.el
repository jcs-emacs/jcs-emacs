;;; jcs-clojure-mode.el --- Clojure mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'clojure-mode)


(defun jcs-clojure-mode-hook ()
  "Clojure mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]clj")
                              'jcs-insert-clojure-template)

  )
(add-hook 'clojure-mode-hook 'jcs-clojure-mode-hook)


(provide 'jcs-clojure-mode)
;;; jcs-clojure-mode.el ends here
