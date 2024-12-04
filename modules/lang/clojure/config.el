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
  (setcdr clojure-mode-map nil)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]clj")
                              'jcs-insert-clojure-template))

(jcs-add-hook 'cider-repl-mode-hook
  (jcs-key-local
    `(((kbd "M-<up>")   . cider-repl-previous-input)
      ((kbd "M-<down>") . cider-repl-next-input)
      ((kbd "M-K")      . cider-repl-clear-buffer))))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-clojure
  :hook (flycheck-mode . flycheck-clojure-setup))

(use-package flycheck-clj-kondo
  :hook (flycheck-mode . (lambda (&rest _) (require 'flycheck-clj-kondo))))
