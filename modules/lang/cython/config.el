;;; lang/cython/config.el  -*- lexical-binding: t; -*-

(require 'python-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-cython-template "cython" "default.txt"
  "Cython template.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'cython-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]pxd")
                              'jcs-insert-cython-template))

;;
;; (@* "Extensions" )
;;

(use-package flycheck-cython
  :hook (flycheck-mode . (lambda (&rest _) (require 'flycheck-cython))))
