;;; lang/mlua/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-mlua-template "mlua" "default.txt"
  "mLua file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'mlua-mode-hook
  ;; File Header
  (jcs-insert-header-if-valid '("[.]mlua")
                              'jcs-insert-mlua-template))
