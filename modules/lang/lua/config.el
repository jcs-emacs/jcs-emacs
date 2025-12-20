;;; lang/lua/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-lua-template "lua" "default.txt"
  "Lua file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'lua-mode-hook
  (jcs-elec-pair-add '((?\[ . ?\])))

  (company-fuzzy-backend-add-before 'company-lua 'company-dabbrev)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]lua" "[.]luac")
                              'jcs-insert-lua-template))

;;
;; (@* "Extensions" )
;;

(use-package flymake-lua
  :hook (lua-mode . flymake-lua-load))
