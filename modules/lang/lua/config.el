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

  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (company-fuzzy-backend-add 'company-lua)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]lua" "[.]luac")
                              'jcs-insert-lua-template))
