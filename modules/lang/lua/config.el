;;; lang/lua/config.el  -*- lexical-binding: t; -*-

;;
;; (@* "Electric Pair" )
;;

(defun jcs-lua--electric-pair-inhibit-predicate (c)
  "Electric pair predicate for `lua-mode'."
  (if (jcs-current-char-equal-p "[")
      (electric-pair-default-inhibit c)
    (jcs--electric-pair-inhibit-predicate c)))

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-lua-template "lua" "default.txt"
  "Lua file header format.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'lua-mode-hook
  (setq-local electric-pair-inhibit-predicate 'jcs-lua--electric-pair-inhibit-predicate)
  (jcs-elec-pair-add '((?\[ . ?\])))

  (modify-syntax-entry ?_ "w")  ; Treat underscore as word

  (company-fuzzy-backend-add 'company-lua)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]lua" "[.]luac")
                              'jcs-insert-lua-template))
