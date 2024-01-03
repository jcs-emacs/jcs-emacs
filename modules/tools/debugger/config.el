;;; tools/debugger/config.el  -*- lexical-binding: t; -*-

;; gdb
(setq gdb-show-main t
      gdb-many-windows t)

(jcs-add-hook '(dap-mode-hook)
  (let* ((mode-name (jcs-2str major-mode))
         (guess-req (s-replace "-mode" "" mode-name)))
    (require (intern (format "dap-%s" guess-req)) nil t)))
