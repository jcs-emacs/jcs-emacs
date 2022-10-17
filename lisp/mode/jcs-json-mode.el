;;; jcs-json-mode.el --- JSON mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'json-snatcher)
(require 'json-mode)

;;
;; (@* "Templates" )
;;

(file-header-defins jcs-insert-json-template "json" "default.txt"
  "Header for JSON header file.")

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'json-mode-hook
  (setq js2-bounce-indent-p t)
  (remove-hook 'after-change-functions 'js2-minor-mode-edit t)

  (js2-minor-mode -1)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]json")
                              'jcs-insert-json-template))

(provide 'jcs-json-mode)
;;; jcs-json-mode.el ends here
