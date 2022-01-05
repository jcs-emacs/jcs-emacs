;;; jcs-json-mode.el --- JSON mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'json-snatcher)
(require 'json-mode)

(defun jcs--json-format()
  "Format for json file."
  ;; Empty, cause json should only take data. Even comments will
  ;; be treat as a data too...
  ;;
  ;; TODO: Might add it later?
  )

;;
;; (@* "Hook" )
;;

(jcs-add-hook 'json-mode-hook
  (js2-minor-mode -1)
  (remove-hook 'after-change-functions 'js2-minor-mode-edit t)

  (setq js2-bounce-indent-p t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]json")
                              'jcs--json-format)

  ;; Normal
  (jcs-bind-key (kbd "DEL") #'jcs-electric-backspace)
  (jcs-bind-key (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (jcs-bind-key (kbd ";") #'jcs-vs-semicolon-key))

(provide 'jcs-json-mode)
;;; jcs-json-mode.el ends here
