;;; jcs-json-mode.el --- JSON mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'json-reformat)
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

(defun jcs-json-mode-hook ()
  "JSON mode hook."
  (js2-minor-mode -1)
  (remove-hook 'after-change-functions 'js2-minor-mode-edit t)

  (setq js2-bounce-indent-p t)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]json")
                              'jcs--json-format)

  ;; Normal
  (define-key json-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key json-mode-map (kbd "{") #'jcs-vs-opening-curly-bracket-key)
  (define-key json-mode-map (kbd ";") #'jcs-vs-semicolon-key))

(add-hook 'json-mode-hook 'jcs-json-mode-hook)

(provide 'jcs-json-mode)
;;; jcs-json-mode.el ends here
