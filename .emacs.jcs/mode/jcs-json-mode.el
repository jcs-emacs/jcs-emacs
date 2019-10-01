;;; jcs-js-mode.el --- JSON mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'json-reformat)
(require 'json-snatcher)
(require 'json-mode)


(defun jcs-json-format()
  "Format for json file."
  ;; Empty, cause json should only take data. Even comments will
  ;; be treat as a data too...
  ;;
  ;; TODO: Might add it later?
  )


(defun jcs-json-mode-hook ()
  "JSON mode hook."

  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p t)

  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2)

  ;; File Header
  (jcs-insert-header-if-valid '("[.]json")
                              'jcs-json-format)

  ;; Normal
  (define-key json-mode-map (kbd "DEL") #'jcs-electric-backspace)
  (define-key json-mode-map (kbd "{") #'jcs-vs-front-curly-bracket-key)
  (define-key json-mode-map (kbd ";") #'jcs-vs-semicolon-key)

  ;; comment block
  (define-key json-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key json-mode-map (kbd "*") #'jcs-c-comment-pair)
  )
(add-hook 'json-mode-hook 'jcs-json-mode-hook)


(provide 'jcs-json-mode)
;;; jcs-json-mode.el ends here
