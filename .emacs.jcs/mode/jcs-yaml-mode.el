;;; jcs-yaml-mode.el --- YAML mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'yaml-mode)

(require 'jcs-python-func)
(require 'jcs-yaml-func)


(defun jcs-yaml-mode-hook ()
  "YAML mode hook."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]yaml"
                                "[.]yml")
                              'jcs-insert-yaml-template)

  ;; Normal
  (define-key yaml-mode-map (kbd "<backspace>") #'jcs-yaml-electric-backspace)

  (define-key yaml-mode-map (kbd "<up>") #'jcs-py-indent-up)
  (define-key yaml-mode-map (kbd "<down>") #'jcs-py-indent-down)
  )
(add-hook 'yaml-mode-hook 'jcs-yaml-mode-hook)


(provide 'jcs-yaml-mode)
;;; jcs-yaml-mode.el ends here
