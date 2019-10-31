;;; jcs-processing-mode.el --- Processing mode. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'processing-mode)


;;(setq processing-location "/path/to/processing-java")
;;(setq processing-application-dir "/path/to/processing-application-dir")
;;(setq processing-sketchbook-dir "/path/to/processing-sketchbook-dir")

;; NOTE: Usually when you run a processing sketch,
;; the build files are generated in a sub-directory called
;; output in the current sketch directory. It is also possible
;; to set the processing-output-dir to another directory:
;;(setq processing-output-dir "/tmp")


(defun jcs-processing-mode-hook ()
  "Hook for processing mode."

  ;; Treat underscore as word.
  (modify-syntax-entry ?_ "w")

  ;; File Header
  (jcs-insert-header-if-valid '("[.]pde")
                              'jcs-insert-processing-template)

  ;; Normal
  (define-key processing-mode-map (kbd "DEL") #'jcs-electric-backspace)

  ;; Comment
  (define-key processing-mode-map (kbd "RET") #'jcs-smart-context-line-break)
  (define-key processing-mode-map (kbd "*") #'jcs-c-comment-pair))

(add-hook 'processing-mode-hook 'jcs-processing-mode-hook)


(provide 'jcs-processing-mode)
;;; jcs-processing-mode.el ends here
